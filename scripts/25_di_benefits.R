# 25_di_benefits.R
#
# Disabled-worker benefits in current pay, December 2025-2100 (methodology 4.3, "Disabled-Worker
# Benefits"; DECISIONS.md DB-01 to DB-06).
#
# OCACT carries an average-PIA matrix by age in current pay (20-66) and duration (0-8, 9+), started
# from the 100% MBR, and rolls it forward with the COLA, a workers' compensation adjustment and a
# post-entitlement factor; duration 0 is filled from the award PIAs. This model's DI rolls (scripts/10)
# are by sex, age at entitlement e, duration d and attained age, so benefits are carried by cohort
# (sex, e, entitlement year y) instead; the age x duration averages follow by weighting with the rolls.
#
#   Start (December 2025): mba(sex, e, y) = r(sex, e) x alpha(sex, y) x beta(sex, attained age),
#     r = the 2025 award level by age at entitlement (scripts/22), raked so that benefits by
#     entitlement year match Supplement 5.D1 and by attained age match 5.A1.2 (December 2025)
#     (DB-01). Cohort means over attained age are carried forward.
#   Roll forward: mba(t) = mba(t-1) x (1 + COLA_t) x PE(disabled, sex, duration at t-1, t) (scripts/24)
#   New cohorts (y >= 2026): mba(Dec y) = award MBA(y, sex, e) (scripts/22) x k0(sex) x (1 + COLA_y),
#     k0 = the 2025 cohort's December 2025 average / its award level (DB-03): the step from award
#     amounts (offset at award, award-year mix) to December current pay.
#   The workers' compensation offset is not modeled separately: the Supplement's averages are
#     benefits after offset, so its unwinding with duration is in PE and its level at award in k0
#     (DB-04).
#   Conversions at NRA carry their cohort's December benefit to the retired-worker side (DB-05).
#
# Input:  data/di_stock_2025.rds (09), di_projection.rds (10), award_levels.rds (22),
#         post_entitlement.rds (24), params_by_year.rds; Supplement 2026 5.D1, 5.A1.2, 5.D3
# Output: data/di_benefits.rds

suppressMessages({library(dplyr); library(tidyr)})
source("R/read_di.R")
py <- readRDS("data/params_by_year.rds") |> select(year, cola, awi)
cola <- setNames(py$cola / 100, py$year)
pe <- readRDS("data/post_entitlement.rds")$by_year |> filter(type == "disabled") |> select(sex, dur, year, pe)
aw <- readRDS("data/award_levels.rds") |> filter(type == "disabled") |> transmute(year, sex = SEX, ek = age, award = mba)
ek_of <- function(e) pmin(pmax(e, min(aw$ek)), max(aw$ek))
dp <- readRDS("data/di_projection.rds")
st <- dp$state_by_year |> mutate(y = year - d)

# ---- December 2025 start ----------------------------------------------------------------------------------------
s25 <- readRDS("data/di_stock_2025.rds") |>
  transmute(sex = as.character(sex), e = as.integer(entl_age), y = as.integer(ent_year), a = as.integer(attained_age), cp = as.numeric(current_pay)) |>
  filter(cp > 0) |> mutate(ek = ek_of(e)) |>
  left_join(aw |> filter(year == 2025) |> select(sex, ek, r = award), by = c("sex", "ek"))
d1 <- read_supp_5d1() |> transmute(sex = as.character(sex), yk = ent_year, n_pub = number, mba_pub = mba)   # yk 1986 = before 1987
a12 <- read_supp_5a12() |> transmute(sex = as.character(sex), ak = age, n_pub = number, mba_pub = mba)     # ak 19 = under 20
s25 <- s25 |> mutate(yk = pmax(y, 1986L), ak = pmin(pmax(a, 19L), 66L), alpha = 1, beta = 1)
for (it in 1:50) {
  s25 <- s25 |> group_by(sex, yk) |> mutate(alpha = alpha * {
    k <- d1$mba_pub[d1$sex == first(sex) & d1$yk == first(yk)]; k / (sum(cp * r * alpha * beta) / sum(cp)) }) |> ungroup()
  s25 <- s25 |> group_by(sex, ak) |> mutate(beta = beta * {
    k <- a12$mba_pub[a12$sex == first(sex) & a12$ak == first(ak)]; k / (sum(cp * r * alpha * beta) / sum(cp)) }) |> ungroup()
}
s25 <- s25 |> mutate(mba = r * alpha * beta)
fit_y <- s25 |> group_by(sex, yk) |> summarise(model = sum(cp * mba) / sum(cp), n_model = sum(cp), .groups = "drop") |> inner_join(d1, by = c("sex", "yk"))
fit_a <- s25 |> group_by(sex, ak) |> summarise(model = sum(cp * mba) / sum(cp), n_model = sum(cp), .groups = "drop") |> inner_join(a12, by = c("sex", "ak"))
cat("December 2025 start, largest gap in average benefit: by entitlement year",
    round(100 * max(abs(fit_y$model / fit_y$mba_pub - 1)), 2), "%; by attained age", round(100 * max(abs(fit_a$model / fit_a$mba_pub - 1)), 2), "%\n")
cat("Raking factors by attained age (beta), range:", round(range(s25$beta), 3), "; by entitlement year (alpha):", round(range(s25$alpha), 3), "\n")
coh0 <- s25 |> group_by(sex, e, y) |> summarise(mba = sum(cp * mba) / sum(cp), .groups = "drop")

# k0: December 2025 average of the 2025 cohort / its award level
k0 <- s25 |> filter(y == 2025) |> group_by(sex) |> summarise(k0 = sum(cp * mba) / sum(cp * r) / (1 + cola[["2025"]]), .groups = "drop")
cat("k0 (December current pay of the entitlement-year cohort / award level, net of the December COLA):\n"); print(k0)

# ---- Roll forward ----------------------------------------------------------------------------------------------
coh <- coh0 |> mutate(year = 2025L)
out <- list(coh)
for (t in 2026:2100) {
  prev <- out[[length(out)]] |> mutate(dur = pmin(t - 1L - y, 9L)) |>
    left_join(pe |> filter(year == t) |> select(sex, dur, pe), by = c("sex", "dur")) |>
    transmute(sex, e, y, mba = mba * (1 + cola[[as.character(t)]]) * pe, year = t)
  new <- st |> filter(year == t, d == 0) |> distinct(sex, e) |> mutate(y = t, ek = ek_of(e)) |>
    left_join(aw |> filter(year == t) |> select(sex, ek, award), by = c("sex", "ek")) |> left_join(k0, by = "sex") |>
    transmute(sex, e, y, mba = award * k0 * (1 + cola[[as.character(t)]]), year = t)
  out[[length(out) + 1]] <- bind_rows(prev, new)
}
coh <- bind_rows(out)

# ---- Benefits on the rolls ---------------------------------------------------------------------------------------
cells <- st |> select(year, sex, e, d, a, y, cp) |> inner_join(coh, by = c("year", "sex", "e", "y"))
miss <- st |> anti_join(coh, by = c("year", "sex", "e", "y")) |> summarise(cp = sum(cp))
cat("Current-pay workers without a benefit level (should be 0):", round(miss$cp), "\n")
cells25 <- s25 |> transmute(year = 2025L, sex, e, d = 2025L - y, a, y, cp, mba)
cells <- bind_rows(cells25, cells |> select(year, sex, e, d, a, y, cp, mba))
tot <- cells |> group_by(year, sex) |> summarise(mba = sum(cp * mba) / sum(cp), cp = sum(cp), .groups = "drop") |>
  left_join(py |> transmute(year = year + 1L, awi_prev = awi), by = "year")
by_age_dur <- cells |> mutate(dur = pmin(d, 9L)) |> group_by(year, sex, a, dur) |> summarise(mba = sum(cp * mba) / sum(cp), cp = sum(cp), .groups = "drop")

# Conversions at NRA: the cohort's benefit in the conversion year (DB-05)
# a few conversions are entitled at 67 and convert in the same year; scripts/10's saved state (after
# conversion) has no e = 67 cohort, so they take the 66 cohort's benefit of that year
conv <- dp$conversions_detail |> mutate(y = year - d, e = pmin(e, 66L)) |> inner_join(coh, by = c("year", "sex", "e", "y")) |>
  group_by(year, sex, a) |> summarise(mba = sum(conv * mba) / sum(conv), conv = sum(conv), .groups = "drop")

# ---- Checks ---------------------------------------------------------------------------------------------------
d3 <- read_text_sheet(supp26("5d.xlsx"), "5.D3")
cat("\nDecember 2025 total monthly benefits ($ thousands): model", round(sum(tot$cp[tot$year == 2025] * tot$mba[tot$year == 2025]) / 1e3), "; 5.D3 below (number, total $ thousands; men; women)\n")
print(tail(d3[!is.na(d3[[1]]) & grepl("^20[12][0-9]$", d3[[1]]), ], 3))
cat("\nAverage monthly benefit, and relative to the AWI of the year before (x 1000):\n")
print(as.data.frame(tot |> filter(year %in% c(2025, 2026, 2030, 2035, 2040, 2050, 2060, 2075, 2090, 2100)) |>
  transmute(year, sex, workers_k = round(cp / 1e3), mba = round(mba), rel_awi = round(1000 * mba / awi_prev, 1)) |>
  pivot_wider(names_from = sex, values_from = c(workers_k, mba, rel_awi))))

# Trend against the Trustees: DI cost rate (IV.B1) vs 12 x the mean of December t-1 and t total
# monthly benefits / taxable payroll (VI.G1). Before annualizing (Phase 5), retroactive payments,
# administration and the railroad interchange are a level difference, so the comparison is of the
# ratio, set to 1 in 2026.
source("R/read_tr.R")
ib1 <- read_tr_single_year("IV.B1", c("oasi_inc", "oasi_cost", "oasi_bal", "di_inc", "di_cost", "di_bal", "oasdi_inc", "oasdi_cost", "oasdi_bal"))
g1 <- read_tr_single_year("VI.G1", c("cpi", "awi", "payroll", "gdp", "ratio", "interest"))
mon <- tot |> group_by(year) |> summarise(monthly = sum(cp * mba))
dc <- mon |> mutate(annual = 12 * (monthly + lag(monthly)) / 2) |> filter(year >= 2026) |>
  inner_join(g1 |> select(year, payroll), by = "year") |> inner_join(ib1 |> select(year, di_cost), by = "year") |>
  mutate(model_rate = 100 * annual / (payroll * 1e9), ratio = model_rate / di_cost, ratio = ratio / ratio[year == 2026])
cat("\nDI cost rate: model benefits (before annualizing and loadings) vs Trustees, ratio relative to 2026:\n")
print(as.data.frame(dc |> filter(year %in% c(2026, 2028, 2030, 2035, 2040, 2045, 2050, 2060, 2070, 2080, 2090, 2100)) |>
  transmute(year, model_rate = round(model_rate, 3), tr_di_cost = di_cost, ratio = round(ratio, 3))))

saveRDS(list(totals = tot, cost_check = dc, by_age_dur = by_age_dur, cohorts = coh, conversions = conv, k0 = k0,
             fit_2025 = list(by_ent_year = fit_y, by_age = fit_a)), "data/di_benefits.rds")
cat("Saved data/di_benefits.rds\n")
