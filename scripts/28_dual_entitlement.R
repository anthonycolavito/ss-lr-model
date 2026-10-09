# 28_dual_entitlement.R
#
# Dually entitled beneficiaries and their excess amounts, December 2025-2100 (methodology 4.3,
# "Dually Entitled Beneficiaries and Benefits"; DECISIONS.md DX-04 to DX-06).
#
# OCACT's regressions, coefficients, targets and phase-in years as printed (TF Ops pp. 61-63):
#   PctExp(k, yr)    = a1 x (PIA_M - PIA_F) / PIA_M + b + c(yr) + d(yr)
#   AvgExcPct(k, yr) = a1 x (PIA_M - PIA_F) / PIA_M + b + c(yr) + d(yr);  excess = AvgExcPct x PIA_M
#   c(yr) = min(yr - 2024, phaseyrs) x (target - ult) / phaseyrs, ult = the regression value in 2100
#   d(yr) = the 2025 gap between the data and the regression, phased out linearly over 20 years
# PIA_M, PIA_F: average PIA of retired workers in current pay (scripts/26). Excess targets are 2100
# dollars, turned into percentages of this model's 2100 PIA_M.
# Exposed populations (not defined in the documentation; DX-04): retired workers of that sex x the
# married (wives, husbands) or widowed (widows, widowers) share of the population at that age; with
# them the regressions reproduce 2025's levels within a few points (printed below).
# Husbands' excess = wives' x 83.21% moving to 84% in 2036; widowers' = widows' (by age band) x 63.50%,
# 58.74%, 52.55% moving to 65%, 60%, 53% in 2036.
# 2025 data: Supplement 5.G2/5.G3 (totals), 5.A15 and 5.A14 (women by age band).
#
# Input:  data/rw_benefits.rds (26), data/population_dec.rds (01); R/dual_excess.R; Supplement 5.A14, 5.A15, 5.G2, 5.G3
# Output: data/dual_entitlement.rds

suppressMessages({library(dplyr); library(tidyr)})
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))
years <- 2025:2100
rwb <- readRDS("data/rw_benefits.rds")
pia <- rwb$totals |> select(year, sex, pia) |> pivot_wider(names_from = sex, values_from = pia) |> mutate(x = (M - F) / M)
rwa <- rwb$cells |> group_by(year, sex, age) |> summarise(n = sum(n), .groups = "drop")
pop <- readRDS("data/population_dec.rds") |> filter(year >= 2025, age >= 62) |> mutate(sex = as.character(sex), age = pmin(as.integer(age), 100L)) |>
  group_by(year, sex, age) |> summarise(mar = sum(pop[marital == "married"]) / sum(pop), wid = sum(pop[marital == "widowed"]) / sum(pop), .groups = "drop")
band <- function(a) case_when(a < 75 ~ "62-74", a < 85 ~ "75-84", TRUE ~ "85+")
expo <- rwa |> inner_join(pop, by = c("year", "sex", "age")) |> mutate(b = band(age)) |>
  group_by(year, sex, b) |> summarise(married = sum(n * mar), widowed = sum(n * wid), .groups = "drop")
E <- bind_rows(expo |> filter(sex == "F") |> group_by(year) |> summarise(e = sum(married)) |> mutate(k = "wife"),
               expo |> filter(sex == "M") |> group_by(year) |> summarise(e = sum(married)) |> mutate(k = "husband"),
               expo |> filter(sex == "F") |> transmute(year, e = widowed, k = paste0("widow ", b)),
               expo |> filter(sex == "M") |> transmute(year, e = widowed, k = paste0("widower ", b)))

# ---- OCACT's coefficients (2026 TR) -----------------------------------------------------------------------------
cnt_coef <- tribble(~k, ~a1, ~b, ~targ, ~ph,
  "wife", 0.78799, 0.0, 0.175, 32, "widow 62-74", 0.11226, 0.49562, 0.515, 75, "widow 75-84", 0.53139, 0.52157, 0.580, 75,
  "widow 85+", 0.00239, 0.79217, 0.600, 70, "widower 62-74", -0.49800, 0.21163, 0.110, 26, "widower 75-84", -0.42543, 0.21897, 0.1200, 36,
  "widower 85+", -0.51509, 0.25465, 0.080, 50, "husband", -0.06927, 0.02890, 0.015, 40)
exc_coef <- tribble(~k, ~a1, ~b, ~targ_usd, ~ph,
  "wife", -0.14404, 0.20632, 5084.37, 32, "widow 62-74", -0.30749, 0.53590, 13907.35, 47,
  "widow 75-84", 0.00634, 0.43856, 12874.44, 75, "widow 85+", 1.24058, -0.00245, 12472.45, 50)

# ---- 2025 data ----------------------------------------------------------------------------------------------------
rd <- function(f, s) { m <- as.matrix(readxl::read_excel(f, sheet = s, col_names = FALSE, col_types = "text", .name_repair = "minimal")); m[is.na(m)] <- ""; m }
g3 <- rd("data-raw/supplement/2026/5g.xlsx", "5.G3"); l3 <- trimws(gsub("\\s+", " ", apply(g3[, 1:4], 1, paste, collapse = " ")))
g <- function(p) num(g3[which(grepl(p, l3))[1], 5:8])
wives <- g("Wives of"); widows <- g("^Widows$"); husb <- g("Husbands of"); widr <- g("^Widowers$")
a15 <- rd("data-raw/supplement/2026/5a.xlsx", "5.A15"); l15 <- trimws(apply(a15[, 1:4], 1, paste, collapse = " "))
avg_row <- which(apply(a15, 1, function(r) any(grepl("Average monthl", r))))[1]
rr <- which(grepl("Wife's benefit|Survivor's ben", l15)); nr <- rr[rr < avg_row][1:2]; ar <- rr[rr > avg_row][1:2]
wb <- tibble(lo = c(65, 70, 75, 80, 85, 90), n_wife = num(a15[nr[1], 6:11]), n_wid = num(a15[nr[2], 6:11]), c_wid = num(a15[ar[2], 6:11]))
a14 <- rd("data-raw/supplement/2026/5a.xlsx", "5.A14"); r25 <- which(trimws(a14[, 1]) == "2025")[1]
n62_wid <- num(a14[r25, 8]) * 1e3 - sum(wb$n_wid)
wb <- bind_rows(tibble(lo = 62, n_wife = NA, n_wid = n62_wid, c_wid = wb$c_wid[1]), wb) |> mutate(b = band(lo))
# widows by band: counts scaled to 5.G3's total; excess by band = combined x 5.G3's excess share, scaled to the total
wid25 <- wb |> group_by(b) |> summarise(n = sum(n_wid), exc = sum(n_wid * c_wid), .groups = "drop") |>
  mutate(n = n * widows[1] / sum(n), avg_exc = exc / sum(exc) * widows[1] * widows[4] / n)
act <- bind_rows(tibble(k = "wife", n = wives[1], exc = wives[4]), tibble(k = "husband", n = husb[1], exc = husb[4]),
                 wid25 |> transmute(k = paste0("widow ", b), n, exc = avg_exc))
# widowers: total only (5.G3); spread over bands like the model's exposure x the regression
x25 <- pia$x[pia$year == 2025]; pm25 <- pia$M[pia$year == 2025]

# ---- Projection -------------------------------------------------------------------------------------------------
proj_pct <- function(coef, act_pct) {
  coef |> crossing(year = years) |> left_join(pia |> select(year, x, M), by = "year") |>
    mutate(reg = a1 * x + b) |> group_by(k) |>
    mutate(ult = reg[year == 2100], c = pmin(year - 2024, ph) * (targ - ult) / ph) |> ungroup() |>
    left_join(act_pct, by = "k") |> group_by(k) |>
    mutate(b1 = first(act[year == 2025]) - (reg[year == 2025] + c[year == 2025]), d = b1 * (1 - pmin(year - 2025, 20) / 20), v = reg + c + d) |> ungroup()
}
# counts: actual 2025 percentages of the exposed populations
e25 <- E |> filter(year == 2025)
act_cnt <- act |> select(k, n) |> left_join(e25 |> select(k, e), by = "k") |> transmute(k, act = n / e)
wr_e <- e25 |> filter(grepl("widower", k))
wr_reg <- cnt_coef |> filter(grepl("widower", k)) |> mutate(reg = a1 * x25 + b) |> left_join(wr_e, by = "k")
act_cnt <- bind_rows(act_cnt, wr_reg |> transmute(k, act = reg * widr[1] / sum(reg * e)))     # one common scaling for widowers
cat("2025: regression (no add-factor) vs data, share of the exposed population:\n")
print(as.data.frame(cnt_coef |> mutate(reg = round(a1 * x25 + b, 3)) |> left_join(act_cnt |> mutate(act = round(act, 3)), by = "k") |>
  left_join(e25 |> transmute(k, exposed_m = round(e / 1e6, 2)), by = "k") |> select(k, exposed_m, reg, act)))
pc <- proj_pct(cnt_coef, act_cnt)
cnt <- pc |> inner_join(E, by = c("year", "k")) |> mutate(n = v * e)

# excess: as a percentage of PIA_M
act_exc <- act |> filter(k %in% exc_coef$k) |> transmute(k, act = exc / pm25)
pe <- proj_pct(exc_coef |> cross_join(pia |> filter(year == 2100) |> transmute(pm2100 = M)) |> mutate(targ = targ_usd / pm2100) |> select(-pm2100, -targ_usd), act_exc) |>
  mutate(avg_exc = v * M)
fac <- function(y, a, z) a + (z - a) * pmin(pmax(y - 2025, 0), 11) / 11
exc <- bind_rows(pe |> select(year, k, avg_exc),
                 pe |> filter(k == "wife") |> transmute(year, k = "husband", avg_exc = avg_exc * fac(year, husb[4] / wives[4], 0.84)),
                 pe |> filter(grepl("widow ", k)) |> transmute(year, b = sub("widow ", "", k), avg_exc) |>
                   left_join(tibble(b = c("62-74", "75-84", "85+"), f0 = c(0.6350, 0.5874, 0.5255), f1 = c(0.65, 0.60, 0.53)), by = "b") |>
                   transmute(year, k = paste0("widower ", b), avg_exc = avg_exc * fac(year, f0, f1)))
du <- cnt |> select(year, k, n) |> inner_join(exc, by = c("year", "k")) |> mutate(monthly = n * avg_exc,
  group = sub(" .*", "", k), sex = ifelse(group %in% c("wife", "widow"), "F", "M"))
chk <- du |> filter(year == 2025) |> group_by(group) |> summarise(n = sum(n), avg = sum(monthly) / sum(n))
cat("\n2025 by construction vs 5.G3: wives", round(wives[1]), round(wives[4], 2), "; widows", round(widows[1]), round(widows[4], 2),
    "; husbands", round(husb[1]), round(husb[4], 2), "; widowers", round(widr[1]), round(widr[4], 2), "\n"); print(chk)
tot <- du |> group_by(year, group) |> summarise(n = sum(n), monthly = sum(monthly), .groups = "drop")
cat("\nDually entitled (millions), average excess ($) and as a share of men's average PIA:\n")
print(as.data.frame(tot |> filter(year %in% c(2025, 2030, 2035, 2050, 2075, 2100)) |> left_join(pia |> select(year, M), by = "year") |>
  transmute(year, group, n_m = round(n / 1e6, 2), avg = round(monthly / n), pct_pia_m = round(monthly / n / M, 3)) |>
  pivot_wider(names_from = group, values_from = c(n_m, avg, pct_pia_m))))
saveRDS(list(by_band = du, totals = tot, exposure = E, pct = pc, pia = pia), "data/dual_entitlement.rds")
cat("Saved data/dual_entitlement.rds\n")
