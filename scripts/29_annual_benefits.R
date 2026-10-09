# 29_annual_benefits.R
#
# Annual scheduled benefits by trust fund, 2026-2100, and the check against the Trustees
# (methodology 4.3, "Annualizing Benefits", "Retroactive Payments", "Aggregate Scheduled Benefits";
# DECISIONS.md AB-01 to AB-03).
#
#   December monthly benefits B(t), by fund:
#     OASI = retired workers incl. conversions (26) + dual-entitlement excess (28) + OASI dependents
#            and survivors (27);  DI = disabled workers (25) + DI dependents (27)
#   Annual = sum over months i = 0..11 of (12 - i)/12 x B(t-1) + i/12 x B(t)/(1 + COLA_t)
#          = 6.5 B(t-1) + 5.5 B(t) / (1 + COLA_t)                                   (OCACT's formula)
#   x (1 + L_fund x the ratio of new entitlements to the stock relative to 2026): retroactive payments
#   and other amounts outside the December levels, a loading by fund fitted to 2024 actual benefits (IV.A1/IV.A2) with 5.A4's December 2023 and 2024 totals
#   (2025 is distorted by the Social Security Fairness Act's retroactive payments) (AB-02)
#   + lump-sum death payments (23), OASI
# Check: scheduled benefits 2026-2035 (IV.A1, IV.A2) and, after 2035, benefits implied by the cost
# rates (IV.B1 x taxable payroll, VI.G1) less administration (OCACT's formula from 2035) and railroad
# interchange (2035 share of cost) (AB-03).
#
# Input:  data/rw_benefits.rds, di_benefits.rds, aux_benefits.rds, dual_entitlement.rds, lump_sum.rds,
#         params_by_year.rds; Supplement 2026 5.A4; TRTables_TR2026 IV.A1, IV.A2; single-year IV.B1, VI.G1
# Output: data/annual_benefits.rds, outputs/benefits_vs_tr.csv

suppressMessages({library(dplyr); library(tidyr)})
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))
py <- readRDS("data/params_by_year.rds") |> select(year, cola)
cola <- setNames(py$cola / 100, py$year)

rw <- readRDS("data/rw_benefits.rds")$totals |> group_by(year) |> summarise(v = sum(n * mba)) |> mutate(fund = "OASI", part = "retired workers")
di <- readRDS("data/di_benefits.rds")$totals |> group_by(year) |> summarise(v = sum(cp * mba)) |> mutate(fund = "DI", part = "disabled workers")
ax <- readRDS("data/aux_benefits.rds")$totals |> transmute(year, v = monthly, fund, part = "dependents and survivors")
du <- readRDS("data/dual_entitlement.rds")$totals |> group_by(year) |> summarise(v = sum(monthly)) |> mutate(fund = "OASI", part = "dual-entitlement excess")
dec <- bind_rows(rw, di, ax, du)
B <- dec |> group_by(year, fund) |> summarise(B = sum(v), .groups = "drop")

# December 2025 against 5.A4 (by construction, apart from the excess estimate)
a4 <- as.matrix(readxl::read_excel("data-raw/supplement/2026/5a.xlsx", sheet = "5.A4", col_names = FALSE, col_types = "text", .name_repair = "minimal"))
yr <- suppressWarnings(as.integer(a4[, 1])); blk <- which(yr == 2025)    # two blocks: numbers, then total monthly benefits ($ thousands)
pick <- function(y) { i <- which(yr == y); i <- i[length(i)]; c(OASI = num(a4[i, 3]) * 1e3, DI = num(a4[i, 4]) * 1e3) }
cat("December 2025 monthly benefits, $ billions: model OASI", round(B$B[B$year == 2025 & B$fund == "OASI"] / 1e9, 3), "DI", round(B$B[B$year == 2025 & B$fund == "DI"] / 1e9, 3),
    "; 5.A4", round(pick(2025) / 1e9, 3), "\n")
print(dec |> filter(year == 2025) |> mutate(v = round(v / 1e9, 3)))

# ---- Loading fitted to 2024 -------------------------------------------------------------------------------------
tr <- as.matrix(readxl::read_excel("data-raw/tr2026/TRTables_TR2026.xlsx", sheet = "IV_A", col_names = FALSE, col_types = "text", .name_repair = "minimal"))
t2 <- which(grepl("Table IV.A2", tr[, 1]))[1]
grab <- function(rows) { y <- suppressWarnings(as.integer(tr[rows, 1])); sec <- cumsum(grepl("^(Historical|Intermediate|Low|High)", tr[rows, 1]))
  tibble(year = y, sec = sec, cost = num(tr[rows, 8]), ben = num(tr[rows, 9]), adm = num(tr[rows, 10]), rr = num(tr[rows, 11])) |> filter(!is.na(year), sec <= 2) }
ivA <- bind_rows(grab(1:(t2 - 1)) |> mutate(fund = "OASI"), grab(t2:nrow(tr)) |> mutate(fund = "DI")) |> select(-sec)
ann <- function(b0, b1, y) 6.5 * b0 + 5.5 * b1 / (1 + cola[[as.character(y)]])
L <- sapply(c("OASI", "DI"), function(f) ivA$ben[ivA$year == 2024 & ivA$fund == f] * 1e9 / ann(pick(2023)[[f]], pick(2024)[[f]], 2024) - 1)
cat("\nLoading for retroactive and other payments, fitted to 2024 (AB-02):", round(L, 4), "\n")
l25 <- sapply(c("OASI", "DI"), function(f) ann(pick(2024)[[f]], pick(2025)[[f]], 2025) * (1 + L[[f]]) / 1e9)
cat("2025 with the same method, $ billions:", round(l25, 1), "vs actual", ivA$ben[ivA$year == 2025 & ivA$fund == "OASI"], ivA$ben[ivA$year == 2025 & ivA$fund == "DI"],
    "(OASI 2025 includes the Fairness Act's retroactive payments)\n")

# ---- Annual benefits -------------------------------------------------------------------------------------------
ls <- readRDS("data/lump_sum.rds")$lump_sum |> transmute(year, lsdp = amount)
# The loading is mostly retroactive payments to new entitlements, so it moves with new entitlements
# relative to the stock (AB-02): DI from scripts/10, OASI retired workers from scripts/26's diagonal;
# the ratio is set to 1 in 2026, the first projected year (DI: 0.135 falling to 0.098 by 2100)
dent <- readRDS("data/di_projection.rds")$entitlements_by_age |> group_by(year) |> summarise(e = sum(n)) |>
  inner_join(readRDS("data/di_benefits.rds")$totals |> group_by(year) |> summarise(s = sum(cp)) |> mutate(year = year + 1L), by = "year") |> mutate(fund = "DI")
rcel <- readRDS("data/rw_benefits.rds")$cells
oent <- rcel |> filter(col != "conv") |> filter(age == suppressWarnings(as.integer(col))) |> group_by(year) |> summarise(e = sum(n)) |>
  inner_join(rcel |> group_by(year) |> summarise(s = sum(n)) |> mutate(year = year + 1L), by = "year") |> mutate(fund = "OASI")
lrel <- bind_rows(dent, oent) |> mutate(r = e / s) |> group_by(fund) |> mutate(lrel = r / r[year == 2026]) |> ungroup() |> select(year, fund, lrel)
annual <- B |> arrange(fund, year) |> group_by(fund) |> mutate(B0 = lag(B)) |> ungroup() |> filter(year >= 2026) |>
  left_join(lrel, by = c("year", "fund")) |>
  mutate(annual = (6.5 * B0 + 5.5 * B / (1 + cola[as.character(year)])) * (1 + L[fund] * coalesce(lrel, 1))) |>
  left_join(ls, by = "year") |> mutate(annual = annual + ifelse(fund == "OASI", coalesce(lsdp, 0), 0)) |> select(year, fund, annual)

# ---- Trustees: IV.A to 2035, cost rates after ------------------------------------------------------------------
source("R/read_tr.R")
ib1 <- read_tr_single_year("IV.B1", c("oasi_inc", "oasi_cost", "oasi_bal", "di_inc", "di_cost", "di_bal", "oasdi_inc", "oasdi_cost", "oasdi_bal"))
g1 <- read_tr_single_year("VI.G1", c("cpi", "awi", "payroll", "gdp", "ratio", "interest"))
# After 2035 OCACT's administrative costs grow with beneficiaries x the AWI x (1 - productivity growth,
# 1.63% ultimate); railroad interchange is held at its 2035 share of cost (AB-03)
vc4 <- read_tr_single_year("V.C4", c("rw", "spouse", "child", "widow", "mother", "parent", "total"))
vc5t <- read_tr_single_year("V.C5", c("dw", "spouse", "child", "total", "pg", "pa"))
bens <- bind_rows(vc4 |> transmute(year, fund = "OASI", nb = total), vc5t |> transmute(year, fund = "DI", nb = total))
c35 <- ivA |> filter(year == 2035) |> transmute(fund, adm35 = adm, rr_share = coalesce(rr, 0) / cost)
lr <- ib1 |> select(year, OASI = oasi_cost, DI = di_cost) |> pivot_longer(c(OASI, DI), names_to = "fund", values_to = "cost_rate") |>
  inner_join(g1 |> select(year, payroll, awi), by = "year") |> inner_join(bens, by = c("year", "fund")) |> filter(year >= 2035) |>
  left_join(c35, by = "fund") |> arrange(fund, year) |> group_by(fund) |>
  mutate(adm = adm35 * (nb / nb[year == 2035]) * (awi / awi[year == 2035]) * (1 - 0.0163)^(year - 2035)) |> ungroup() |>
  mutate(cost = cost_rate / 100 * payroll, tr_ben = cost - adm - cost * rr_share) |> filter(year > 2035) |> select(year, fund, tr_ben, payroll, cost_rate)
trb <- bind_rows(ivA |> filter(year >= 2026, year <= 2035) |> transmute(year, fund, tr_ben = ben), lr |> select(year, fund, tr_ben))
cmp <- annual |> inner_join(trb, by = c("year", "fund")) |> mutate(model = annual / 1e9, gap_pct = 100 * (model / tr_ben - 1)) |>
  left_join(g1 |> select(year, payroll), by = "year") |> mutate(model_rate = 100 * model / payroll)
cat("\nAnnual scheduled benefits, model vs Trustees ($ billions; after 2035 from cost rates less administration and railroad):\n")
print(as.data.frame(cmp |> filter(year %in% c(2026, 2027, 2028, 2030, 2032, 2035, 2040, 2045, 2050, 2060, 2070, 2080, 2090, 2100)) |>
  transmute(year, fund, model = round(model, 1), tr = round(tr_ben, 1), gap_pct = round(gap_pct, 1)) |>
  pivot_wider(names_from = fund, values_from = c(model, tr, gap_pct))))
tot <- cmp |> group_by(year) |> summarise(model = sum(model), tr = sum(tr_ben), payroll = first(payroll)) |>
  mutate(gap_pct = 100 * (model / tr - 1), model_rate = 100 * model / payroll, tr_rate = 100 * tr / payroll)
cat("\nOASDI benefits as a percent of taxable payroll (benefit rate), model vs Trustees:\n")
print(as.data.frame(tot |> filter(year %in% c(2026, 2030, 2035, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) |>
  transmute(year, model_rate = round(model_rate, 2), tr_rate = round(tr_rate, 2), gap_pct = round(gap_pct, 1))))
s75 <- tot |> filter(year >= 2026, year <= 2100)
cat("Average 2026-2100 benefit rate (unweighted): model", round(mean(s75$model_rate), 2), "vs Trustees", round(mean(s75$tr_rate), 2), "\n")

write.csv(cmp |> select(year, fund, model, tr = tr_ben, gap_pct, model_rate), "outputs/benefits_vs_tr.csv", row.names = FALSE)
saveRDS(list(december = dec, annual = annual, loading = L, comparison = cmp, totals = tot), "data/annual_benefits.rds")
cat("Saved data/annual_benefits.rds, outputs/benefits_vs_tr.csv\n")
