# 30_trust_fund.R
#
# Trust fund operations and summary measures, 2026-2100 (methodology 4.3.1-4.3.13; DECISIONS.md TF-01
# to TF-08). Benefits come from the model (scripts/29); nothing here is fitted to the Trustees' cost
# or income (P-11).
#
#   CONTRIB(tf, t) = lag x rate(tf, t) x payroll(t) + (1 - lag) x rate(tf, t-1) x payroll(t-1)
#                    payroll = effective taxable payroll (VI.G1); rates 10.6% OASI, 1.8% DI; lag from
#                    the Trustees' 2035 contributions (OCACT's formula), used for every year (TF-01)
#   TAXBEN(tf, t)  = RTB(tf, t) x BEN(tf, t): 2026-2035 the Trustees' ratio of taxation of benefits to
#                    benefits (IV.A1/IV.A2, OTA-based); after 2035 OCACT's formula 4.1.1,
#                    RTB = RTB(2035) x (AWI(2035)/AWI(t))^P + RTB_ult x (1 - (AWI(2035)/AWI(t))^P),
#                    P = 0.99, RTB_ult = 0.0570 OASI, 0.0190 DI (TF-02)
#   ADM            = the Trustees' 2026-2035 (IV.A); after, ADM(t-1) x beneficiaries ratio x AWI ratio x
#                    (1 - 1.63% productivity), beneficiaries from this model (TF-03)
#   RR             = the Trustees' 2026-2035; after, held at 2035's ratio to OASI benefits (TF-04)
#   INT            = yield x average assets; average assets = assets(BOY) + .518 CONTRIB + .625 TAXBEN
#                    - .5 BEN - .583 RR - .5 ADM; yield from VI.G1's compound effective interest
#                    factor (TF-05); assets(1 Jan 2026) = the Trustees' end of 2025
#   Summarized rates (4.3.12-13), actuarial balance (4.3.10), unfunded obligation (4.3.11), annual
#   rates and trust fund ratios (4.3.7-9) as printed; discounting with the same yields; exposures as
#   above (benefits 0.5, TF-06); target fund = next year's cost (2101 extrapolated from 2099-2100).
# Checks: IV.B1 (annual rates), IV.B5 (trust fund ratios, depletion), IV.B6 (summarized), IV.B8 (PVs).
#
# Input:  data/annual_benefits.rds (29), aux_benefits.rds, rw_benefits.rds, di_benefits.rds,
#         dual_entitlement.rds (counts); TRTables IV_A, IV_B; single-year VI.G1, IV.B1
# Output: data/trust_fund.rds, outputs/trust_fund_vs_tr.csv

suppressMessages({library(dplyr); library(tidyr)})
num <- function(x) as.numeric(gsub("[^0-9.eE-]", "", gsub("−", "-", x)))
source("R/read_tr.R")
years <- 2026:2100

# ---- Inputs ------------------------------------------------------------------------------------------------------
g1 <- read_tr_single_year("VI.G1", c("cpi", "awi", "payroll", "gdp", "ratio", "intfac"))
ib1 <- read_tr_single_year("IV.B1", c("oasi_inc", "oasi_cost", "oasi_bal", "di_inc", "di_cost", "di_bal", "oasdi_inc", "oasdi_cost", "oasdi_bal"))
tr <- as.matrix(readxl::read_excel("data-raw/tr2026/TRTables_TR2026.xlsx", sheet = "IV_A", col_names = FALSE, col_types = "text", .name_repair = "minimal"))
t2 <- which(grepl("Table IV.A2", tr[, 1]))[1]
grab <- function(rows) { y <- suppressWarnings(as.integer(tr[rows, 1])); sec <- cumsum(grepl("^(Historical|Intermediate|Low|High)", tr[rows, 1]))
  tibble(year = y, sec = sec, contrib = num(tr[rows, 3]), taxben = num(tr[rows, 5]), interest = num(tr[rows, 6]), cost = num(tr[rows, 8]),
         ben = num(tr[rows, 9]), adm = num(tr[rows, 10]), rr = num(tr[rows, 11]), assets = num(tr[rows, 14])) |> filter(!is.na(year), sec <= 2) |> select(-sec) }
ivA <- bind_rows(grab(1:(t2 - 1)) |> mutate(fund = "OASI"), grab(t2:nrow(tr)) |> mutate(fund = "DI")) |> mutate(across(c(contrib:assets), ~ coalesce(.x, 0)))
rate <- c(OASI = 0.106, DI = 0.018)
pay <- setNames(g1$payroll * 1e9, g1$year)
pay[["2101"]] <- pay[["2100"]] * pay[["2100"]] / pay[["2099"]]
awi <- setNames(g1$awi, g1$year)

# yields: F(t) = prod_{j<t}(1 + y_j) x (1 + y_t)^0.5 (VI.G1 note c), normalized to 1 at 1 January 2026
Fac <- setNames(g1$intfac, g1$year)
y <- c(); prev_half <- 1
for (t in years) {                                # recursion, then smoothed by a centered average (TF-05)
  y[[as.character(t)]] <- (Fac[[as.character(t)]] / ifelse(t == 2026, 1, Fac[[as.character(t - 1)]] * prev_half))^2 - 1
  prev_half <- sqrt(1 + y[[as.character(t)]])
}
y <- unlist(y); ys <- stats::filter(y, rep(1 / 3, 3), sides = 2); ys[is.na(ys)] <- y[is.na(ys)]; y <- setNames(as.numeric(ys), years)

# ---- Benefits and counts from the model --------------------------------------------------------------------------
ab <- readRDS("data/annual_benefits.rds")$annual |> transmute(year, fund, ben = annual)
ben <- function(f, t) ab$ben[ab$fund == f & ab$year == t]
cnt <- bind_rows(readRDS("data/rw_benefits.rds")$totals |> group_by(year) |> summarise(n = sum(n)) |> mutate(fund = "OASI"),
                 readRDS("data/di_benefits.rds")$totals |> group_by(year) |> summarise(n = sum(cp)) |> mutate(fund = "DI"),
                 readRDS("data/aux_benefits.rds")$totals |> transmute(year, fund, n)) |> group_by(year, fund) |> summarise(n = sum(n), .groups = "drop")

# ---- Lag and short-range items --------------------------------------------------------------------------------
sr <- function(f, t, v) ivA[[v]][ivA$fund == f & ivA$year == t]
c35 <- sr("OASI", 2035, "contrib") + sr("DI", 2035, "contrib")
lag <- (c35 * 1e9 - sum(rate) * pay[["2034"]]) / (sum(rate) * pay[["2035"]] - sum(rate) * pay[["2034"]])
cat("Collection lag fitted to the Trustees' 2035 contributions:", round(lag, 4), "\n")
rtb_ult <- c(OASI = 0.0570, DI = 0.0190); P <- 0.99

run_fund <- function(f) {
  out <- list(); assets <- sr(f, 2025, "assets") * 1e9
  rtb35 <- sr(f, 2035, "taxben") / sr(f, 2035, "ben")
  for (t in years) {
    tc <- as.character(t)
    B <- ben(f, t)
    contrib <- lag * rate[[f]] * pay[[tc]] + (1 - lag) * rate[[f]] * pay[[as.character(t - 1)]]
    rtb <- if (t <= 2035) sr(f, t, "taxben") / sr(f, t, "ben") else { w <- (awi[["2035"]] / awi[[tc]])^P; rtb35 * w + rtb_ult[[f]] * (1 - w) }
    taxben <- rtb * B
    if (t <= 2035) { adm <- sr(f, t, "adm") * 1e9; rr <- sr(f, t, "rr") * 1e9 }
    else {
      n1 <- cnt$n[cnt$fund == f & cnt$year == t]; n0 <- cnt$n[cnt$fund == f & cnt$year == t - 1]
      adm <- out[[length(out)]]$adm * (n1 / n0) * (awi[[tc]] / awi[[as.character(t - 1)]]) * (1 - 0.0163)
      rr <- if (f == "OASI") B * sr("OASI", 2035, "rr") / sr("OASI", 2035, "ben") else 0
    }
    avg <- assets + 0.518 * contrib + 0.625 * taxben - 0.5 * B - 0.583 * rr - 0.5 * adm
    int <- y[[tc]] * avg
    end <- assets + contrib + taxben + int - B - rr - adm
    out[[length(out) + 1]] <- tibble(fund = f, year = t, boy = assets, contrib, taxben, int, ben = B, rr, adm, cost = B + rr + adm, eoy = end, rtb)
    assets <- end
  }
  bind_rows(out)
}
ops <- bind_rows(run_fund("OASI"), run_fund("DI"))
comb <- ops |> group_by(year) |> summarise(across(c(boy, contrib, taxben, int, ben, rr, adm, cost, eoy), sum)) |> mutate(fund = "OASDI")
ops <- bind_rows(ops, comb) |> mutate(payroll = pay[as.character(year)], inc_rate = 100 * (contrib + taxben) / payroll,
                                      cost_rate = 100 * cost / payroll, tfr = 100 * boy / cost)

# ---- Summarized measures -----------------------------------------------------------------------------------------
v <- setNames(cumprod(1 / (1 + y)), years)
next_cost <- ops |> group_by(fund) |> arrange(year) |> mutate(nc = lead(cost), nc = ifelse(is.na(nc), cost * cost / lag(cost), nc)) |> ungroup() |> select(fund, year, nc)
pv <- ops |> left_join(next_cost, by = c("fund", "year")) |>
  mutate(vv = v[as.character(year)], yy = y[as.character(year)],
         pv_tax = (1 + 0.518 * yy) * contrib * vv, pv_taxben = (1 + 0.625 * yy) * taxben * vv,
         pv_cost = ((1 + 0.5 * yy) * ben + (1 + 0.583 * yy) * rr + (1 + 0.5 * yy) * adm) * vv,
         pv_targ = nc * vv, pv_pay = (1 + 0.5 * yy) * payroll * vv)
summ <- function(y2) pv |> filter(year <= y2) |> group_by(fund) |>
  summarise(period = paste0("2026-", y2), assets0 = first(boy), pv_tax = sum(pv_tax), pv_taxben = sum(pv_taxben), pv_cost = sum(pv_cost),
            pv_targ = last(pv_targ), pv_pay = sum(pv_pay), .groups = "drop") |>
  mutate(inc_nonint = 100 * (pv_tax + pv_taxben) / pv_pay, inc_res = 100 * assets0 / pv_pay, inc = inc_nonint + inc_res,
         cost_only = 100 * pv_cost / pv_pay, targ = 100 * pv_targ / pv_pay, cost = cost_only + targ, bal = inc - cost,
         unf_obl = (pv_cost - pv_tax - pv_taxben - assets0) / 1e9)
S <- bind_rows(summ(2050), summ(2075), summ(2100))

# ---- Trustees' values ------------------------------------------------------------------------------------------
ivb <- as.matrix(readxl::read_excel("data-raw/tr2026/TRTables_TR2026.xlsx", sheet = "IV_B", col_names = FALSE, col_types = "text", .name_repair = "minimal")); ivb[is.na(ivb)] <- ""
b6 <- which(grepl("^Table IV.B6", ivb[, 1]))[1]
trS <- list(); fund_cur <- NA; sec <- NA
for (r in (b6 + 1):(b6 + 50)) {
  l1 <- trimws(ivb[r, 1])
  if (l1 %in% c("OASI:", "DI:", "OASDI:")) fund_cur <- sub(":", "", l1)
  if (grepl("^(Intermediate|Low-cost|High-cost)", l1)) sec <- sub(":.*", "", l1)
  p <- trimws(ivb[r, 2])
  if (grepl("^2026-", p) && isTRUE(grepl("^Intermediate", sec))) trS[[length(trS) + 1]] <- tibble(fund = fund_cur, period = sub("-(50|75)$", "-20\\1", p),
     tr_inc_nonint = num(ivb[r, 4]), tr_inc_res = num(ivb[r, 5]), tr_inc = num(ivb[r, 6]), tr_cost_only = num(ivb[r, 8]), tr_targ = num(ivb[r, 9]), tr_cost = num(ivb[r, 10]), tr_bal = num(ivb[r, 12]))
}
trS <- bind_rows(trS)
cmpS <- S |> left_join(trS, by = c("fund", "period"))
cat("\nSummarized rates and actuarial balance (percent of taxable payroll), model vs Trustees (IV.B6):\n")
print(as.data.frame(cmpS |> transmute(fund, period, inc = round(inc, 2), tr_inc, cost = round(cost, 2), tr_cost, bal = round(bal, 2), tr_bal)))
cat("\nPresent values 2026-2100 ($ billions), model vs IV.B8 (OASDI: payroll tax 85,559; taxation 6,308; cost 123,731; payroll 690,396; open-group unfunded obligation 29,303):\n")
print(as.data.frame(S |> filter(period == "2026-2100") |> transmute(fund, pv_tax = round(pv_tax / 1e9), pv_taxben = round(pv_taxben / 1e9), pv_cost = round(pv_cost / 1e9),
                                                                     pv_pay = round(pv_pay / 1e9), unf_obl = round(unf_obl))))

dep <- ops |> filter(eoy < 0) |> group_by(fund) |> summarise(depleted = min(year))
cat("\nReserve depletion (first year ending negative): model\n"); print(dep); cat("Trustees (IV.B5): OASI 2032, DI not depleted by 2100, OASDI 2034\n")

cmpA <- ops |> filter(fund != "OASDI" | TRUE) |> select(fund, year, inc_rate, cost_rate, tfr) |>
  left_join(ib1 |> transmute(year, OASI_i = oasi_inc, OASI_c = oasi_cost, DI_i = di_inc, DI_c = di_cost, OASDI_i = oasdi_inc, OASDI_c = oasdi_cost) |>
              pivot_longer(-year, names_to = c("fund", "k"), names_sep = "_") |> pivot_wider(names_from = k, values_from = value) |> rename(tr_inc = i, tr_cost = c), by = c("fund", "year"))
cat("\nAnnual income and cost rates (percent of taxable payroll), model vs Trustees (IV.B1):\n")
print(as.data.frame(cmpA |> filter(year %in% c(2026, 2030, 2035, 2040, 2050, 2060, 2075, 2090, 2100)) |>
  transmute(fund, year, inc = round(inc_rate, 2), tr_inc, cost = round(cost_rate, 2), tr_cost) |> arrange(fund, year)))

write.csv(cmpA, "outputs/trust_fund_vs_tr.csv", row.names = FALSE)
saveRDS(list(operations = ops, summarized = cmpS, annual_vs_tr = cmpA, yields = y, lag = lag, depletion = dep), "data/trust_fund.rds")
cat("Saved data/trust_fund.rds, outputs/trust_fund_vs_tr.csv\n")
