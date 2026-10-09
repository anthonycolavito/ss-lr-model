# 15_oasi_auxiliaries.R
#
# Dependents of retired and deceased workers, 2025-2100 (methodology 3.3,
# equations 3.3.3-3.3.12), as for dependents of disabled workers (scripts/11):
#
#   beneficiaries = exposure x linkages x residual
#
# Categories (TR V.C4 group in brackets):
#   aged spouses of retired workers, 62+, married / divorced      [rw_spouse]
#   young spouses of retired workers caring for a child           [rw_spouse]
#   minor / student / disabled adult children of retired workers  [rw_child]
#   minor / student / disabled adult children of deceased workers [surv_child]
#   widowed mothers and fathers (young spouses of deceased)        [mother]
#   parents of deceased workers                                    [parent]
# Linkages (DECISIONS.md OA-01 to OA-05)
#   P_RW(age, sex)     retired workers (scripts/13, incl. converted DI) /
#                      population: the account holder is a retired worker.
#   spouse's age       own age + gap (husband 2.5 years older, s.d. 4.5; DA-03).
#   parent's age       child's age + age at birth (DA-03 distributions).
#   parent deceased    cumulative general-population mortality from the
#                      child's birth; parent fully insured at that age.
#   own insured        aged spouses: 1 - own fully insured (spouses insured on
#                      their own record are counted as retired workers).
#   child in care      children under 16 per adult (DA-04).
# Residuals: December 2025 counts by category (Supplement 2026, 5.A1.3, 5.A1.4,
# 5.A1.5, 5.A1.8) / linkage index, held. Levels: V.C4 group totals every year,
# split by the model (P-02, as DA-06 and AW-06); the model's own path is the
# test (F-16).
#
# Input:  data/population_dec.rds (01), data/death_probs.rds (02),
#         data/insured_rates_calibrated.rds (07), data/retired_workers.rds (13)
# Output: data/oasi_auxiliaries.rds, outputs/oasi_auxiliaries_checks.csv

library(dplyr)
library(tidyr)
source("R/read_tr.R")

sexes <- c("M", "F")
years <- 2025:2100
yi <- function(t) t - 2024

popm <- readRDS("data/population_dec.rds") |>
  mutate(sex = as.character(sex), age = pmin(as.integer(age), 100L), year = as.integer(year), marital = as.character(marital)) |>
  filter(year %in% years)
mk <- function(d, v) {
  a <- array(0, c(2, 101, length(years)), dimnames = list(sexes, 0:100, years))
  a[cbind(match(d$sex, sexes), d$age + 1, yi(d$year))] <- d[[v]]
  a
}
agg <- function(m = NULL) {
  d <- if (is.null(m)) popm else filter(popm, marital %in% m)
  d |> group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop")
}
POP <- mk(agg(), "pop"); MAR <- mk(agg("married"), "pop"); DIV <- mk(agg("divorced"), "pop"); WID <- mk(agg("widowed"), "pop")
fi <- readRDS("data/insured_rates_calibrated.rds") |> mutate(sex = as.character(sex), age = pmin(as.integer(age), 100L)) |>
  filter(year %in% years) |> group_by(year, sex, age) |> summarise(fully = mean(fully), .groups = "drop")
FI <- mk(fi, "fully")
rw <- readRDS("data/retired_workers.rds")
rws <- bind_rows(rw$hist |> filter(year == 2025) |> select(year, sex, age, rw),
                 rw$proj |> select(year, sex, age, rw)) |> mutate(age = as.integer(age))
RW <- mk(rws, "rw")
PRW <- ifelse(POP > 0, pmin(1, RW / POP), 0)

# Parent's survival: cumulative log survival by parent birth cohort and age
q <- readRDS("data/death_probs.rds") |> mutate(sex = as.character(sex))
bp_range <- 1840:2100
LS <- array(0, c(2, length(bp_range), 122), dimnames = list(sexes, bp_range, 0:121))
qa <- array(NA, c(2, 201, 121), dimnames = list(sexes, 1900:2100, 0:120))
qq <- q |> filter(age <= 120)
qa[cbind(match(qq$sex, sexes), qq$year - 1899, qq$age + 1)] <- qq$qx
for (s in 1:2) for (j in seq_along(bp_range)) {
  yrs <- pmin(2100, pmax(1900, bp_range[j] + 0:120))
  qv <- qa[cbind(s, yrs - 1899, 1:121)]; qv[is.na(qv)] <- 0.5
  LS[s, j, ] <- c(0, cumsum(log(1 - pmin(qv, 0.999))))
}

kern <- function(ages, m, s) { w <- dnorm(ages, m, s); w / sum(w) }
A <- 15:55
f_birth <- list(F = kern(A, 29, 5.5), M = kern(A, 31.5, 6.5))
O <- -15:25
g_gap <- list(F = kern(O, 2.5, 4.5), M = kern(-O, 2.5, 4.5))   # partner's age minus own age, by own sex

kids16 <- function(y, t, s) {
  cs <- 0:15; ab <- y - cs; ok <- ab >= 15 & ab <= 55
  if (!any(ok) || POP[s, y + 1, yi(t)] == 0) return(0)
  sum((POP["M", cs[ok] + 1, yi(t)] + POP["F", cs[ok] + 1, yi(t)]) * f_birth[[s]][ab[ok] - 14]) / POP[s, y + 1, yi(t)]
}
partner_link <- function(b, x, t, f) {           # sum over partner ages of gap weight x f(partner age)
  partner <- setdiff(sexes, b); y <- x + O; ok <- y >= 0 & y <= 100
  sum(g_gap[[b]][ok] * sapply(y[ok], function(yy) f(partner, yy)))
}
parent_dead_insured <- function(c, t, s) {      # P(parent of sex s is deceased and was insured), child aged c
  a_now <- c + A; ok <- a_now <= 120
  bp <- t - a_now[ok]; j <- match(bp, bp_range)
  si <- match(s, sexes)
  surv <- exp(LS[cbind(si, j, a_now[ok] + 1)] - LS[cbind(si, j, A[ok] + 1)])
  fi_p <- FI[s, pmin(a_now[ok], 100) + 1, yi(t)]
  sum(f_birth[[s]][ok] * (1 - surv) * fi_p)
}
parent_rw <- function(c, t, s) {
  a_now <- c + A; ok <- a_now <= 100
  sum(f_birth[[s]][ok] * PRW[s, a_now[ok] + 1, yi(t)])
}

index_year <- function(t) {
  child_idx <- function(cs, fn) sum(sapply(cs, function(c)
    (POP["M", min(c, 100) + 1, yi(t)] + POP["F", min(c, 100) + 1, yi(t)]) * (fn(c, t, "M") + fn(c, t, "F"))))
  aged_sp <- function(b, arr) sum(sapply(62:100, function(x)
    arr[b, x + 1, yi(t)] * (1 - FI[b, x + 1, yi(t)]) * partner_link(b, x, t, function(p, y) PRW[p, y + 1, yi(t)])))
  young_sp <- function(b) sum(sapply(20:69, function(x)
    MAR[b, x + 1, yi(t)] * partner_link(b, x, t, function(p, y) PRW[p, y + 1, yi(t)] * kids16(y, t, p))))
  mothers <- function(b) sum(sapply(18:69, function(x)
    WID[b, x + 1, yi(t)] * kids16(x, t, b) * partner_link(b, x, t, function(p, y) FI[p, y + 1, yi(t)])))
  tibble(year = t,
         rw_minor = child_idx(0:17, parent_rw), rw_student = child_idx(18:19, parent_rw), rw_dac = child_idx(18:80, parent_rw),
         dw_minor = child_idx(0:17, parent_dead_insured), dw_student = child_idx(18:19, parent_dead_insured),
         dw_dac = child_idx(18:80, parent_dead_insured),
         aged_mar_F = aged_sp("F", MAR), aged_mar_M = aged_sp("M", MAR),
         aged_div_F = aged_sp("F", DIV), aged_div_M = aged_sp("M", DIV),
         young_F = young_sp("F"), young_M = young_sp("M"),
         mother_F = mothers("F"), mother_M = mothers("M"))
}
idx <- bind_rows(lapply(years, index_year)) |> pivot_longer(-year, names_to = "category", values_to = "index")

# ---- Residuals from December 2025 (Supplement 2026) ---------------------------------------------
t25 <- tribble(~category, ~n,
  "rw_minor", 322812, "rw_student", 15683, "rw_dac", 396427,
  "dw_minor", 1271622, "dw_student", 50392, "dw_dac", 714031,
  "aged_mar_F", 1880823 - 167798, "aged_mar_M", 173450 - 17555,      # 5.A1.3: aged, less divorced (taken as all aged)
  "aged_div_F", 167798, "aged_div_M", 17555,
  "young_F", 27265, "young_M", 208,
  "mother_F", 90772, "mother_M", 8083)
group_of <- c(rw_minor = "rw_child", rw_student = "rw_child", rw_dac = "rw_child",
              dw_minor = "surv_child", dw_student = "surv_child", dw_dac = "surv_child",
              aged_mar_F = "rw_spouse", aged_mar_M = "rw_spouse", aged_div_F = "rw_spouse", aged_div_M = "rw_spouse",
              young_F = "rw_spouse", young_M = "rw_spouse", mother_F = "mother", mother_M = "mother")
res <- idx |> filter(year == 2025) |> inner_join(t25, by = "category") |> transmute(category, res = n / index)
raw <- idx |> inner_join(res, by = "category") |> mutate(n = index * res, group = group_of[category])

vc4 <- read_tr_single_year("V.C4", c("rw", "rw_spouse", "rw_child", "widow", "mother", "surv_child", "parent", "total")) |>
  select(year, rw_spouse, rw_child, mother, surv_child, parent) |>
  pivot_longer(-year, names_to = "group", values_to = "tr") |> mutate(tr = 1000 * tr)
aux <- raw |> group_by(year, group) |> mutate(share = n / sum(n)) |> ungroup() |>
  inner_join(vc4, by = c("year", "group")) |> transmute(year, group, category, n_model = n, share, n = tr * share)
parents <- vc4 |> filter(group == "parent") |> transmute(year, group, category = "parent", n_model = tr, share = 1, n = tr)
aux <- bind_rows(aux, parents |> filter(year %in% years))

# ---- Checks ------------------------------------------------------------------------------------------
cmp <- raw |> group_by(year, group) |> summarise(model = sum(n), .groups = "drop") |>
  inner_join(vc4, by = c("year", "group")) |> mutate(gap_pct = 100 * (model / tr - 1))
cat("Model alone (residuals fixed at 2025) vs TR V.C4, percent gap:\n")
print(cmp |> filter(year %in% c(2025, 2035, 2050, 2075, 2099)) |> select(year, group, gap_pct) |>
        mutate(gap_pct = round(gap_pct, 1)) |> pivot_wider(names_from = group, values_from = gap_pct))
cat("\nV.C4 levels (thousands):\n")
print(vc4 |> filter(year %in% c(2025, 2035, 2050, 2075, 2099), group != "parent") |> mutate(tr = round(tr / 1000)) |>
        pivot_wider(names_from = group, values_from = tr))
cat("\nShares within groups, 2025 / 2099 (percent):\n")
print(aux |> filter(year %in% c(2025, 2099), group != "parent") |> transmute(year, category, pct = round(100 * share, 1)) |>
        pivot_wider(names_from = year, values_from = pct))

dir.create("outputs", showWarnings = FALSE)
write.csv(cmp, "outputs/oasi_auxiliaries_checks.csv", row.names = FALSE)
saveRDS(list(aux = aux, residual = res, model_vs_tr = cmp), "data/oasi_auxiliaries.rds")
cat("Saved data/oasi_auxiliaries.rds\n")
