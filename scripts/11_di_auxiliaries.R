# 11_di_auxiliaries.R
#
# Dependents of disabled workers, 1990-2100 (methodology 3.2, equation 3.2.4):
#
#   beneficiaries = exposure x linkages x residual factor
#
# for six categories: minor child (0-17), student child (18-19), disabled adult
# child, young spouse (caring for a child), married aged spouse (62+) and
# divorced aged spouse (62+). OCACT's linkages are probabilities that the
# account holder is under NRA, disability insured and disabled, plus category
# conditions. We build them from (DECISIONS.md DA-01 to DA-06):
#   P_DI(age, sex, year)  disabled workers in current pay / population: the
#                         product of OCACT's three account-holder linkages.
#                         History: Study 130 Table 6 (2001-24) spread to single
#                         ages with the December 2025 shape; 2025: the stock
#                         (scripts/09); 2026-2100: scripts/10.
#   parent's age          child's age + age at birth, fixed distributions
#                         (mothers mean 29, s.d. 5.5; fathers 31.5, 6.5) - OCACT
#                         uses children by age of parent, which isn't published.
#   spouse's age          own age + gap (husband 2.5 years older, s.d. 4.5) -
#                         OCACT uses married couples by age of husband x wife.
#   child in care         children under 16 per parent at the account holder's
#                         age and sex (same age-at-birth distributions).
#   not insured           1 - fully insured rate of the beneficiary (scripts/07).
#   other conditions      school, disabled child, earnings test, married 10+
#                         years: in the residual factor.
# Residual factors: each category's December 2025 count (Supplement 2026,
# 5.A1.3 and 5.A1.4, disabled-worker basis) divided by its linkage index, held
# constant (OCACT fits a 10-year regression; we have one year by category).
# Levels: TR V.C5 child and spouse totals, every year; the model splits them by
# category (DA-06). The model's own levels are reported against V.C5.
#
# Input:  data/di_inputs.rds (08), data/di_stock_2025.rds (09),
#         data/di_projection.rds (10), data/insured_rates_calibrated.rds (07),
#         data/population_dec.rds (01)
# Output: data/di_auxiliaries.rds, outputs/di_auxiliaries_checks.csv

library(dplyr)
library(tidyr)

di   <- readRDS("data/di_inputs.rds")
st25 <- readRDS("data/di_stock_2025.rds")
proj <- readRDS("data/di_projection.rds")
ins  <- readRDS("data/insured_rates_calibrated.rds")
popm <- readRDS("data/population_dec.rds") |> mutate(sex = as.character(sex))
pop  <- popm |> group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop")
sexes <- c("M", "F")
years <- 2001:2100

# ---- Disabled-worker prevalence P_DI(age, sex, year) ----------------------------------
ag <- c("a15_19", "a20_24", "a25_29", "a30_34", "a35_39", "a40_44", "a45_49", "a50_54", "a55_59", "a60_64", "a65_66")
glo <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)
cp25 <- st25 |> mutate(sex = as.character(sex), age = pmax(attained_age, 15L)) |>
  group_by(sex, age) |> summarise(n = sum(current_pay), .groups = "drop")
shape <- cp25 |> mutate(g = ag[findInterval(age, glo)]) |> group_by(sex, g) |> mutate(w = n / sum(n)) |> ungroup()
dib <- bind_rows(
  di$hist_inforce |> filter(sex != "T") |> mutate(sex = as.character(sex)) |>
    pivot_longer(all_of(ag), names_to = "g", values_to = "n") |>
    inner_join(shape |> select(sex, g, age, w), by = c("sex", "g"), relationship = "many-to-many") |>
    transmute(year, sex, age, n = n * w),
  cp25 |> mutate(year = 2025L),
  proj$stock_age |> transmute(year, sex = as.character(sex), age = a, n = cp)
)
P <- array(0, c(2, 121, length(years)), dimnames = list(sexes, 0:120, years))
d0 <- dib |> inner_join(pop, by = c("year", "sex", "age")) |> filter(year %in% years)
P[cbind(match(d0$sex, sexes), d0$age + 1, d0$year - 2000)] <- d0$n / d0$pop

# ---- Age kernels ------------------------------------------------------------------------
kern <- function(ages, m, s) { w <- dnorm(ages, m, s); w / sum(w) }
A <- 15:55
f_birth <- list(F = kern(A, 29, 5.5), M = kern(A, 31.5, 6.5))       # parent's age at birth
O <- -15:25
g_gap <- list(F = kern(O, 2.5, 4.5), M = kern(-O, 2.5, 4.5))        # partner's age minus own age, by own sex
names(g_gap$M) <- NULL

# Population arrays [sex, age, year], total and by marital status
mk <- function(d) {
  a <- array(0, c(2, 121, length(years)), dimnames = list(sexes, 0:120, years))
  d <- d |> filter(year %in% years)
  a[cbind(match(d$sex, sexes), pmin(d$age, 120) + 1, d$year - 2000)] <- d$pop
  a
}
POP <- mk(pop)
MAR <- mk(popm |> filter(marital == "married") |> select(year, sex, age, pop))
DIV <- mk(popm |> filter(marital == "divorced") |> select(year, sex, age, pop))
FI <- array(0, c(2, 121, length(years)), dimnames = list(sexes, 0:120, years))
fi <- ins |> mutate(sex = as.character(sex)) |> filter(year %in% years)
FI[cbind(match(fi$sex, sexes), fi$age + 1, fi$year - 2000)] <- fi$fully

# Expected disabled parent of sex s for a child aged c in year t
parent_dis <- function(c, t, s) {
  y <- c + A; ok <- y <= 120
  sum(f_birth[[s]][ok] * P[s, y[ok] + 1, t - 2000])
}
# Children under 16 per person of sex s aged y in year t (child-in-care proxy)
kids16 <- function(y, t, s) {
  cs <- 0:15; a_birth <- y - cs; ok <- a_birth >= 15 & a_birth <= 55
  if (!any(ok) || POP[s, y + 1, t - 2000] == 0) return(0)
  sum((POP["M", cs[ok] + 1, t - 2000] + POP["F", cs[ok] + 1, t - 2000]) * f_birth[[s]][a_birth[ok] - 14]) /
    POP[s, y + 1, t - 2000]
}

# ---- Linkage indices by category ------------------------------------------------------------
index_year <- function(t) {
  child <- function(cs) sum(sapply(cs, function(c) (POP["M", c + 1, t - 2000] + POP["F", c + 1, t - 2000]) *
                                      (parent_dis(c, t, "M") + parent_dis(c, t, "F"))))
  spouse <- function(b, ages, pop_arr, extra) {
    partner <- setdiff(sexes, b)
    sum(sapply(ages, function(x) {
      y <- x + O; ok <- y >= 15 & y <= 120
      pd <- sum(g_gap[[b]][ok] * P[partner, y[ok] + 1, t - 2000] * extra(y[ok], x, partner, b))
      pop_arr[b, x + 1, t - 2000] * pd
    }))
  }
  care  <- function(y, x, partner, b) sapply(y, function(yy) kids16(yy, t, partner))
  notin <- function(y, x, partner, b) rep(1 - FI[b, x + 1, t - 2000], length(y))
  tibble(year = t,
         minor = child(0:17), student = child(18:19), dac = child(18:66),
         young_F = spouse("F", 20:64, MAR, care), young_M = spouse("M", 20:64, MAR, care),
         aged_F = spouse("F", 62:100, MAR, notin), aged_M = spouse("M", 62:100, MAR, notin),
         div_F = spouse("F", 62:100, DIV, notin), div_M = spouse("M", 62:100, DIV, notin))
}
idx <- bind_rows(lapply(years, index_year)) |>
  pivot_longer(-year, names_to = "category", values_to = "index")

# ---- Residual factors from December 2025 (Supplement 2026, disabled-worker basis) ----------
# Spouses: 5.A1.3 gives care-of-child and aged by sex of spouse, and divorced in
# total by sex; divorced spouses are taken as all aged.
t25 <- tribble(~category, ~n,
  "minor", 813433, "student", 25488, "dac", 109482,
  "young_F", 17872, "young_M", 621,
  "aged_F", 61544 - 10283, "aged_M", 9411 - 2088,
  "div_F", 10283, "div_M", 2088)
res <- idx |> filter(year == 2025) |> inner_join(t25, by = "category") |> transmute(category, res = n / index)

raw <- idx |> inner_join(res, by = "category") |> mutate(n = index * res,
  group = ifelse(category %in% c("minor", "student", "dac"), "child", "spouse"))

# ---- Levels from TR V.C5 --------------------------------------------------------------------
# Out of sample, the linkage indices can't reproduce the TR's falling spouses
# per disabled worker (DECISIONS.md F-13): the trends come from linkages OCACT
# doesn't publish. V.C5 publishes the child and spouse totals for every year,
# so - as for every published projection (P-02) - we take those levels as
# given and use the indices to split each total by category (DA-06). For
# reforms, the change in an index relative to baseline scales its category.
vc5 <- di$vc5 |> transmute(year, child = 1000 * child, spouse = 1000 * spouse) |>
  pivot_longer(-year, names_to = "group", values_to = "tr")
aux <- raw |> group_by(year, group) |> mutate(share = n / sum(n)) |> ungroup() |>
  inner_join(vc5, by = c("year", "group")) |>
  transmute(year, group, category, index, res, share, n_model = n, n = tr * share)

# ---- Checks -------------------------------------------------------------------------------------
cmp <- raw |> group_by(year, group) |> summarise(n = sum(n), .groups = "drop") |>
  left_join(vc5, by = c("year", "group")) |> mutate(gap_pct = 100 * (n / tr - 1))
cat("Residual factors (2025 count / linkage index):\n"); print(res |> mutate(res = signif(res, 3)))
cat("\nLinkage model alone (residuals fixed at 2025) vs TR V.C5, thousands - the test behind DA-06:\n")
print(cmp |> filter(year %in% c(2001, 2010, 2020, 2025, 2035, 2050, 2075, 2100)) |>
        transmute(year, group, model_k = round(n / 1000), tr_k = round(tr / 1000), gap_pct = round(gap_pct, 1)) |>
        pivot_wider(names_from = group, values_from = c(model_k, tr_k, gap_pct)), n = Inf)
cat("\nCategory shares within V.C5 totals (percent):\n")
print(aux |> filter(year %in% c(2025, 2050, 2100)) |> transmute(year, category, pct = round(100 * share, 1)) |>
        pivot_wider(names_from = year, values_from = pct))

dir.create("outputs", showWarnings = FALSE)
write.csv(cmp, "outputs/di_auxiliaries_checks.csv", row.names = FALSE)
saveRDS(list(aux = aux, residual = res, model_vs_tr = cmp), "data/di_auxiliaries.rds")
cat("Saved data/di_auxiliaries.rds\n")
