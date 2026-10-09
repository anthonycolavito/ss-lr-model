# 08_di_inputs.R
#
# Imports everything the disabled-worker projection (Phase 2, methodology
# section 3.2) needs, checks each source against a published total, and saves
# one list. No modeling choices are made here beyond reading; derived rates
# from the actuarial note are kept separate and labeled.
#
# Sources (data-raw/SOURCES.md)
#   Actuarial Study No. 130 (2026): select-and-ultimate probabilities of death
#     (Tables 7A-7C) and recovery (14A-14B), 2016-20 experience, by age at
#     entitlement and duration; history by year 2001-24 of awards (3),
#     incidence rates (4), terminations and conversions (5), in current pay (6).
#   Actuarial Note 2026.6: survival and disability status of insured workers
#     attaining 20 in 2026, single ages 20-67 (Tables C-D), 2026 TR.
#   Annual Statistical Supplement 2026 (December 2025): disabled workers in
#     current pay by single age and sex (5.A1.2) and by year of entitlement
#     (5.D1); 2025 awards by age (6.A4); 2025 terminations by reason (6.F2).
#   2026 TR Table V.C5: DI beneficiaries and prevalence rates, 1975-2100.
#   Methodology 3.2.b: ultimate incidence rates (entitlement basis) by age group.
#
# Output: data/di_inputs.rds

library(dplyr)
library(tidyr)
source("R/read_tr.R")
source("R/read_di.R")

ag <- c("a15_19", "a20_24", "a25_29", "a30_34", "a35_39", "a40_44", "a45_49",
        "a50_54", "a55_59", "a60_64")

# ---- Actuarial Study No. 130 ----------------------------------------------------
su_death <- bind_rows(read_as130_su("7A") |> mutate(sex = "M"),
                      read_as130_su("7B") |> mutate(sex = "F"))
su_recov <- bind_rows(read_as130_su("14A") |> mutate(sex = "M"),
                      read_as130_su("14B") |> mutate(sex = "F"))
death_76 <- read_as130_7c()
su_death$sex <- factor(su_death$sex, levels = c("M", "F"))
su_recov$sex <- factor(su_recov$sex, levels = c("M", "F"))
death_76$sex <- factor(death_76$sex, levels = c("M", "F"))

# Check: the study's worked example (appendix): male entitled at 50, on the
# rolls 5 years, survives 10 more years with probability .705256.
g <- function(a, du) su_death$q[su_death$sex == "M" & su_death$entl_age == a & su_death$duration %in% du]
p <- prod(1 - c(g(50, 5:9), sapply(50:54, function(a) g(a, 10))))
cat("Study 130 check, male [50]+5 survives 10 years:", round(p, 6), "(study: 0.705256)\n")
stopifnot(abs(p - 0.705256) < 1e-6)

hist_awards  <- read_as130_hist("3", c(ag, "a65p", "total"))
hist_incid   <- read_as130_hist("4", c(ag, "a65p", "gross", "adjusted"))
hist_terms   <- read_as130_hist("5", c("death", "recovery", "other", "conversion", "total",
                                       "death_gross", "death_adj", "rec_gross", "rec_adj",
                                       "oth_gross", "oth_adj", "sub_gross", "sub_adj",
                                       "conv_ratio", "gross_total"))
hist_inforce <- read_as130_hist("6", c(ag, "a65_66", "total"))
for (h in list(hist_awards, hist_inforce)) {
  s <- h |> filter(sex != "T") |> group_by(year) |> summarise(mw = sum(total)) |>
    inner_join(h |> filter(sex == "T") |> select(year, total), by = "year")
  stopifnot(max(abs(s$mw - s$total)) <= 2)
}

# ---- Actuarial Note 2026.6 ------------------------------------------------------
an <- read_an_illustration()
an_check <- an |> group_by(sex) |>
  summarise(p_disabled = sum(new_from_active, na.rm = TRUE) / 1e6,
            p_die_never_disabled = sum(deaths_active, na.rm = TRUE) / 1e6)
cat("\nActuarial note, ages 20-67 (note Table A: men .232 / .112, women .239 / .056):\n")
print(an_check |> mutate(across(where(is.double), ~ round(.x, 3))))
stopifnot(abs(an_check$p_disabled - c(0.232, 0.239)) < 0.0006,
          abs(an_check$p_die_never_disabled - c(0.112, 0.056)) < 0.0006)

# Rates implied by the note (cohort born 2006: age x in year 2006 + x).
# Incidence applies to active + recovered at the start of the year (note,
# "Assumptions and Methods"); entitlement basis.
an_rates <- an |> filter(!is.na(new_disabled)) |>
  mutate(incidence = new_disabled / (active + recovered),
         disabled_death = deaths_disabled / (disabled + new_disabled / 2),
         recovery = recoveries / (disabled + new_disabled / 2)) |>
  select(sex, age, year, incidence, disabled_death, recovery)

# ---- Methodology 3.2.b: ultimate incidence by age group (per 1,000) -------------
ult_incidence <- tibble(
  group = c("u20", "20_24", "25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", "60_64", "65_69"),
  lo = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65), hi = c(19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69),
  M = c(1.08, 1.74, 1.41, 1.94, 2.59, 3.41, 4.69, 8.07, 14.09, 16.25, 8.81),
  F = c(0.98, 1.30, 1.32, 2.09, 2.87, 4.21, 5.48, 8.97, 14.02, 13.60, 7.55)
) |> pivot_longer(c(M, F), names_to = "sex", values_to = "per1000") |>
  mutate(sex = factor(sex, levels = c("M", "F")))

# Diagnostic: the note's single-age incidence, averaged within groups for the
# years it is at ultimate (2036+, ages 30+), against the methodology's table.
# Simple averages; the methodology's rates are exposure-weighted.
cmp_ult <- an_rates |> filter(year >= 2036) |>
  inner_join(ult_incidence, by = join_by(sex, between(age, lo, hi))) |>
  group_by(sex, group) |>
  summarise(note = round(1000 * mean(incidence), 2), methodology = first(per1000), .groups = "drop")
cat("\nUltimate incidence per 1,000, note (single ages averaged) vs methodology:\n")
print(cmp_ult |> pivot_wider(names_from = sex, values_from = c(note, methodology)))

# ---- Supplement 2026 (December 2025) ----------------------------------------------
stock_age  <- read_supp_5a12()          # age 19 = under 20
stock_ent  <- read_supp_5d1()
awards_age <- read_supp_6a4_di()
terms_2025 <- read_supp_6f2_di()

# ---- TR V.C5 ----------------------------------------------------------------------
vc5 <- read_tr_single_year("V.C5", c("dw", "spouse", "child", "total", "prev_gross", "prev_adj"))
cat("\nDisabled workers, December 2025: Supplement 5.A1.2", sum(stock_age$number),
    "| 5.D1", sum(stock_ent$number), "| TR V.C5", 1000 * vc5$dw[vc5$year == 2025], "(thousands, rounded)\n")
stopifnot(abs(sum(stock_age$number) / 1000 - vc5$dw[vc5$year == 2025]) < 1,
          abs(hist_inforce$total[hist_inforce$year == 2024 & hist_inforce$sex == "T"] / 1000 -
                vc5$dw[vc5$year == 2024]) < 1)

# ---- Save ---------------------------------------------------------------------------
di_inputs <- list(
  su_death = su_death, su_recov = su_recov, death_76 = death_76,
  hist_awards = hist_awards, hist_incid = hist_incid, hist_terms = hist_terms,
  hist_inforce = hist_inforce,
  an = an, an_rates = an_rates, ult_incidence = ult_incidence,
  stock_age = stock_age, stock_ent = stock_ent, awards_age = awards_age, terms_2025 = terms_2025,
  vc5 = vc5
)
saveRDS(di_inputs, "data/di_inputs.rds")
cat("\nSaved data/di_inputs.rds\n")
