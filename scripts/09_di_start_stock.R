# 09_di_start_stock.R
#
# Disabled workers at the end of 2025 by sex, age at entitlement and duration
# (methodology 3.2.c, equation 3.2.3: OCACT reads this from the Master
# Beneficiary Record; we rebuild it). Published margins:
#   by single age at December 2025          Supplement 2026, 5.A1.2
#   by year of entitlement                  Supplement 2026, 5.D1
# Neither gives the age-by-duration cross. We build a prior table and rake it
# to both margins (DECISIONS.md DS-01 to DS-04):
#   1. Entrants by year of entitlement (1966-2025), sex and age group from
#      awards by age at award (Supplement 6.C2, annual from 1980; earlier years
#      use 1980's age mix). Single ages within a group in proportion to
#      incidence (Actuarial Note 2026.6 at age of entitlement, DI-03) times
#      disability-insured exposure that year (scripts/07).
#   2. Carry each year's entrants to December 2025 with the Study 130
#      select-and-ultimate probabilities of death and recovery (2016-20 base),
#      entrants exposed half a year in the year of entitlement (the note's
#      convention). Those past age 66 have converted at NRA and drop out.
#   3. Attained age at December 2025: entitlement age + duration, split evenly
#      with one year older (entitlement falls mid-year on average).
#   4. Rake (iterative proportional fitting) by sex to 5.A1.2 by age and 5.D1
#      by year of entitlement. This is the stock in current pay.
#   5. IBNR (incurred but not reported) factors: the share of those entitled
#      at each duration whose awards have been processed. Estimated from two
#      vintages of 5.D1 (December 2024, Supplement 2025; December 2025,
#      Supplement 2026): an entitlement-year cohort grows from d to d + 1 by
#      IBNR(d + 1) / IBNR(d) times its survival. Survival by duration comes
#      from Study 130 and conversions on this stock, scaled so durations 5-15
#      (IBNR = 1) match their observed ratios. Currently entitled = current
#      pay / IBNR(duration), as OCACT converts (methodology 3.2.c).
#
# Input:  data/di_inputs.rds (08), data/insured_rates_calibrated.rds (07),
#         data/population_dec.rds (01)
# Output: data/di_stock_2025.rds (current pay and currently entitled), data/di_ibnr.rds

library(dplyr)
library(tidyr)
source("R/read_di.R")

di  <- readRDS("data/di_inputs.rds")
ins <- readRDS("data/insured_rates_calibrated.rds")
pop <- readRDS("data/population_dec.rds") |>
  group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop")
awards_hist <- read_supp_6c2()

ent_years <- 1966:2025
ent_ages  <- 16:66

# ---- 1. Entrants by year, sex and single age at entitlement ---------------------
# Incidence shape at entitlement age a: mean of the note's rows a and a + 1
# (F-07). Ages 16-19 aren't in the note: 18-19 take the age-20 value, 16-17 half.
inc <- di$an_rates |> group_by(sex) |> arrange(age) |>
  mutate(ent = (incidence + coalesce(lead(incidence), incidence)) / 2) |>
  ungroup() |> select(sex, age, ent)
inc <- bind_rows(inc, inc |> filter(age == 20) |> select(sex, ent) |>
                   expand_grid(age = 16:19) |> mutate(ent = ifelse(age <= 17, ent / 2, ent))) |>
  filter(age %in% ent_ages)

grp_ages <- tibble(group = c("u30", "30_39", "40_44", "45_49", "50_54", "55_59", "60_61", "62_64", "65_fra"),
                   lo = c(16, 30, 40, 45, 50, 55, 60, 62, 65), hi = c(29, 39, 44, 49, 54, 59, 61, 64, 66))

# Awards by year and group; years without data take the nearest published year
# (1966-79: 1970, 1975 and 1980 age mixes and totals as available).
aw <- awards_hist |> filter(year >= 1965)
yrs_have <- sort(unique(aw$year))
aw_full <- expand_grid(year = ent_years, sex = factor(c("M", "F"), levels = c("M", "F"))) |>
  mutate(src = sapply(year, function(y) yrs_have[which.min(abs(yrs_have - y))])) |>
  inner_join(aw |> rename(src = year), by = c("src", "sex"), relationship = "many-to-many") |>
  select(year, sex, group, awards)

expo <- ins |> select(year, sex, age, disability) |>
  inner_join(pop, by = c("year", "sex", "age")) |>
  transmute(year, sex, age, exposure = disability * pop)
expo <- bind_rows(expo, expo |> filter(year == 1970) |> select(-year) |> expand_grid(year = 1966:1969))

entrants <- expand_grid(year = ent_years, sex = factor(c("M", "F"), levels = c("M", "F")), age = ent_ages) |>
  inner_join(grp_ages, by = join_by(between(age, lo, hi))) |>
  left_join(inc, by = c("sex", "age")) |>
  left_join(expo, by = c("year", "sex", "age")) |>
  mutate(exposure = coalesce(exposure, 0), w = ent * exposure) |>
  group_by(year, sex, group) |> mutate(share = w / sum(w)) |> ungroup() |>
  inner_join(aw_full, by = c("year", "sex", "group")) |>
  transmute(ent_year = year, sex, entl_age = age, entrants = awards * share)

# ---- 2. Carry to December 2025 ------------------------------------------------------
q_at <- function(tab, s, e, t) {
  # select (t = 0-9) or ultimate (t >= 10, indexed by attained age e + t)
  if (t <= 9) {
    v <- tab$q[tab$sex == s & tab$entl_age == e & tab$duration == t]
  } else {
    a <- e + t
    v <- tab$q[tab$sex == s & tab$duration == 10 & tab$attained_age == a]
  }
  if (length(v) == 0 || is.na(v)) 0 else v
}
surv <- expand_grid(sex = factor(c("M", "F"), levels = c("M", "F")), entl_age = ent_ages, ent_year = ent_years) |>
  mutate(d = 2025L - ent_year) |>
  filter(entl_age + d <= 66)
surv$s <- mapply(function(s, e, d) {
  s <- as.character(s)
  qd <- sapply(0:d, function(t) q_at(di$su_death, s, e, t))
  qr <- sapply(0:d, function(t) q_at(di$su_recov, s, e, t))
  full <- if (d > 0) prod(1 - qd[1:d] - qr[1:d]) else 1
  full * (1 - 0.5 * (qd[d + 1] + qr[d + 1]))
}, surv$sex, surv$entl_age, surv$d)

prior <- entrants |> inner_join(surv, by = c("sex", "entl_age", "ent_year")) |>
  mutate(number = entrants * s) |>
  # 3. attained age at December 2025, split with one year older
  reframe(sex, entl_age, ent_year, duration = d,
          attained_age = c(entl_age + d, entl_age + d + 1), number = number / 2, .by = c(sex, entl_age, ent_year)) |>
  select(sex, entl_age, ent_year, duration, attained_age, number) |>
  filter(attained_age <= 66) |>
  mutate(age_m = pmax(19L, attained_age), year_m = pmax(1986L, ent_year))  # 5.D1: "Before 1987" = 1986

# ---- 4. Rake to the published margins -----------------------------------------------
m_age <- di$stock_age |> rename(age_m = age, t_age = number)
m_ent <- di$stock_ent |> transmute(sex, year_m = ent_year, t_ent = number)
rake <- function(d) {
  for (it in 1:200) {
    d <- d |> group_by(sex, age_m) |> mutate(number = number * first(t_age) / sum(number)) |> ungroup()
    d <- d |> group_by(sex, year_m) |> mutate(number = number * first(t_ent) / sum(number)) |> ungroup()
    gap <- d |> group_by(sex, age_m) |> summarise(g = abs(sum(number) - first(t_age)), .groups = "drop")
    if (max(gap$g) < 0.5) break
  }
  attr(d, "iterations") <- it
  d
}
stock <- prior |> inner_join(m_age, by = c("sex", "age_m")) |> inner_join(m_ent, by = c("sex", "year_m"))
stopifnot(abs(sum(stock$number) - sum(prior$number[prior$age_m %in% m_age$age_m])) < 1e6)
stock <- rake(stock)
cat("Raking converged in", attr(stock, "iterations"), "iterations\n")

# ---- Checks ---------------------------------------------------------------------------
chk_age <- stock |> group_by(sex, age_m) |> summarise(n = sum(number), t = first(t_age), .groups = "drop")
chk_ent <- stock |> group_by(sex, year_m) |> summarise(n = sum(number), t = first(t_ent), .groups = "drop")
cat("Largest gap to 5.A1.2 (by age):", round(max(abs(chk_age$n - chk_age$t))),
    "| to 5.D1 (by entitlement year):", round(max(abs(chk_ent$n - chk_ent$t))), "\n")
cat("Raking adjustment (raked / prior), by sex: ")
print(stock |> group_by(sex) |> summarise(prior = sum(prior$number[prior$sex == first(sex)]), raked = sum(number)) |>
        mutate(ratio = round(raked / prior, 3)))

cat("\nMean age at entitlement by duration band, December 2025:\n")
print(stock |> mutate(band = cut(duration, c(-1, 0, 2, 4, 9, 19, 60), labels = c("0", "1-2", "3-4", "5-9", "10-19", "20+"))) |>
        group_by(sex, band) |> summarise(n_thousands = round(sum(number) / 1000), mean_entl_age = round(sum(number * entl_age) / sum(number), 1),
                                         .groups = "drop") |>
        pivot_wider(names_from = sex, values_from = c(n_thousands, mean_entl_age)))

# Exits in 2026 implied by the Study 130 base rates, against 2025 actuals
# (Supplement 6.F2: deaths 217,802; disability ceased 84,602; conversions at
# FRA 457,350). The death and recovery gaps set the starting projection
# factors (DPROJG, RPROJG) in the next step.
exp_exits <- stock |> rowwise() |>
  mutate(qd = q_at(di$su_death, as.character(sex), entl_age, duration),
         qr = q_at(di$su_recov, as.character(sex), entl_age, duration)) |> ungroup() |>
  summarise(deaths = sum(number * qd), recoveries = sum(number * qr))
cat("\nExits implied by Study 130 base rates on this stock: deaths", round(exp_exits$deaths),
    "(2025 actual 217,802) | recoveries", round(exp_exits$recoveries), "(2025 actual 84,602)\n")

# ---- 5. IBNR factors and the currently entitled stock ---------------------------------
v25 <- di$stock_ent |> filter(!before)
v24 <- read_supp_5d1("data-raw/supplement/supplement25_all.xlsx") |> filter(!before)
growth <- v25 |> inner_join(v24, by = c("ent_year", "sex"), suffix = c("_25", "_24")) |>
  mutate(d24 = 2024L - ent_year, ratio = number_25 / number_24)

# One-year survival by duration on this stock: Study 130 base death and
# recovery (with roughly the 2025 factors, F-08) plus conversion at NRA.
nra <- readRDS("data/params_by_cohort.rds") |> transmute(birth_year, nra = nra_months / 12)
surv_d <- stock |> rowwise() |>
  mutate(qd = 1.11 * q_at(di$su_death, as.character(sex), entl_age, duration),
         qr = 1.8 * q_at(di$su_recov, as.character(sex), entl_age, duration)) |> ungroup() |>
  mutate(b = 2026L - (attained_age + 1L),
         nra_b = coalesce(nra$nra[match(b, nra$birth_year)], 67),
         stay = pmin(1, pmax(0, nra_b - (attained_age + 1L))),
         s = (1 - qd - qr) * stay) |>
  group_by(sex, duration) |> summarise(s = sum(number * s) / sum(number), .groups = "drop")
# Scale so durations 5-15 match the observed cohort ratios (IBNR = 1 there).
scale <- growth |> filter(d24 >= 5, d24 <= 15) |>
  inner_join(surv_d |> rename(d24 = duration), by = c("sex", "d24")) |>
  group_by(sex) |> summarise(k = mean(ratio) / mean(s))
ibnr <- growth |> filter(d24 <= 4) |>
  inner_join(surv_d |> rename(d24 = duration), by = c("sex", "d24")) |>
  inner_join(scale, by = "sex") |>
  mutate(step = ratio / (s * k)) |>                     # IBNR(d+1) / IBNR(d)
  arrange(sex, desc(d24)) |> group_by(sex) |>
  mutate(ibnr = 1 / cumprod(step)) |> ungroup() |>     # IBNR(5) = 1, back to IBNR(0)
  transmute(sex, duration = d24, ibnr = pmin(1, ibnr))
ibnr <- bind_rows(ibnr, expand_grid(sex = factor(c("M", "F"), levels = c("M", "F")), duration = 5:60, ibnr = 1)) |>
  arrange(sex, duration)
cat("\nIBNR factors (share of the entitled in current pay) by duration:\n")
print(ibnr |> filter(duration <= 5) |> mutate(ibnr = round(ibnr, 3)) |> pivot_wider(names_from = sex, values_from = ibnr))
cat("Survival scale (observed / Study 130 + conversions), durations 5-15:", round(scale$k, 3), "\n")

stock <- stock |> left_join(ibnr, by = c("sex", "duration")) |>
  mutate(ibnr = coalesce(ibnr, 1), entitled = number / ibnr)
cat("Currently entitled, December 2025:", round(sum(stock$entitled)), "| in current pay:", round(sum(stock$number)), "\n")

saveRDS(stock |> select(sex, entl_age, ent_year, duration, attained_age, current_pay = number, entitled),
        "data/di_stock_2025.rds")
saveRDS(ibnr, "data/di_ibnr.rds")
cat("Saved data/di_stock_2025.rds, data/di_ibnr.rds\n")
