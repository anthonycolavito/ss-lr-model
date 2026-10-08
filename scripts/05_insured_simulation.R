# 05_insured_simulation.R
#
# Simulates work histories by birth cohort and sex to estimate the share of
# the population that is fully insured and disability insured, by age, sex and
# year, 1970-2100 (methodology section 3.1).
#
#   Step 1  calibrate the persistence parameter rho for each sex to the
#           historical insured rates in Supplement 4.C2
#   Step 2  run every cohort at the chosen rho
#   Step 3  compare with 4.C2 and report
#
# Simplifications relative to OCACT, to revisit if the fit calls for it:
#   - no separate treatment of immigrants (OCACT gives new LPR immigrants a
#     fresh record with no prior earnings) or of temporary/unlawfully present
#     workers; their effect is absorbed by the calibration
#   - workers on the disability rolls lose recent work and so fail the
#     disability-insured test; OCACT adds them back (DINADD). We do this in
#     Phase 2, once disabled-worker counts exist
#   - insured rates for ages 85+ hold each cohort's age-84 value (as OCACT does)
#
# Input:  data/insured_inputs.rds (script 04), data/population_dec.rds
# Output: data/insured_rates.rds  (year, age, sex, fully, disability)

library(dplyr)
library(tidyr)
source("R/insured_sim.R")

inp <- readRDS("data/insured_inputs.rds")
pop_dec <- readRDS("data/population_dec.rds")
set.seed(20261008)

ages <- 13:84
first_cohort <- 1870        # aged 100 in 1970, the first target year
last_cohort  <- 2087        # aged 13 in 2100

# Lookup arrays [year, age] for one sex.
arr <- function(d, value, sex) {
  d <- d[d$sex == sex, ]
  m <- matrix(NA_real_, length(1937:2100), length(ages), dimnames = list(1937:2100, ages))
  m[cbind(match(d$year, 1937:2100), match(d$age, ages))] <- d[[value]]
  m
}
qc_by_year <- setNames(inp$qc_amount$qc_amount, inp$qc_amount$year)

cohort_inputs <- function(cohort, sex) {
  p_arr <- arr(inp$covered_rate, "rate", sex)
  m_arr <- arr(inp$median_earnings, "median", sex)
  yrs <- cohort + ages
  idx <- cbind(match(yrs, 1937:2100), seq_along(ages))
  list(p = p_arr[idx],
       qc_ratio = qc_by_year[as.character(yrs)] / m_arr[idx])
}

run_cohorts <- function(cohorts, sex, rho, N) {
  p_arr <- arr(inp$covered_rate, "rate", sex)
  m_arr <- arr(inp$median_earnings, "median", sex)
  bind_rows(lapply(cohorts, function(c) {
    yrs <- c + ages
    idx <- cbind(match(yrs, 1937:2100), seq_along(ages))
    res <- simulate_cohort(c, p_arr[idx], qc_by_year[as.character(yrs)] / m_arr[idx],
                           inp$frac_points, inp$frac_low_power, N = N, rho = rho)
    res$cohort <- c
    res
  })) |>
    mutate(sex = factor(sex, levels = c("M", "F")), year = cohort + age)
}

# Extend each cohort past 84 with its age-84 value, and keep years 1970-2100.
to_year_age <- function(sim) {
  old <- sim |> filter(age == 84) |> select(cohort, sex, fully84 = fully)
  ext <- expand_grid(old, age = 85:100) |>
    mutate(year = cohort + age, fully = fully84, disability = NA_real_) |>
    select(-fully84)
  bind_rows(sim |> select(cohort, sex, age, year, fully, disability), ext) |>
    filter(year >= 1970, year <= 2100)
}

# Aggregate simulated rates to 4.C2's age groups, weighting by Dec 31 population.
# Ages 0-12 are never insured but count in 4.C2's "under 20" denominator.
# Disability-insured status is only counted below age 65 here: 4.C2 counts it
# only below full retirement age, so its 65-69 group isn't comparable.
to_groups <- function(rates) {
  g <- inp$target_groups
  kids <- expand_grid(year = unique(rates$year), age = 0:12,
                      sex = factor(c("M", "F"), levels = c("M", "F")),
                      fully = 0, disability = 0)
  bind_rows(rates, kids) |>
    mutate(disability = ifelse(age >= 65, NA, disability)) |>
    inner_join(pop_dec |> group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop"),
               by = c("year", "sex", "age")) |>
    inner_join(g, by = join_by(between(age, lo, hi))) |>
    group_by(year, sex, group) |>
    summarise(fully = sum(fully * pop) / sum(pop),
              disability = sum(disability * pop, na.rm = TRUE) /
                sum(pop[!is.na(disability)]),
              .groups = "drop") |>
    pivot_longer(c(fully, disability), names_to = "status", values_to = "sim") |>
    filter(!is.na(sim))
}

fit_groups <- c("25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59",
                "60_64", "65_69", "70_74")
score <- function(rates) {
  to_groups(rates) |>
    inner_join(inp$targets, by = c("year", "sex", "group", "status")) |>
    filter(group %in% fit_groups, year >= 1990) |>
    summarise(rmse_fully = sqrt(mean((sim - rate)[status == "fully"]^2)),
              rmse_disab = sqrt(mean((sim - rate)[status == "disability"]^2)),
              bias_fully = mean((sim - rate)[status == "fully"]))
}

# ---- Step 1: calibrate rho by sex ------------------------------------------
# Cohorts whose ages 25-74 fall in 1990-2025, at N = 5,000 for speed.

calib_cohorts <- 1916:2000
grid <- c(0.6, 0.75, 0.85, 0.9, 0.95, 0.98)
calib <- expand_grid(sex = c("M", "F"), rho = grid) |>
  rowwise() |>
  mutate(score = list(score(to_year_age(run_cohorts(calib_cohorts, sex, rho, N = 5000))))) |>
  unnest(score) |>
  ungroup()

cat("Calibration (fit to 4.C2, ages 25-74, 1990-2025):\n")
print(calib |> mutate(across(where(is.double), ~ round(.x, 4))), n = Inf)
best <- calib |> group_by(sex) |> slice_min(rmse_fully, n = 1) |> ungroup()
cat("\nChosen rho: men", best$rho[best$sex == "M"], "| women", best$rho[best$sex == "F"], "\n")

# ---- Step 2: full run --------------------------------------------------------
cohorts <- first_cohort:last_cohort
sim <- bind_rows(
  run_cohorts(cohorts, "M", best$rho[best$sex == "M"], N = 30000),
  run_cohorts(cohorts, "F", best$rho[best$sex == "F"], N = 30000)
)
insured_rates <- to_year_age(sim) |> select(year, age, sex, fully, disability) |>
  arrange(sex, year, age)

# ---- Step 3: compare with Supplement 4.C2 -----------------------------------
cmp <- to_groups(insured_rates) |>
  inner_join(inp$targets |> select(year, status, sex, group, rate),
             by = c("year", "sex", "group", "status")) |>
  mutate(gap = sim - rate)

cat("\nFully insured, simulated vs 4.C2, 2025:\n")
print(cmp |> filter(year == 2025, status == "fully") |>
        select(sex, group, sim, rate, gap) |>
        mutate(across(where(is.double), ~ round(.x, 3))), n = Inf)
cat("\nDisability insured, simulated vs 4.C2, 2025:\n")
print(cmp |> filter(year == 2025, status == "disability", !is.na(rate)) |>
        select(sex, group, sim, rate, gap) |>
        mutate(across(where(is.double), ~ round(.x, 3))), n = Inf)
cat("\nMean gap by decade, ages 25-74 (simulated minus 4.C2):\n")
print(cmp |> filter(group %in% fit_groups) |>
        mutate(decade = 10 * (year %/% 10)) |>
        group_by(status, sex, decade) |> summarise(gap = round(mean(gap), 3), .groups = "drop") |>
        pivot_wider(names_from = decade, values_from = gap))

dir.create("data", showWarnings = FALSE)
saveRDS(insured_rates, "data/insured_rates.rds")
saveRDS(list(calibration = calib, chosen = best, comparison = cmp), "data/insured_fit.rds")
cat("\nSaved data/insured_rates.rds:", nrow(insured_rates), "rows\n")
