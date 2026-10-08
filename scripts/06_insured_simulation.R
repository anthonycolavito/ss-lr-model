# 06_insured_simulation.R
#
# Simulates work histories by birth cohort and sex to estimate the share of the
# population that is fully insured and disability insured, by age, sex and
# year, 1970-2100 (methodology section 3.1), using OCACT's method of choosing
# non-covered workers (SLCT/SRCH search; see R/insured_sim.R).
#
#   Step 1  calibrate SLCT and SRCH for men, then women, to Supplement 4.C2
#   Step 2  run every cohort at 30,000 records
#   Step 3  compare with 4.C2
#   Step 4  compare with our first version (latent-attachment method), with
#           and without the Study 127 teen work rates, to see what each change
#           moved
#
# Not yet included (to revisit):
#   - immigrants: new arrivals should enter with no prior earnings, as OCACT
#     models them. Net immigration by age is ready (script 05); waiting on
#     OCACT's 2010 study of the unauthorized population to split it by status.
#   - workers on the disability rolls lose recent work and fail the disability
#     insured test; OCACT adds them back (DINADD). Phase 2.
#   - insured rates for ages 85+ hold each cohort's age-84 value (as OCACT does)
#
# Input:  data/insured_inputs.rds (script 04), data/population_dec.rds
# Output: data/insured_rates.rds, data/insured_fit.rds,
#         outputs/insured_method_comparison.csv

library(dplyr)
library(tidyr)
source("R/insured_sim.R")

inp <- readRDS("data/insured_inputs.rds")
pop_dec <- readRDS("data/population_dec.rds")
set.seed(20261008)

ages <- 13:84
cohorts <- 1870:2087          # aged 100 in 1970 through aged 13 in 2100
calib_cohorts <- seq(1916, 2000, by = 2)   # ages 25-74 overlap 1990-2025

# ---- Inputs by cohort --------------------------------------------------------
arr <- function(d, value, sex) {
  d <- d[d$sex == sex, ]
  m <- matrix(NA_real_, length(1937:2100), length(ages), dimnames = list(1937:2100, ages))
  m[cbind(match(d$year, 1937:2100), match(d$age, ages))] <- d[[value]]
  m
}
qc_by_year <- setNames(inp$qc_amount$qc_amount, inp$qc_amount$year)
p_arr <- list(M = arr(inp$covered_rate, "rate", "M"), F = arr(inp$covered_rate, "rate", "F"))
m_arr <- list(M = arr(inp$median_earnings, "median", "M"), F = arr(inp$median_earnings, "median", "F"))

cohort_inputs <- function(cohort, sex) {
  yrs <- cohort + ages
  idx <- cbind(match(yrs, 1937:2100), seq_along(ages))
  list(p = p_arr[[sex]][idx], p_m = p_arr$M[idx],
       qc_ratio = qc_by_year[as.character(yrs)] / m_arr[[sex]][idx])
}

# Run a set of cohorts for one sex. `method` is "ocact" (with m_params and,
# for women, f_params) or "latent" (with rho).
run_cohorts <- function(cohorts, sex, N, method = "ocact",
                        m_params = NULL, f_params = NULL, rho = NULL) {
  bind_rows(lapply(cohorts, function(c) {
    x <- cohort_inputs(c, sex)
    res <- if (method == "ocact") {
      pa <- if (sex == "M") m_params else grade_female_params(f_params, m_params, x$p, x$p_m)
      simulate_cohort_ocact(c, x$p, x$qc_ratio, inp$frac_points, inp$frac_low_power,
                            pa$slct, pa$srch, N = N)
    } else {
      simulate_cohort_latent(c, x$p, x$qc_ratio, inp$frac_points, inp$frac_low_power,
                             N = N, rho = rho)
    }
    res$cohort <- c
    res
  })) |>
    mutate(sex = factor(sex, levels = c("M", "F")), year = cohort + age)
}

# Extend each cohort past 84 with its age-84 value; keep years 1970-2100.
to_year_age <- function(sim) {
  old <- sim |> filter(age == 84) |> select(cohort, sex, fully84 = fully)
  ext <- expand_grid(old, age = 85:100) |>
    mutate(year = cohort + age, fully = fully84, disability = NA_real_) |>
    select(-fully84)
  bind_rows(sim |> select(cohort, sex, age, year, fully, disability), ext) |>
    filter(year >= 1970, year <= 2100)
}

# Aggregate to 4.C2's age groups, weighting by Dec 31 population. Ages 0-12
# are never insured but count in 4.C2's "under 20" denominator. Disability
# insured is compared below age 65 only: 4.C2 counts it only below full
# retirement age, so its 65-69 group isn't comparable.
pop_sa <- pop_dec |> group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop")
to_groups <- function(rates) {
  kids <- expand_grid(year = unique(rates$year), age = 0:12,
                      sex = factor(c("M", "F"), levels = c("M", "F")),
                      fully = 0, disability = 0)
  bind_rows(rates, kids) |>
    mutate(disability = ifelse(age >= 65, NA, disability)) |>
    inner_join(pop_sa, by = c("year", "sex", "age")) |>
    inner_join(inp$target_groups, by = join_by(between(age, lo, hi))) |>
    group_by(year, sex, group) |>
    summarise(fully = sum(fully * pop) / sum(pop),
              disability = sum(disability * pop, na.rm = TRUE) / sum(pop[!is.na(disability)]),
              .groups = "drop") |>
    pivot_longer(c(fully, disability), names_to = "status", values_to = "sim") |>
    filter(!is.na(sim))
}

fit_groups <- c("25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59",
                "60_64", "65_69", "70_74")
score <- function(rates, sex_keep) {
  to_groups(rates) |>
    filter(sex == sex_keep) |>
    inner_join(inp$targets, by = c("year", "sex", "group", "status")) |>
    filter(group %in% fit_groups, year >= 1990) |>
    summarise(rmse_fully = sqrt(mean((sim - rate)[status == "fully"]^2)),
              bias_fully = mean((sim - rate)[status == "fully"]),
              rmse_disab = sqrt(mean((sim - rate)[status == "disability"]^2)),
              bias_disab = mean((sim - rate)[status == "disability"]))
}

# ---- Step 1: calibrate SLCT and SRCH -----------------------------------------
# Grid over SLCT (consecutive zero-QC years that qualify a record to be chosen
# as a non-worker) and SRCH (records examined per pick) for ages 25+, with ages
# 18-24 one year lower on SLCT and ages 13-17 fixed at SLCT 1, SRCH 3. Fit on
# fully insured, ages 25-74, 1990-2025, at N = 5,000 on alternate cohorts.
# Men first; women's values are graded toward the men's as women's covered
# rates approach men's, so the women's fit uses the men's result.

grid <- expand_grid(slct = c(2, 3, 4, 5, 6, 8), srch = c(20, 100, 500, 2000))

calibrate <- function(sex, m_params = NULL) {
  grid |>
    rowwise() |>
    mutate(score = list({
      pa <- age_params(slct, srch)
      sim <- if (sex == "M") run_cohorts(calib_cohorts, "M", 5000, m_params = pa)
             else run_cohorts(calib_cohorts, "F", 5000, m_params = m_params, f_params = pa)
      score(to_year_age(sim), sex)
    })) |>
    unnest(score) |> ungroup() |> mutate(sex = sex)
}

t0 <- Sys.time()
calib_m <- calibrate("M")
best_m <- calib_m |> slice_min(rmse_fully, n = 1, with_ties = FALSE)
m_params <- age_params(best_m$slct, best_m$srch)
calib_f <- calibrate("F", m_params)
best_f <- calib_f |> slice_min(rmse_fully, n = 1, with_ties = FALSE)
f_params <- age_params(best_f$slct, best_f$srch)
cat("Calibration took", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "minutes\n")

cat("\nCalibration grid (fully insured fit, ages 25-74, 1990-2025):\n")
print(bind_rows(calib_m, calib_f) |> select(sex, slct, srch, rmse_fully, bias_fully, rmse_disab) |>
        mutate(across(where(is.double), ~ round(.x, 4))), n = Inf)
cat("\nChosen: men SLCT", best_m$slct, "SRCH", best_m$srch,
    "| women SLCT", best_f$slct, "SRCH", best_f$srch, "(graded toward men's)\n")

# ---- Step 2: full run --------------------------------------------------------
sim <- bind_rows(
  run_cohorts(cohorts, "M", 30000, m_params = m_params),
  run_cohorts(cohorts, "F", 30000, m_params = m_params, f_params = f_params)
)
insured_rates <- to_year_age(sim) |> select(year, age, sex, fully, disability) |>
  arrange(sex, year, age)

# ---- Step 3: compare with Supplement 4.C2 ------------------------------------
compare <- function(rates) {
  to_groups(rates) |>
    inner_join(inp$targets |> select(year, status, sex, group, rate),
               by = c("year", "sex", "group", "status")) |>
    mutate(gap = sim - rate)
}
cmp <- compare(insured_rates)

cat("\nSimulated vs 4.C2, 2025 (share of population):\n")
print(cmp |> filter(year == 2025) |>
        select(status, sex, group, sim, rate, gap) |>
        mutate(across(where(is.double), ~ round(.x, 3))) |>
        arrange(desc(status), sex, group), n = Inf)
cat("\nMean gap by decade, ages 25-74:\n")
print(cmp |> filter(group %in% fit_groups) |>
        mutate(decade = 10 * (year %/% 10)) |>
        group_by(status, sex, decade) |> summarise(gap = round(mean(gap), 3), .groups = "drop") |>
        pivot_wider(names_from = decade, values_from = gap))

# ---- Step 4: compare methods -------------------------------------------------
#   v1  latent method, assumed teen weights     (saved from the first run)
#   v2  latent method, Study 127 teen weights   (same rho as v1: 0.90 / 0.85)
#   v3  OCACT method, Study 127 teen weights    (this run)
v1 <- readRDS("data/insured_rates_v1_latent_oldteens.rds")
v2 <- bind_rows(run_cohorts(cohorts, "M", 30000, method = "latent", rho = 0.90),
                run_cohorts(cohorts, "F", 30000, method = "latent", rho = 0.85)) |>
  to_year_age() |> select(year, age, sex, fully, disability)

summarise_fit <- function(rates, label) {
  compare(rates) |>
    filter(year >= 1990, group %in% c("20_24", fit_groups)) |>
    group_by(version = label, status, sex) |>
    summarise(rmse = sqrt(mean(gap^2)), mean_gap = mean(gap),
              worst_group_2025 = group[year == 2025][which.max(abs(gap[year == 2025]))],
              worst_gap_2025 = gap[year == 2025][which.max(abs(gap[year == 2025]))],
              .groups = "drop")
}
method_cmp <- bind_rows(
  summarise_fit(v1, "v1 latent, assumed teens"),
  summarise_fit(v2, "v2 latent, Study 127 teens"),
  summarise_fit(insured_rates, "v3 OCACT SLCT/SRCH, Study 127 teens")
) |> mutate(across(where(is.double), ~ round(.x, 4)))

cat("\nFit to 4.C2 by version (ages 20-74, 1990-2025; disability below 65):\n")
print(method_cmp, n = Inf)

by_group_2025 <- bind_rows(
  compare(v1) |> mutate(version = "v1"),
  compare(v2) |> mutate(version = "v2"),
  cmp |> mutate(version = "v3")
) |>
  filter(year == 2025) |>
  select(version, status, sex, group, sim, rate) |>
  pivot_wider(names_from = version, values_from = sim) |>
  mutate(across(where(is.double), ~ round(.x, 3))) |>
  arrange(desc(status), sex, group)
cat("\n2025 by age group, all versions:\n")
print(by_group_2025, n = Inf)

dir.create("outputs", showWarnings = FALSE)
write.csv(method_cmp, "outputs/insured_method_comparison.csv", row.names = FALSE)
write.csv(by_group_2025, "outputs/insured_method_comparison_2025_by_group.csv", row.names = FALSE)

saveRDS(insured_rates, "data/insured_rates.rds")
saveRDS(list(calibration = bind_rows(calib_m, calib_f),
             params = list(M = m_params, F = f_params,
                           chosen = bind_rows(best_m, best_f)),
             comparison = cmp, methods = method_cmp),
        "data/insured_fit.rds")
cat("\nSaved data/insured_rates.rds:", nrow(insured_rates), "rows\n")
