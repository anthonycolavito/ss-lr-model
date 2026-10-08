# 06_insured_simulation.R
#
# Simulates work histories by birth cohort and sex to estimate the share of the
# Social Security area population that is fully insured and disability insured,
# by age, sex and year, 1970-2100 (methodology section 3.1), following OCACT:
#
#   - records represent the work-authorized population; non-covered workers are
#     chosen each year by the SLCT/SRCH search (src/insured_select.cpp)
#   - each year a share of records stands for new net LPR immigrants: prior
#     earnings wiped, 0-4 QCs that year (data/immigration_status.rds)
#   - the temporary or unlawfully present population is outside the
#     simulation; combined as  sim x (L + 0.75 k U) / (L + U)
#
# Calibration
#   Stage 1  SLCT and SRCH (ages 25+; 18-24 one lower on SLCT; 13-17 fixed) to
#            Supplement 4.C2 fully insured rates, ages 25-74, 1990-2025
#   Stage 2  k by sex (covered-worker rate of the temporary or unlawfully
#            present relative to everyone else) to the TR's fully insured
#            share at age 62: men 92.6% (2025) and 88.4% (2100), women 88.5%
#            and 87.7% (2026 TR, Program Assumptions, section V.C.3)
#
# Then a full run, a check against 4.C2 and the TR, and a comparison with our
# earlier latent-attachment versions.
#
# Still to do: workers on the disability rolls fail the recent-work test but
# keep disability insured status; OCACT adds them back (DINADD). Phase 2.
#
# Input:  data/insured_inputs.rds (04), data/immigration_status.rds (05),
#         data/population_dec.rds (01)
# Output: data/insured_rates.rds, data/insured_fit.rds,
#         outputs/insured_method_comparison.csv,
#         outputs/insured_method_comparison_2025_by_group.csv

library(dplyr)
library(tidyr)
source("R/insured_sim.R")

inp <- readRDS("data/insured_inputs.rds")
imm <- readRDS("data/immigration_status.rds")
pop_dec <- readRDS("data/population_dec.rds")
set.seed(20261008)

ages <- 13:84
cohorts <- 1870:2087                       # aged 100 in 1970 .. aged 13 in 2100
calib_cohorts <- seq(1916, 2000, by = 2)   # ages 25-74 overlap 1990-2025
tr_age62 <- tibble(sex = factor(c("M", "M", "F", "F"), levels = c("M", "F")),
                   year = c(2025, 2100, 2025, 2100),
                   target = c(0.926, 0.884, 0.885, 0.877))

# ---- Arrays by [year 1937-2100, age 0-100] for each sex ------------------------
yrs_all <- 1937:2100
to_arr <- function(d, value, sex, age_range = 0:100) {
  d <- d[d$sex == sex & d$year %in% yrs_all & d$age %in% age_range, ]
  m <- matrix(0, length(yrs_all), 101, dimnames = list(yrs_all, 0:100))
  m[cbind(match(d$year, yrs_all), d$age + 1)] <- d[[value]]
  m
}
pop_sa <- pop_dec |> group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop")
P <- list(M = to_arr(pop_sa, "pop", "M"), F = to_arr(pop_sa, "pop", "F"))
U <- list(M = to_arr(imm$tu_stock, "stock", "M"), F = to_arr(imm$tu_stock, "stock", "F"))
E <- list(M = to_arr(imm$lpr_entrants, "entrants", "M"), F = to_arr(imm$lpr_entrants, "entrants", "F"))
for (s in c("M", "F")) {                       # 1937-40: population starts 1940
  P[[s]][as.character(1937:1939), ] <- matrix(P[[s]]["1940", ], 3, 101, byrow = TRUE)
  U[[s]] <- pmin(U[[s]], 0.95 * P[[s]])
}
cov_arr <- list(M = to_arr(inp$covered_rate, "rate", "M", ages),
                F = to_arr(inp$covered_rate, "rate", "F", ages))
med_arr <- list(M = to_arr(inp$median_earnings, "median", "M", ages),
                F = to_arr(inp$median_earnings, "median", "F", ages))
qc_by_year <- setNames(inp$qc_amount$qc_amount, inp$qc_amount$year)

cohort_inputs <- function(cohort, sex, k) {
  yr <- cohort + ages
  ok <- yr %in% yrs_all
  idx <- cbind(match(yr, yrs_all), ages + 1)
  get <- function(m) { v <- rep(NA_real_, length(ages)); v[ok] <- m[idx[ok, , drop = FALSE]]; v }
  p_all <- get(cov_arr[[sex]]); Pv <- get(P[[sex]]); Uv <- get(U[[sex]]); Ev <- get(E[[sex]])
  Lv <- Pv - Uv
  # Covered workers = p x P = p_leg x (L + k U): the work-authorized rate.
  p_leg <- pmin(0.995, p_all * Pv / (Lv + k * Uv))
  p_m <- { v <- get(cov_arr$M); Pm <- get(P$M); Um <- get(U$M); pmin(0.995, v * Pm / (Pm - Um + k * Um)) }
  list(p = p_leg, p_m = p_m,
       qc_ratio = qc_by_year[as.character(yr)] / get(med_arr[[sex]]),
       imm_frac = ifelse(Lv > 0, Ev / Lv, 0))
}

run_cohorts <- function(cohorts, sex, N, m_params, f_params = NULL, k,
                        immigrants = TRUE) {
  bind_rows(lapply(cohorts, function(c) {
    x <- cohort_inputs(c, sex, if (immigrants) k else 0)
    pa <- if (sex == "M") m_params else grade_female_params(f_params, m_params, x$p, x$p_m)
    res <- simulate_cohort_ocact(c, x$p, x$qc_ratio, inp$frac_points, inp$frac_low_power,
                                 pa$slct, pa$srch,
                                 imm_frac = if (immigrants) x$imm_frac else 0, N = N)
    res$cohort <- c
    res
  })) |>
    mutate(sex = factor(sex, levels = c("M", "F")), year = cohort + age)
}

# Simulated (work-authorized) shares -> shares of everyone, by year and age,
# 1970-2100. Ages 85-100 hold each cohort's age-84 simulated share.
to_year_age <- function(sim, k_by_sex) {
  old <- sim |> filter(age == 84) |> select(cohort, sex, f84 = fully)
  ext <- expand_grid(old, age = 85:100) |>
    mutate(year = cohort + age, fully = f84, disability = NA_real_) |> select(-f84)
  out <- bind_rows(sim |> select(cohort, sex, age, year, fully, disability), ext) |>
    filter(year >= 1970, year <= 2100)
  idx <- cbind(match(out$year, yrs_all), out$age + 1)
  Pv <- ifelse(out$sex == "M", P$M[idx], P$F[idx])
  Uv <- ifelse(out$sex == "M", U$M[idx], U$F[idx])
  kv <- k_by_sex[as.character(out$sex)]
  out |> mutate(fully = combine_status(fully, Pv - Uv, Uv, kv),
                disability = combine_status(disability, Pv - Uv, Uv, kv))
}

# Aggregate to 4.C2 age groups, weighting by Dec 31 population. Ages 0-12 count
# in 4.C2's "under 20" denominator. Disability insured is compared below 65.
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
compare <- function(rates) {
  to_groups(rates) |>
    inner_join(inp$targets |> select(year, status, sex, group, rate),
               by = c("year", "sex", "group", "status")) |>
    mutate(gap = sim - rate)
}
score_hist <- function(rates, sex_keep) {
  compare(rates) |>
    filter(sex == sex_keep, group %in% fit_groups, year >= 1990) |>
    summarise(rmse_fully = sqrt(mean(gap[status == "fully"]^2)),
              bias_fully = mean(gap[status == "fully"]),
              rmse_disab = sqrt(mean(gap[status == "disability"]^2)))
}
age62 <- function(rates) {
  rates |> filter(age == 62, year %in% c(2025, 2100)) |>
    select(sex, year, fully) |>
    inner_join(tr_age62, by = c("sex", "year")) |>
    mutate(gap = fully - target)
}

# ---- Stage 1: SLCT and SRCH --------------------------------------------------
k0 <- c(M = 0.5, F = 0.5)    # provisional; revisited in stage 2
grid <- expand_grid(slct = c(2, 3, 4, 5, 6, 8), srch = c(20, 100, 500, 2000))
calibrate <- function(sex, m_params = NULL) {
  grid |>
    rowwise() |>
    mutate(score = list({
      pa <- age_params(slct, srch)
      sim <- if (sex == "M") run_cohorts(calib_cohorts, "M", 5000, m_params = pa, k = k0["M"])
             else run_cohorts(calib_cohorts, "F", 5000, m_params = m_params, f_params = pa, k = k0["F"])
      score_hist(to_year_age(sim, k0), sex)
    })) |>
    unnest(score) |> ungroup() |> mutate(sex = sex)
}
# Stage 1 takes several minutes; its result is saved and reused unless the
# inputs have changed since (delete data/insured_stage1.rds to force a rerun).
stage1_file <- "data/insured_stage1.rds"
inputs_newer <- file.exists(stage1_file) &&
  any(file.mtime(c("data/insured_inputs.rds", "data/immigration_status.rds")) > file.mtime(stage1_file))
t0 <- Sys.time()
if (file.exists(stage1_file) && !inputs_newer) {
  st1 <- readRDS(stage1_file); calib_m <- st1$m; calib_f <- st1$f
  best_m <- calib_m |> slice_min(rmse_fully, n = 1, with_ties = FALSE)
  m_params <- age_params(best_m$slct, best_m$srch)
  best_f <- calib_f |> slice_min(rmse_fully, n = 1, with_ties = FALSE)
  f_params <- age_params(best_f$slct, best_f$srch)
  cat("Stage 1: reusing", stage1_file, "\n")
} else {
  calib_m <- calibrate("M")
  best_m <- calib_m |> slice_min(rmse_fully, n = 1, with_ties = FALSE)
  m_params <- age_params(best_m$slct, best_m$srch)
  calib_f <- calibrate("F", m_params)
  best_f <- calib_f |> slice_min(rmse_fully, n = 1, with_ties = FALSE)
  f_params <- age_params(best_f$slct, best_f$srch)
  saveRDS(list(m = calib_m, f = calib_f), stage1_file)
}
cat("Stage 1 took", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "minutes\n")
cat("\nStage 1 grid (fit to 4.C2 fully insured, ages 25-74, 1990-2025):\n")
print(bind_rows(calib_m, calib_f) |> select(sex, slct, srch, rmse_fully, bias_fully, rmse_disab) |>
        mutate(across(where(is.double), ~ round(.x, 4))), n = Inf)
cat("\nChosen: men SLCT", best_m$slct, "SRCH", best_m$srch,
    "| women SLCT", best_f$slct, "SRCH", best_f$srch, "\n")

# ---- Stage 2: k by sex, to the TR's age-62 fully insured shares ---------------
# Cohorts around those turning 62 in 2025 (born 1963) and 2100 (born 2038).
k_grid <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
t0 <- Sys.time()
stage2 <- expand_grid(sex = c("M", "F"), k = k_grid) |>
  rowwise() |>
  mutate(res = list({
    sx <- sex
    kk <- setNames(c(k, k), c("M", "F"))
    sim <- run_cohorts(c(1961:1965, 2036:2040), sx, 30000,
                       m_params = m_params, f_params = f_params, k = k)
    age62(to_year_age(sim, kk)) |> filter(.data$sex == sx) |> select(year, fully, target, gap)
  })) |>
  unnest(res) |> ungroup()
cat("\nStage 2 took", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "minutes\n")
print(stage2 |> mutate(across(where(is.double), ~ round(.x, 4))), n = Inf)
k_fit <- stage2 |> group_by(sex, k) |> summarise(sse = sum(gap^2), .groups = "drop") |>
  group_by(sex) |> slice_min(sse, n = 1, with_ties = FALSE)
k_by_sex <- setNames(k_fit$k, as.character(k_fit$sex))
cat("\nChosen k: men", k_by_sex["M"], "| women", k_by_sex["F"], "\n")

# ---- Full run ------------------------------------------------------------------
sim <- bind_rows(
  run_cohorts(cohorts, "M", 30000, m_params = m_params, k = k_by_sex["M"]),
  run_cohorts(cohorts, "F", 30000, m_params = m_params, f_params = f_params, k = k_by_sex["F"])
)
insured_rates <- to_year_age(sim, k_by_sex) |> select(year, age, sex, fully, disability) |>
  arrange(sex, year, age)

cmp <- compare(insured_rates)
cat("\nSimulated vs 4.C2, 2025:\n")
print(cmp |> filter(year == 2025) |> select(status, sex, group, sim, rate, gap) |>
        mutate(across(where(is.double), ~ round(.x, 3))) |> arrange(desc(status), sex, group), n = Inf)
cat("\nMean gap by decade, ages 25-74:\n")
print(cmp |> filter(group %in% fit_groups) |> mutate(decade = 10 * (year %/% 10)) |>
        group_by(status, sex, decade) |> summarise(gap = round(mean(gap), 3), .groups = "drop") |>
        pivot_wider(names_from = decade, values_from = gap))
cat("\nFully insured at 62 vs TR:\n")
print(age62(insured_rates) |> mutate(across(where(is.double), ~ round(.x, 3))))
d50 <- insured_rates |> filter(age == 50, year %in% c(2025, 2100)) |>
  inner_join(pop_sa |> filter(age == 50), by = c("year", "sex", "age")) |>
  group_by(year) |> summarise(disability = sum(disability * pop) / sum(pop))
cat("Disability insured at 50, both sexes (TR: 75.9% in 2025, 77.4% in 2100):",
    paste(round(100 * d50$disability, 1), collapse = " / "), "\n")

# ---- Compare versions ----------------------------------------------------------
#   v1  latent method, assumed teen weights, no immigrants  (first run, saved)
#   v2  latent method, Study 127 teen weights, no immigrants
#   v3  OCACT SLCT/SRCH, Study 127 teens, LPR entrants and T/U population
v1 <- readRDS("data/insured_rates_v1_latent_oldteens.rds")
lat_inputs <- function(c, sex) {
  yr <- c + ages; ok <- yr %in% yrs_all; idx <- cbind(match(yr, yrs_all), ages + 1)
  get <- function(m) { v <- rep(NA_real_, length(ages)); v[ok] <- m[idx[ok, , drop = FALSE]]; v }
  list(p = get(cov_arr[[sex]]), qc_ratio = qc_by_year[as.character(yr)] / get(med_arr[[sex]]))
}
v2 <- bind_rows(lapply(c("M", "F"), function(sex) {
  rho <- if (sex == "M") 0.90 else 0.85
  bind_rows(lapply(cohorts, function(c) {
    x <- lat_inputs(c, sex)
    r <- simulate_cohort_latent(c, x$p, x$qc_ratio, inp$frac_points, inp$frac_low_power,
                                N = 30000, rho = rho)
    r$cohort <- c; r
  })) |> mutate(sex = factor(sex, levels = c("M", "F")), year = cohort + age)
})) |> to_year_age(c(M = 1, F = 1) / 0.75) |>      # k/alpha = 1/0.75 x 0.75 -> no adjustment
  select(year, age, sex, fully, disability)

summarise_fit <- function(rates, label) {
  hist <- compare(rates) |>
    filter(year >= 1990, group %in% c("20_24", fit_groups)) |>
    group_by(status, sex) |>
    summarise(rmse_hist = sqrt(mean(gap^2)), mean_gap_hist = mean(gap), .groups = "drop")
  a62 <- age62(rates) |> select(sex, year, gap) |>
    pivot_wider(names_from = year, values_from = gap, names_prefix = "gap_age62_") |>
    mutate(status = "fully")
  hist |> left_join(a62, by = c("status", "sex")) |> mutate(version = label, .before = 1)
}
method_cmp <- bind_rows(
  summarise_fit(v1, "v1 latent, assumed teens, no immigrants"),
  summarise_fit(v2, "v2 latent, Study 127 teens, no immigrants"),
  summarise_fit(insured_rates, "v3 OCACT SLCT/SRCH, Study 127 teens, immigrants")
) |> mutate(across(where(is.double), ~ round(.x, 4)))
cat("\nVersions: fit to 4.C2 (ages 20-74, 1990-2025) and gap to TR fully insured at 62:\n")
print(method_cmp, n = Inf, width = 200)

by_group_2025 <- bind_rows(compare(v1) |> mutate(version = "v1"),
                           compare(v2) |> mutate(version = "v2"),
                           cmp |> mutate(version = "v3")) |>
  filter(year == 2025) |>
  select(version, status, sex, group, sim, rate) |>
  pivot_wider(names_from = version, values_from = sim) |>
  mutate(across(where(is.double), ~ round(.x, 3))) |>
  arrange(desc(status), sex, group)

dir.create("outputs", showWarnings = FALSE)
write.csv(method_cmp, "outputs/insured_method_comparison.csv", row.names = FALSE)
write.csv(by_group_2025, "outputs/insured_method_comparison_2025_by_group.csv", row.names = FALSE)
saveRDS(insured_rates, "data/insured_rates.rds")
saveRDS(list(stage1 = bind_rows(calib_m, calib_f), stage2 = stage2,
             params = list(M = m_params, F = f_params, k = k_by_sex,
                           chosen = bind_rows(best_m, best_f)),
             comparison = cmp, methods = method_cmp),
        "data/insured_fit.rds")
cat("\nSaved data/insured_rates.rds:", nrow(insured_rates), "rows\n")
