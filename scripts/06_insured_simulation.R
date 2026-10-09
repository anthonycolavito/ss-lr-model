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
#   SRCH varies by 4.C2 age group and sex (SLCT fixed at 4; ages 13-17 at
#   SLCT 1, SRCH 3). OCACT varies both by single age, but its values aren't
#   published and our history comes in 5-year groups, so one parameter per
#   group is what the data can pin down; SRCH is the one with range (see
#   group_params() in R/insured_sim.R).
#
#   For each candidate k (covered-worker rate of the temporary or unlawfully
#   present relative to everyone else), SRCH is fitted group by group, youngest
#   first, so the simulated fully insured rate has zero average gap to 4.C2
#   over 1990-2025. Insured status at an age depends only on work up to that
#   age, so each group's SRCH is settled before the next. After 40 QCs status
#   is permanent, so at older ages a group's own SRCH moves its rate little;
#   gaps that remain there come from earlier in those cohorts' lives and are
#   reported, not forced. Every k then matches
#   history; k is chosen by the TR's fully insured share at age 62: men 92.6%
#   (2025) and 88.4% (2100), women 88.5% and 87.7% (2026 TR, Program
#   Assumptions, section V.C.3). Men first: women's SLCT is graded toward men's
#   as women's covered-worker rate approaches men's.
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
RNGkind("L'Ecuyer-CMRG")   # reproducible random streams across cores
set.seed(20261008)
n_cores <- parallel::detectCores()

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

cohort_inputs <- function(cohort, sex, k, k_m = if (exists("k_men")) k_men else k) {
  yr <- cohort + ages
  ok <- yr %in% yrs_all
  idx <- cbind(match(yr, yrs_all), ages + 1)
  get <- function(m) { v <- rep(NA_real_, length(ages)); v[ok] <- m[idx[ok, , drop = FALSE]]; v }
  p_all <- get(cov_arr[[sex]]); Pv <- get(P[[sex]]); Uv <- get(U[[sex]]); Ev <- get(E[[sex]])
  Lv <- Pv - Uv
  # Covered workers = p x P = p_leg x (L + k U): the work-authorized rate.
  p_leg <- pmin(0.995, p_all * Pv / (Lv + k * Uv))
  # men's work-authorized rate, with the men's k (F-34: women's grading had used the women's k)
  p_m <- { v <- get(cov_arr$M); Pm <- get(P$M); Um <- get(U$M); pmin(0.995, v * Pm / (Pm - Um + k_m * Um)) }
  list(p = p_leg, p_m = p_m,
       qc_ratio = qc_by_year[as.character(yr)] / get(med_arr[[sex]]),
       imm_frac = ifelse(Lv > 0, Ev / Lv, 0))
}

run_cohorts <- function(cohorts, sex, N, m_params, f_params = NULL, k,
                        immigrants = TRUE) {
  bind_rows(lapply(cohorts, function(c) {
    x <- cohort_inputs(c, sex, if (immigrants) k else 0, if (immigrants) k_by_sex[["M"]] else 0)
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

# ---- Calibration ------------------------------------------------------------
group_ages <- tibble(
  group = c("20_24", "25_29", "30_34", "35_39", "40_44", "45_49", "50_54",
            "55_59", "60_64", "65_69", "70_74", "75plus"),
  lo = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75),
  hi = c(24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 84)
)

# Mean gap to 4.C2 (fully insured, 1990-2025) for one group, one sex.
group_gap <- function(sex, k, srch_vec, m_params, grp, N = 3000) {
  g <- group_ages[group_ages$group == grp, ]
  cs <- seq(1990 - g$hi, 2025 - g$lo, by = 2)
  pa <- group_params(srch_vec)
  set.seed(4242)                         # same random draws for every SLCT tried
  sim <- bind_rows(lapply(cs, function(c) {
    x <- cohort_inputs(c, sex, k)
    p_use <- if (sex == "M") pa else grade_female_params(pa, m_params, x$p, x$p_m)
    r <- simulate_cohort_ocact(c, x$p, x$qc_ratio, inp$frac_points, inp$frac_low_power,
                               p_use$slct, p_use$srch, imm_frac = x$imm_frac, N = N,
                               max_age = g$hi)
    r$cohort <- c; r
  })) |>
    mutate(sex = factor(sex, levels = c("M", "F")), year = cohort + age) |>
    filter(!is.na(fully))
  kk <- setNames(c(k, k), c("M", "F"))
  rates <- to_year_age(sim, kk) |> filter(age >= g$lo, age <= max(g$hi, if (grp == "75plus") 100 else g$hi))
  compare(rates) |>
    filter(sex == !!sex, group == grp, status == "fully", year >= 1990, year <= 2025) |>
    summarise(gap = mean(gap)) |> pull(gap)
}

# Fit SLCT group by group for one sex at a given k. For women, `m_params`
# carries the men's fitted values (grading).
calibrate_sex <- function(sex, k, m_params = NULL) {
  srch <- setNames(rep(100, nrow(group_ages)), group_ages$group)
  for (grp in group_ages$group) {
    idx <- which(group_ages$group == grp)
    # search on log10(SRCH) between 1 and 30,000
    f <- function(lv) { s <- srch; s[idx:length(s)] <- 10^lv; group_gap(sex, k, s, m_params, grp) }
    lo <- f(0); hi <- f(log10(30000))
    lv <- if (lo <= 0) 0 else if (hi >= 0) log10(30000) else
      uniroot(f, c(0, log10(30000)), f.lower = lo, f.upper = hi, tol = 0.02, maxiter = 12)$root
    srch[idx:length(srch)] <- 10^lv
  }
  srch
}

age62_check <- function(sex, k, slct_vec, m_params = NULL, N = 10000) {
  pa <- group_params(slct_vec)
  set.seed(777)
  sim <- bind_rows(lapply(c(1961:1965, 2036:2040), function(c) {
    x <- cohort_inputs(c, sex, k)
    p_use <- if (sex == "M") pa else grade_female_params(pa, m_params, x$p, x$p_m)
    r <- simulate_cohort_ocact(c, x$p, x$qc_ratio, inp$frac_points, inp$frac_low_power,
                               p_use$slct, p_use$srch, imm_frac = x$imm_frac, N = N,
                               max_age = 62)
    r$cohort <- c; r
  })) |> mutate(sex = factor(sex, levels = c("M", "F")), year = cohort + age) |>
    filter(!is.na(fully))
  age62(to_year_age(sim, setNames(c(k, k), c("M", "F")))) |> filter(.data$sex == !!sex)
}

calib_file <- "data/insured_calibration.rds"
inputs_newer <- file.exists(calib_file) &&
  any(file.mtime(c("data/insured_inputs.rds", "data/immigration_status.rds", "scripts/06_insured_simulation.R",
                   "R/insured_sim.R", "src/insured_select.cpp")) > file.mtime(calib_file))
k_grid <- c(0, 1/3, 2/3, 1)

if (file.exists(calib_file) && !inputs_newer) {
  cal <- readRDS(calib_file)
  cat("Calibration: reusing", calib_file, "\n")
} else {
  t0 <- Sys.time()
  fits_m <- parallel::mclapply(k_grid, mc.cores = n_cores, FUN = function(k) {
    sl <- calibrate_sex("M", k)
    list(k = k, slct = sl, age62 = age62_check("M", k, sl))
  })
  sse_m <- sapply(fits_m, function(f) sum(f$age62$gap^2))
  best_m <- fits_m[[which.min(sse_m)]]
  m_params <- group_params(best_m$slct)
  k_men <- best_m$k
  fits_f <- parallel::mclapply(k_grid, mc.cores = n_cores, FUN = function(k) {
    sl <- calibrate_sex("F", k, m_params)
    list(k = k, slct = sl, age62 = age62_check("F", k, sl, m_params))
  })
  sse_f <- sapply(fits_f, function(f) sum(f$age62$gap^2))
  best_f <- fits_f[[which.min(sse_f)]]
  cal <- list(fits_m = fits_m, fits_f = fits_f, best_m = best_m, best_f = best_f)
  saveRDS(cal, calib_file)
  cat("Calibration took", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "minutes\n")
}

show_fits <- function(fits, sex) {
  bind_rows(lapply(fits, function(f) {
    tibble(sex = sex, k = round(f$k, 2),
           gap62_2025 = round(f$age62$gap[f$age62$year == 2025], 4),
           gap62_2100 = round(f$age62$gap[f$age62$year == 2100], 4),
           !!!as.list(round(f$slct)))
  }))
}
cat("\nSRCH by age group (fitted to 4.C2; SLCT 4) and gap to TR at age 62, for each k:\n")
print(bind_rows(show_fits(cal$fits_m, "M"), show_fits(cal$fits_f, "F")), width = 250)

k_by_sex <- c(M = cal$best_m$k, F = cal$best_f$k); k_men <- cal$best_m$k
m_params <- group_params(cal$best_m$slct)
f_params <- group_params(cal$best_f$slct)
cat("\nChosen k: men", round(k_by_sex["M"], 2), "| women", round(k_by_sex["F"], 2), "\n")

# ---- Full run ------------------------------------------------------------------
# Cohorts in chunks of 10 per sex, spread over all cores; progress per chunk.
jobs <- expand_grid(sx = c("M", "F"), chunk = split(cohorts, ceiling(seq_along(cohorts) / 10)))
sim <- bind_rows(parallel::mclapply(seq_len(nrow(jobs)), mc.cores = n_cores, FUN = function(i) {
  sx <- jobs$sx[i]; cs <- jobs$chunk[[i]]
  r <- run_cohorts(cs, sx, 30000, m_params = m_params, f_params = f_params, k = k_by_sex[sx])
  message(format(Sys.time(), "%H:%M"), " ", sx, " cohorts ", min(cs), "-", max(cs), " done")
  r
}))
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
#   v1  latent method, assumed teens, no immigrants          (saved)
#   v3  OCACT SLCT/SRCH, one value for ages 25+, immigrants  (saved)
#   v5  OCACT SRCH by age group and sex, log-graded women    (this run)
# (v2, latent with Study 127 teens, matched v1 on history; see
#  outputs/insured_method_comparison_v1_v3.csv.)
# Earlier versions' rates are kept only where they were built (they aren't
# rebuilt by the current scripts); a fresh clone skips the comparison and keeps
# the committed outputs/insured_method_comparison*.csv.
have_old <- file.exists("data/insured_rates_v1_latent_oldteens.rds") && file.exists("data/insured_rates_v3_ocact_single.rds")
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
if (have_old) {
  v1 <- readRDS("data/insured_rates_v1_latent_oldteens.rds")
  v3 <- readRDS("data/insured_rates_v3_ocact_single.rds")
  method_cmp <- bind_rows(
    summarise_fit(v1, "v1 latent, assumed teens, no immigrants"),
    summarise_fit(v3, "v3 OCACT, one SLCT/SRCH for 25+, immigrants"),
    summarise_fit(insured_rates, "v5 OCACT, SRCH by age group and sex, log-graded women")
  ) |> mutate(across(where(is.double), ~ round(.x, 4)))
  cat("\nVersions: fit to 4.C2 (ages 20-74, 1990-2025) and gap to TR fully insured at 62:\n")
  print(method_cmp, n = Inf, width = 200)

  by_group_2025 <- bind_rows(compare(v1) |> mutate(version = "v1"),
                             compare(v3) |> mutate(version = "v3"),
                             cmp |> mutate(version = "v5")) |>
    filter(year == 2025) |>
    select(version, status, sex, group, sim, rate) |>
    pivot_wider(names_from = version, values_from = sim) |>
    mutate(across(where(is.double), ~ round(.x, 3))) |>
    arrange(desc(status), sex, group)
} else {
  method_cmp <- summarise_fit(insured_rates, "v5 OCACT, SRCH by age group and sex, log-graded women") |>
    mutate(across(where(is.double), ~ round(.x, 4)))
  cat("\nFit to 4.C2 (ages 20-74, 1990-2025) and gap to TR fully insured at 62:\n")
  print(method_cmp, n = Inf, width = 200)
  by_group_2025 <- NULL
}

dir.create("outputs", showWarnings = FALSE)
if (have_old) {
  write.csv(method_cmp, "outputs/insured_method_comparison.csv", row.names = FALSE)
  write.csv(by_group_2025, "outputs/insured_method_comparison_2025_by_group.csv", row.names = FALSE)
}
saveRDS(insured_rates, "data/insured_rates.rds")
saveRDS(list(calibration = cal,
             params = list(M = m_params, F = f_params, k = k_by_sex),
             comparison = cmp, methods = method_cmp),
        "data/insured_fit.rds")
cat("\nSaved data/insured_rates.rds:", nrow(insured_rates), "rows\n")
