# Insured-status simulation (methodology section 3.1).
#
# For one birth cohort and sex, simulate N work histories from age 13 to 84.
# Each year a record either has covered earnings or not; if covered it earns
# 0-4 quarters of coverage (QCs) depending on where its earnings fall against
# the QC amount. From the accumulated QCs we read off, at each age, whether the
# record is fully insured and disability insured.
#
# Two ways of choosing who works each year:
#
#   "ocact"   OCACT's method. The number of non-covered records is set by the
#             covered-worker rate; they are chosen by a search that favors
#             records with at least SLCT consecutive prior years without QCs,
#             looking at most SRCH records per pick (src/insured_select.cpp).
#             Covered records then draw QCs independently each year from the
#             earnings distribution.
#
#   "latent"  Our first version, kept for comparison. Each record has a fixed
#             attachment to covered work plus yearly noise (one persistence
#             parameter, rho), and a persistent earnings rank.

Rcpp::sourceCpp("src/insured_select.cpp")

#' Cumulative share of covered workers earning below `ratio` x median.
frac_cdf <- function(ratio, frac_points, low_power) {
  r1 <- frac_points$ratio[1]; f1 <- frac_points$cum_share[1]
  out <- approx(log(frac_points$ratio), frac_points$cum_share, xout = log(pmax(ratio, 1e-9)),
                rule = 2)$y
  low <- ratio < r1
  out[low] <- f1 * (ratio[low] / r1)^low_power
  out
}

#' QCs needed to be fully insured at the end of calendar year t.
#' Elapsed years = years after 1950 (or the year of turning 21, if later) and
#' before the year of turning 62 (or year t, if earlier); need 6 to 40 QCs.
qcs_needed_fully <- function(cohort, t) {
  elapsed <- pmin(t, cohort + 62) - pmax(1950, cohort + 21) - 1
  pmin(40, pmax(6, elapsed))
}

#' Prepare a cohort's year-by-year inputs: zero out years without data (before
#' 1937 or after 2100) and turn QC-to-median ratios into QC thresholds.
cohort_setup <- function(p, qc_ratio, frac_points, low_power) {
  outside <- is.na(p) | is.na(qc_ratio)
  p[outside] <- 0
  qc_ratio[outside] <- 1
  # thr[j, n]: share of covered workers earning less than n QCs' worth at age j
  thr <- sapply(1:4, function(n) frac_cdf(n * qc_ratio, frac_points, low_power))
  list(p = p, thr = thr)
}

#' Fully and disability insured shares by age, from a records x ages QC matrix.
#'
#' Disability insured = fully insured and recent work:
#'   age 31+   20 QCs in the last 10 years (40 quarters)
#'   24-30     QCs since age 21 >= half the quarters elapsed (at least 6)
#'   under 24  6 QCs in the last 3 years
insured_status <- function(qcs, cohort, ages = 13:84) {
  N <- nrow(qcs); A <- length(ages)
  cum <- t(apply(qcs, 1, cumsum))
  need <- qcs_needed_fully(cohort, cohort + ages)
  fully <- sweep(cum, 2, need, ">=")

  roll <- function(k) {
    out <- cum
    if (k < A) out[, (k + 1):A] <- cum[, (k + 1):A] - cum[, 1:(A - k)]
    out
  }
  last10 <- roll(10); last3 <- roll(3)
  since21 <- cum - cum[, which(ages == 21)] * matrix(rep(ages >= 21, each = N), N)
  half_needed <- pmax(6, 2 * (ages - 21))
  recent <- matrix(FALSE, N, A)
  for (j in seq_len(A)) {
    a <- ages[j]
    recent[, j] <- if (a >= 31) last10[, j] >= 20 else if (a >= 24) since21[, j] >= half_needed[j] else last3[, j] >= 6
  }
  disab <- fully & recent

  data.frame(age = ages,
             fully = colMeans(fully),
             disability = ifelse(ages <= 69, colMeans(disab), NA),
             mean_qc = colMeans(qcs),
             share_covered = colMeans(qcs > 0))
}

#' OCACT method: simulate one cohort and sex.
#'
#' @param cohort    Birth year.
#' @param p         Covered-worker rates at ages 13..84 (NA outside 1937-2100).
#' @param qc_ratio  QC amount / median earnings at ages 13..84.
#' @param slct,srch SLCT and SRCH at ages 13..84 (integer vectors).
#' @param N         Number of records (OCACT uses 30,000).
simulate_cohort_ocact <- function(cohort, p, qc_ratio, frac_points, low_power,
                                  slct, srch, N = 30000) {
  ages <- 13:84; A <- length(ages)
  s <- cohort_setup(p, qc_ratio, frac_points, low_power)
  qcs <- matrix(0L, N, A)
  zero_run <- integer(N)
  for (j in seq_len(A)) {
    n_out <- round((1 - s$p[j]) * N)
    out <- select_noncovered(zero_run, n_out, slct[j], srch[j])
    u <- runif(N)
    q <- (u >= s$thr[j, 1]) + (u >= s$thr[j, 2]) + (u >= s$thr[j, 3]) + (u >= s$thr[j, 4])
    q[out] <- 0L
    qcs[, j] <- q
    zero_run <- ifelse(q == 0L, zero_run + 1L, 0L)
  }
  insured_status(qcs, cohort, ages)
}

#' Latent-attachment method (first version): simulate one cohort and sex.
simulate_cohort_latent <- function(cohort, p, qc_ratio, frac_points, low_power,
                                   N = 30000, rho = 0.8, rho_e = 0.8) {
  ages <- 13:84; A <- length(ages)
  s <- cohort_setup(p, qc_ratio, frac_points, low_power)
  eta <- rnorm(N); zeta <- rnorm(N)
  qcs <- matrix(0L, N, A)
  for (j in seq_len(A)) {
    if (s$p[j] <= 0) next
    works <- rho * eta + sqrt(1 - rho^2) * rnorm(N) < qnorm(s$p[j])
    u <- pnorm(rho_e * zeta + sqrt(1 - rho_e^2) * rnorm(N))
    qcs[, j] <- works * ((u >= s$thr[j, 1]) + (u >= s$thr[j, 2]) + (u >= s$thr[j, 3]) + (u >= s$thr[j, 4]))
  }
  insured_status(qcs, cohort, ages)
}

#' SLCT and SRCH by age (13..84) from a small set of band values.
#'
#' The methodology says SRCH is generally lower at younger ages and SLCT is
#' lower for the very young, but publishes no values. We use three bands:
#'   13-17   SLCT 1, SRCH = young_srch (teen work is sporadic)
#'   18-24   SLCT = slct_mid, SRCH = srch
#'   25-84   SLCT = slct, SRCH = srch
age_params <- function(slct, srch, slct_mid = max(1, slct - 1), young_srch = 3) {
  ages <- 13:84
  list(slct = as.integer(ifelse(ages <= 17, 1, ifelse(ages <= 24, slct_mid, slct))),
       srch = as.integer(ifelse(ages <= 17, young_srch, srch)))
}

#' Grade women's SLCT/SRCH toward men's as women's covered-worker rate
#' approaches men's (methodology 3.1.c, footnote 2): women's own values below
#' 90% of the men's rate, men's values at or above 100%, linear in between.
grade_female_params <- function(f_params, m_params, p_f, p_m) {
  w <- pmin(1, pmax(0, (p_f / pmax(p_m, 1e-9) - 0.9) / 0.1))
  w[is.na(w)] <- 0
  list(slct = as.integer(round((1 - w) * f_params$slct + w * m_params$slct)),
       srch = as.integer(round((1 - w) * f_params$srch + w * m_params$srch)))
}
