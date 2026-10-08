# Insured-status simulation (methodology section 3.1, simplified).
#
# For one birth cohort and sex, simulate N work histories from age 13 to 84.
# Each year a record either has covered earnings or not, and if covered earns
# 0-4 quarters of coverage (QCs) depending on where its earnings fall relative
# to the QC amount. From the accumulated QCs we read off, at each age, whether
# the record is fully insured and disability insured.
#
# Who works: record i is a covered worker at age a if
#     rho * eta_i + sqrt(1 - rho^2) * e_ia  <  qnorm(p_a)
# where eta_i is a fixed "attachment to covered work", e_ia is yearly noise,
# and p_a is the covered-worker rate for that age, sex and year. This hits p_a
# exactly in expectation; rho sets how much the same people work every year.
# rho plays the role of OCACT's SLCT/SRCH parameters and is calibrated to the
# historical insured rates in Supplement 4.C2.
#
# How many QCs: a covered worker's earnings percentile u (persistent, with
# correlation rho_e) maps to earnings through the distribution relative to the
# median; QCs = number of n in 1..4 with earnings >= n x QC amount.

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

#' Simulate one cohort and sex.
#'
#' @param cohort   Birth year.
#' @param p        Covered-worker rates at ages 13..84 for this cohort (vector, NA
#'                 or 0 where the year is before 1937).
#' @param qc_ratio QC amount / median earnings at ages 13..84 (vector).
#' @param N        Number of records.
#' @param rho      Persistence of covered work (0 = independent each year).
#' @param rho_e    Persistence of earnings rank among covered workers.
#' @return Data frame: age, fully (share fully insured), disability (share
#'   disability insured; ages 13-69), mean_qc.
simulate_cohort <- function(cohort, p, qc_ratio, frac_points, low_power,
                            N = 30000, rho = 0.8, rho_e = 0.8) {
  ages <- 13:84
  A <- length(ages)
  # Years outside 1937-2100 have no data; nobody has covered work then.
  outside <- is.na(p) | is.na(qc_ratio)
  p[outside] <- 0
  qc_ratio[outside] <- 1

  # Thresholds: share of covered workers earning fewer than n QCs' worth.
  thr <- sapply(1:4, function(n) frac_cdf(n * qc_ratio, frac_points, low_power))  # A x 4

  eta  <- rnorm(N)
  zeta <- rnorm(N)
  qcs <- matrix(0L, N, A)
  for (j in seq_len(A)) {
    if (p[j] <= 0) next
    works <- rho * eta + sqrt(1 - rho^2) * rnorm(N) < qnorm(p[j])
    u <- pnorm(rho_e * zeta + sqrt(1 - rho_e^2) * rnorm(N))
    qcs[, j] <- works * ((u >= thr[j, 1]) + (u >= thr[j, 2]) + (u >= thr[j, 3]) + (u >= thr[j, 4]))
  }

  cum <- t(apply(qcs, 1, cumsum))                        # QCs to date, N x A
  years <- cohort + ages
  need <- qcs_needed_fully(cohort, years)
  fully <- sweep(cum, 2, need, ">=")

  # Disability insured: fully insured and recent work.
  #   age 31+: 20 QCs in the last 10 years (40 quarters)
  #   24-30:   QCs since age 21 >= half the quarters elapsed (at least 6)
  #   under 24: 6 QCs in the last 3 years
  roll <- function(k) {                                  # QCs in last k years
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
             mean_qc = colMeans(qcs))
}
