# claim_factor.R
#
# Benefit / PIA at award for retired workers who claim at an exact age (in whole years) during a year,
# used the same way by scripts/19 (2025 base, fitted to 6.A4) and scripts/22 (projected awards), so the
# fit and the projection share one convention (F-34, PS-05).
#
#   Claims are spread evenly over the 12 months of the exact age, except at 62 (claims at the first
#   eligible month) and 70+ (the maximum delayed credit). The factor is averaged over the months, so
#   it handles the kink at 36 months and a FRA that falls inside the year of age.
#   At the age that holds the FRA, OCACT-style claims at the FRA itself (the "FRA spike") get 1; the
#   rest of that age's claims are the reduced months before the FRA (spike_share is supplied by the
#   caller: scripts/19's targets).
#
# b: birth cohort (year - age, as BEPUF and scripts/22 date them).

cf_months <- function(m, drc, nra) {          # factor for a claim m months before the FRA (m < 0: after)
  ifelse(m > 0, 1 - pmin(m, 36) * 5 / 900 - pmax(m - 36, 0) * 5 / 1200,
         1 + pmin(-m, 12 * 70 - nra) / 12 * drc)
}

claim_factor_age <- function(age, b, pc, spike_share = 0) {
  nra <- pc$nra_months[match(b, pc$birth_year)]; nra[is.na(nra)] <- 804L
  drc <- pc$drc_annual[match(b, pc$birth_year)] / 100; drc[is.na(drc)] <- 0.08
  out <- numeric(length(age))
  for (i in seq_along(age)) {
    a <- age[i]
    if (a <= 62) { out[i] <- cf_months(nra[i] - 744, drc[i], nra[i]); next }
    if (a >= 70) { out[i] <- cf_months(nra[i] - 840, drc[i], nra[i]); next }
    m <- nra[i] - 12 * a - 0:11
    s <- if (length(spike_share) == 1) spike_share else spike_share[i]
    if (nra[i] %/% 12 == a && s > 0) {        # the age holding the FRA: the spike, and the other months
      rest <- if (any(m > 0)) m[m > 0] else m[m < 0]   # reduced months before the FRA, or else credits after it
      out[i] <- s + (1 - s) * mean(cf_months(rest, drc[i], nra[i]))
    } else out[i] <- mean(cf_months(m, drc[i], nra[i]))
  }
  out
}

# Factor of the non-spike claims at the age holding the FRA (reduced months before it; scripts/19's
# reduced part at 66 in 2025)
fra_rest_factor <- function(b, pc) claim_factor_age(pc$nra_months[match(b, pc$birth_year)] %/% 12, b, pc, spike_share = 1e-12)

# FRA spike timing: months of births whose FRA falls in year T, by exact age at the FRA (floor of the
# FRA), for uniformly spread birth months
fra_spike_months <- function(years, pc) {
  do.call(rbind, lapply(years, function(T) {
    rows <- lapply((T - 68):(T - 65), function(b) {
      nra <- pc$nra_months[match(b, pc$birth_year)]; if (is.na(nra)) nra <- 804L
      k <- 0:11; yr <- b + (k + nra) %/% 12
      if (any(yr == T)) data.frame(year = T, age = nra %/% 12, months = sum(yr == T)) else NULL
    })
    x <- do.call(rbind, rows); aggregate(months ~ year + age, x, sum)
  }))
}
