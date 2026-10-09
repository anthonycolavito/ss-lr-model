# 21_paps_projection.R
#
# Moves the careers behind the 2025 PAPs to the cohorts entitled in later years
# (methodology 4.2.1; DECISIONS.md PE-02 to PE-05), for retired workers
# (scripts/19's calibrated 2025 copies) and disabled workers (scripts/18).
# A 2025 award at age t belongs to the cohort born 2025 - t; an award at the
# same age in year T to the cohort born T - t. For each record:
#
#   x(a)  earnings at age a / AWI that year, from its BEPUF history (sample
#         cohort b); retired records moved to a later age by shuttling get the
#         extra years as in scripts/19 (redrawn here with a fixed seed)
#   level relative to OCACT's ATE: x_c(a) = x(a) x G(a, sex, c + a) / G(a, sex, b + a)
#         for target cohort c, capped at the taxable maximum / AWI of year c + a
#         (G = average taxable earnings / AWI, scripts/20)
#   covered-worker rates: moving from the 2025 cohort to cohort c, at each age
#         years with earnings are added (if the economy-wide covered-worker rate
#         of year c + a is higher than that of 2025's cohort) or removed (if
#         lower), with probability f x change / room (OCACT's "potential
#         difference"; f = 1 for men, 0.57 for women, the share of the change
#         OCACT attributes to people becoming insured). Added years earn the
#         record's own level (its mean x / G over its years with earnings) x G.
#         The same random number per record and age is used for every year, so
#         PAPs move smoothly. Disabled records' last 10 years before entitlement
#         are not modified (they determine disability insured status).
#   position = sum of the top 35 x_c (retired) or the mean of the top-y x_c over
#         the disability computation years (disabled); the record's 2025 AIME /
#         first bend point is scaled by position(T) / position(2025).
# PAPs are recomputed every five years from 2025 and interpolated linearly.
# Not done: OCACT's dispersion adjustment (above / below the median), the
# women's years-of-earnings rule, and covered earnings above old taxable
# maximums (PE-05).
#
# Input:  data/bepuf_awardees.rds (17), data/paps.rds (18), data/paps_retired.rds (19),
#         data/ate_by_age.rds (20), data/insured_inputs.rds (04), params (03)
# Output: data/paps_projected.rds

suppressMessages({library(data.table); library(matrixStats); library(ranypia)})
set.seed(20261010)

d <- readRDS("data/bepuf_awardees.rds"); aw <- d$awardees; M0 <- d$earnings
p18 <- readRDS("data/paps.rds"); p19 <- readRDS("data/paps_retired.rds")
G <- as.data.table(readRDS("data/ate_by_age.rds")$G)
py <- as.data.table(readRDS("data/params_by_year.rds"))
cov <- as.data.table(readRDS("data/insured_inputs.rds")$covered_rate)[, sex := as.character(sex)]
iv <- p18$intervals
ages <- 15:69; na <- length(ages)
awi <- setNames(py$awi, py$year); capr <- setNames(py$taxmax / py$awi, py$year)
Garr <- function(sx, yrs, a) { k <- G[data.table(year = pmin(pmax(yrs, 1951), 2100), sex = sx, age = pmin(a, 70)), on = c("year", "sex", "age"), x.G]; k }
CRarr <- function(sx, yrs, a) { k <- cov[data.table(year = as.numeric(pmin(pmax(yrs, 1937), 2100)), age = as.numeric(a), sex = sx), on = c("year", "age", "sex"), x.rate]; k[is.na(k)] <- 0; k }

# ---- Records -------------------------------------------------------------------------------------------------
# retired: 2025 copies (ID, original age s, target age t, weight w, 2025 position rel)
rc <- as.data.table(p19$copies_2025)[, .(ID, SEX, s, t, w, rel25 = rel, type = "retired")]
rc <- rc[, .(w = sum(w), rel25 = rel25[1]), by = .(ID, SEX, s, t, type)]          # the two halves at 66
# disabled: scripts/18 weights; position = AIME / BP1 at entitlement
dw <- p18$weights[IP == "D", .(ID, SEX, w)]
dc <- aw[dw, on = c("ID", "SEX")][, .(ID, SEX, s = ACE, t = ACE, w, rel25 = rel, type = "disabled")]
recs <- rbind(rc, dc)
recs[, b := aw$BY[match(ID, aw$ID)]]
recs[, row := match(ID, aw$ID)]
cat("Records: retired", rc[, .N], "copies; disabled", dc[, .N], "\n")

# x(a) = earnings / AWI at age a, ages 15-69, from the sample history (calendar years b + a)
X <- matrix(0, nrow(recs), na)
for (j in seq_along(ages)) {
  yr <- recs$b + ages[j]; ok <- yr >= 1951 & yr <= 2020
  X[ok, j] <- M0[cbind(recs$row[ok], yr[ok] - 1950)] / awi[as.character(yr[ok])]
}
# keep only years before entitlement at the original age (award basis)
for (j in seq_along(ages)) X[ages[j] >= recs$s, j] <- 0
# record's own level relative to the economy-wide pattern of its cohort
Gs <- sapply(seq_along(ages), function(j) Garr(recs$SEX, recs$b + ages[j], ages[j]))
lev <- rowSums(ifelse(X > 0, X / Gs, 0)) / pmax(1, rowSums(X > 0))
# shuttled retired copies: extra years s..t-1 at the covered rate of the 2025 cohort, at the record's level
U <- matrix(runif(length(X)), nrow(X))
for (j in seq_along(ages)) {
  a <- ages[j]; m <- recs$type == "retired" & a >= recs$s & a < recs$t
  if (!any(m)) next
  c25 <- 2025L - recs$t[m]
  cr <- CRarr(recs$SEX[m], c25 + a, a)
  X[m, j] <- ifelse(U[m, j] < cr, pmin(lev[m] * Garr(recs$SEX[m], c25 + a, a), capr[as.character(pmin(c25 + a, 2105))]), 0)
}
cy <- ifelse(recs$type == "retired", 35L, computation_years(recs$b, 1, recs$b + recs$t, birth_day = 1, disabled = TRUE))
protect <- sapply(ages, function(a) recs$type == "disabled" & a >= recs$t - 10)   # DI insured-status years

position <- function(T) {
  cT <- T - recs$t; c25 <- 2025L - recs$t
  Xc <- X
  for (j in seq_along(ages)) {
    a <- ages[j]; live <- a < recs$t
    if (!any(live)) next
    gb <- Gs[, j]; gt <- Garr(recs$SEX, cT + a, a); g25 <- Garr(recs$SEX, c25 + a, a)
    cap <- capr[as.character(pmin(cT + a, 2105))]
    has <- X[, j] > 0
    Xc[has, j] <- pmin(X[has, j] * gt[has] / gb[has], cap[has])
    # covered-worker rates: cohort cT vs the 2025 cohort at the same age
    f <- ifelse(recs$SEX == "M", 1, 0.57)
    crT <- CRarr(recs$SEX, cT + a, a); cr25 <- CRarr(recs$SEX, c25 + a, a)
    up <- crT > cr25
    p_add <- ifelse(up, f * (crT - cr25) / pmax(1e-6, 1 - cr25), 0)
    p_rem <- ifelse(!up, f * (cr25 - crT) / pmax(1e-6, cr25), 0)
    mod <- live & !protect[, j]
    add <- mod & !has & U[, j] < p_add
    rem <- mod & has & U[, j] < p_rem
    Xc[add, j] <- pmin(lev[add] * gt[add], cap[add])
    Xc[rem, j] <- 0
    Xc[!live, j] <- 0
  }
  rk <- rowRanks(Xc, ties.method = "first")
  top <- rk > (na - cy)
  rowSums(Xc * top) / cy
}
pos25 <- position(2025)
grid <- seq(2025, 2100, 5)
len <- iv$len79; lo <- iv$lo79
paps_of <- function(x79, w) sapply(1:30, function(n) sum(w * pmin(pmax(x79 - lo[n], 0), len[n]) / len[n]) / sum(w))
recs[, age := fifelse(type == "disabled", pmax(t, 24L), t)]
out <- list(); ratio_summary <- list()
for (T in grid) {
  f <- position(T) / pmax(pos25, 1e-9); f[pos25 <= 0] <- 1
  recs[, relT := rel25 * f]
  out[[as.character(T)]] <- recs[, .(n = 1:30, pap = paps_of(180 * relT, w)), by = .(type, SEX, age)][, year := T]
  recs[, fT := f]
  ratio_summary[[as.character(T)]] <- recs[, .(year = T, mean_factor = sum(w * fT) / sum(w)), by = .(type, SEX)]
  message(format(Sys.time(), "%H:%M:%S"), " ", T)
}
pp <- rbindlist(out)
cat("\nAverage position factor relative to 2025 (weighted):\n")
print(dcast(rbindlist(ratio_summary), year ~ type + SEX, value.var = "mean_factor")[, lapply(.SD, function(z) if (is.numeric(z)) round(z, 3) else z)])

# Interpolate to every year 2025-2100
full <- pp[, {
  yy <- 2025:2100
  list(year = yy, pap = approx(year, pap, xout = yy)$y)
}, by = .(type, SEX, age, n)]
# Retired: apply each year's shuttling (scripts/19) as a ratio to the 2025 PAPs at that age
ret19 <- as.data.table(p19$paps)
base19 <- ret19[year == 2025, .(SEX, age, n, pap25 = pap)]
ret <- full[type == "retired"][ret19[, .(year, SEX, age, n, pap19 = pap)], on = c("year", "SEX", "age", "n")][base19, on = c("SEX", "age", "n")]
ret[, pap := pmin(1, ifelse(pap25 > 0, pap * pap19 / pap25, pap19))]
paps <- rbind(ret[, .(type, year, SEX, age, n, pap)], full[type == "disabled", .(type, year, SEX, age, n, pap)])
avg <- paps[, .(pia79 = sum(iv$factor * iv$len79 * pap)), by = .(type, year, SEX, age)]
cat("\nAverage PIA in 1979 terms: retired at 62 and 67, disabled at 55, by sex:\n")
print(dcast(avg[((type == "retired" & age %in% c(62, 67)) | (type == "disabled" & age == 55)) & year %in% c(2025, 2030, 2040, 2050, 2075, 2100)][, pia79 := round(pia79)],
            year ~ type + SEX + age, value.var = "pia79"))

saveRDS(list(paps = paps, grid = pp, factors = rbindlist(ratio_summary)), "data/paps_projected.rds")
cat("Saved data/paps_projected.rds\n")
