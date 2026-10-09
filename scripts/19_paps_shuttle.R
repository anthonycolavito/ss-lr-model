# 19_paps_shuttle.R
#
# Retired-worker PAPs by year, sex and age at entitlement with OCACT's
# shuttling method (methodology 4.2, appendix 4.2-3), replacing the
# absolute-age mapping of scripts/18 for retired workers (F-22).
#
# 1. Shuttle matrix. The sample's distribution of new retired workers by age at
#    entitlement (62-70, conversions excluded) is lined up with a target
#    distribution by cumulative shares, so each sample age's records move to
#    the same or later ages (or, rarely, earlier ones) in order (OCACT's example
#    7). Row-normalized, it gives each target age's mix of sample ages.
# 2. Shuttled AIMEs. A record moved from age s to a later age t gets the extra
#    years s..t-1 of earnings: each year earned with the probability of the
#    covered-worker rate at that age and sex (randomized, seed fixed), at the
#    record's own relative earnings level (its mean ratio of earnings to the
#    average wage over the last five years with earnings) times the average
#    wage, capped at the taxable maximum. Moved to an earlier age, its years
#    from t on are dropped. AIME is recomputed with ranypia.
# 3. Base year 2025. Target = 2025 retired-worker awards by single age from
#    Supplement 6.A4 (66: new entitlements only; 70+ = 70-74 plus 75+). Shuttled
#    copies are mapped to 2025 awards at their new age (scripts/17's mapping)
#    and raked, by sex, to 6.B4's PIA bins for reduced awards (ages 62-66,
#    under the 2025 FRA of 66 and 10 months) and for awards without a reduction
#    (67+, with scripts/18's flagged conversions, whose 2025 count is 6.A4's).
#    The raking factors are carried back to the records (weighted mean over
#    each record's copies).
# 4. Projection years. Target(T, a) = 2025 awards(a) x scripts/14's
#    entitlements(T, a) / entitlements(2026, a), normalized - OCACT's alignment
#    of the Beneficiaries distribution to the Awards sample in the sample year.
#    PAPs(T, t) = sum over s of shuttle weight(T; t, s) x PAPs(s -> t),
#    where PAPs(s -> t) come from the calibrated records moved from s to t.
#    Earnings levels and covered-worker rates of the careers themselves are
#    not yet moved to projection years (OCACT's ATE and covered-rate
#    adjustments): PS-04.
# 5. Check: 2025 average award benefit by age vs 6.A4.
#
# Input:  data/bepuf_awardees.rds (17), data/paps.rds (18), data/insured_inputs.rds (04),
#         data/rw_entitlement_age.rds (14), params (03); Supplement 2026 6.A4, 6.B4
# Output: data/paps_retired.rds, outputs/paps_retired_checks.csv

suppressMessages({library(data.table); library(dplyr); library(tidyr); library(ranypia)})
set.seed(20261009)

d <- readRDS("data/bepuf_awardees.rds"); a <- copy(d$awardees); M0 <- d$earnings
p18 <- readRDS("data/paps.rds")
a <- a[p18$weights[, .(ID, conv)], on = "ID"]
py <- as.data.table(readRDS("data/params_by_year.rds"))
pc <- as.data.table(readRDS("data/params_by_cohort.rds"))
cov <- as.data.table(readRDS("data/insured_inputs.rds")$covered_rate)[, sex := as.character(sex)]
read_sheet <- function(f, s) as.matrix(readxl::read_excel(f, sheet = s, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))

# Retired workers, conversions excluded; ages 62-70 (70 = 70+)
r <- a[IP == "R" & !conv & grp %in% c("reduced", "at FRA", "DRC") & ACE >= 62]
r[, s := pmin(ACE, 70L)]
yrs_all <- 1951:2040
M <- matrix(0, nrow(r), length(yrs_all), dimnames = list(NULL, yrs_all))
M[, as.character(1951:2020)] <- M0[match(r$ID, d$awardees$ID), ]
awi <- setNames(py$awi, py$year); taxmax <- setNames(py$taxmax, py$year)
# relative earnings level: mean earnings/AWI over the last five years with earnings before entitlement
relmat <- M[, as.character(1951:2020)] / matrix(awi[as.character(1951:2020)], nrow(r), 70, byrow = TRUE)
r[, rel_level := sapply(seq_len(.N), function(i) {
  yy <- which(relmat[i, ] > 0 & (1950 + seq_len(70)) < ent[i]); if (!length(yy)) 0 else mean(tail(relmat[i, yy], 5))
})]

# ---- 2. AIME for every record at every age 62-70 ------------------------------------------------------------
aime_at <- function(t) {
  Mt <- M
  yrs_to_set <- lapply(seq_len(nrow(r)), function(i) if (t > r$s[i]) (r$BY[i] + r$s[i]):(r$BY[i] + t - 1L) else integer(0))
  idx <- which(lengths(yrs_to_set) > 0)
  if (length(idx)) {
    rows <- rep(idx, lengths(yrs_to_set[idx])); yy <- unlist(yrs_to_set[idx]); ag <- yy - r$BY[rows]
    key <- data.table(year = as.numeric(pmin(yy, 2100)), age = as.numeric(ag), sex = r$SEX[rows])
    cr <- cov[key, on = c("year", "age", "sex"), x.rate]
    cr[is.na(cr)] <- 0
    works <- runif(length(rows)) < cr
    amt <- pmin(r$rel_level[rows] * awi[as.character(yy)], taxmax[as.character(yy)])
    Mt[cbind(rows, yy - 1950L)] <- ifelse(works, amt, 0)
  }
  aime(Mt, r$BY + 62L, rep(35L, nrow(r)), first_year = 1951, last_year = r$BY + t - 1L)
}
A <- sapply(62:70, aime_at); colnames(A) <- 62:70
bp1 <- py$pia_bp1[match(r$BY + 62L, py$year)]
REL <- A / bp1                                   # AIME / first bend point, by target age
cat("Median AIME / BP1 by original age (rows) when moved to each age (columns), men:\n")
print(round(sapply(62:70, function(t) tapply(REL[r$SEX == "M", t - 61], r$s[r$SEX == "M"], median)), 2))

# ---- 1. Shuttle matrix ---------------------------------------------------------------------------------------------
shuttle <- function(S, D) {                       # S, D: shares by age 62:70; returns m[t, s]
  Fs <- c(0, cumsum(S)); Fd <- c(0, cumsum(D)); m <- matrix(0, 9, 9, dimnames = list(t = 62:70, s = 62:70))
  for (t in 1:9) for (s in 1:9) m[t, s] <- max(0, min(Fd[t + 1], Fs[s + 1]) - max(Fd[t], Fs[s]))
  m
}

# ---- 3. Base year 2025: shuttle, map, rake --------------------------------------------------------------------
a4 <- read_sheet("data-raw/supplement/2026/6a.xlsx", "6.A4")
rows <- c(`62` = 8, `63` = 9, `64` = 10, `65` = 12, `66` = 15, `67` = 16, `68` = 17, `69` = 18, `70` = 19)
D25 <- data.table(age = 62:70, M = num(a4[rows, 7]), F = num(a4[rows, 9]))
D25[age == 70, `:=`(M = M + num(a4[20, 7]), F = F + num(a4[20, 9]))]
conv25 <- c(M = num(a4[14, 7]), F = num(a4[14, 9]))
pia_formula <- function(x, b1, b2) floor(10 * (0.9 * pmin(x, b1) + 0.32 * pmax(0, pmin(x, b2) - b1) + 0.15 * pmax(0, x - b2))) / 10
colas <- function(from) { u <- unique(from); f <- sapply(u, function(y) if (y > 2024) 1 else prod(1 + py$cola[py$year %in% y:2024] / 100)); f[match(from, u)] }
nra_m <- function(b) pc$nra_months[match(b, pc$birth_year)]

r[, w0 := 1]
copies <- list()
for (sx in c("M", "F")) {
  i <- which(r$SEX == sx)
  S <- tapply(r$w0[i], factor(r$s[i], levels = 62:70), sum); S <- S / sum(S)
  D <- D25[[sx]] / sum(D25[[sx]])
  m <- shuttle(S, D)
  for (t in 62:70) for (s in 62:70) if (m[t - 61, s - 61] > 0) {
    j <- i[r$s[i] == s]
    copies[[length(copies) + 1]] <- data.table(k = j, SEX = sx, s = s, t = t, w = r$w0[j] * m[t - 61, s - 61] / S[s - 61],
                                               rel = REL[cbind(j, t - 61)])
  }
}
cp <- rbindlist(copies)
cp[, elig25 := 2025L - (t - 62L)]
cp[, `:=`(b1 = py$pia_bp1[match(elig25, py$year)], b2 = py$pia_bp2[match(elig25, py$year)])]
cp[, pia25 := floor(10 * pia_formula(rel * b1, b1, b2) * colas(elig25)) / 10]
cp[, red := t < ceiling(nra_m(2025L - t) / 12)]   # 2025 awards before the FRA: ages 62-66
cp[, mba25 := pia25 * fifelse(red, 1 - pmin(pmax(0, nra_m(2025L - t) - 12 * t), 36) * 5 / 900 - pmax(nra_m(2025L - t) - 12 * t - 36, 0) * 5 / 1200,
                              1 + pmax(0, 12 * pmin(t, 70) - nra_m(2025L - t)) / 12 * 0.08)]
# Age 66 in 2025 holds both reduced claims (66y0m-66y9m, FRA 66y10m) and claims at the FRA. Split the
# copies: the reduced share is 6.A4's new entitlements at 66 less 6.B5's FRA new entitlements (PS-02);
# the reduced part takes the average reduction for 1-10 months early, the rest none.
b5 <- read_sheet("data-raw/supplement/2026/6b.xlsx", "6.B5")
b5r <- which(b5[, 1] == "2025"); fra_new <- c(M = num(b5[b5r[1], 11]), F = num(b5[b5r[2], 11])) / 100
tot25 <- c(M = num(a4[6, 7]), F = num(a4[6, 9]))
rho <- 1 - fra_new / (c(M = num(a4[15, 7]), F = num(a4[15, 9])) / tot25)
cat("Share of 2025 new entitlements at 66 that are reduced:", round(rho, 3), "\n")
c66 <- cp[t == 66]
cp <- rbind(cp[t != 66],
            c66[, `:=`(w = w * rho[SEX], red = TRUE, mba25 = pia25 * (1 - mean(1:10) * 5 / 900))],
            copy(cp[t == 66])[, `:=`(w = w * (1 - rho[SEX]), red = FALSE, mba25 = pia25)])
brk <- c(-Inf, seq(300, 3300, 100), Inf)
cp[, bin := as.integer(cut(pia25, brk, right = FALSE))]
# conversions (scripts/18 mapping and weights) join the not-reduced cell
b4 <- read_sheet("data-raw/supplement/2026/6b.xlsx", "6.B4")
tgt <- rbind(data.table(SEX = "M", red = TRUE, bin = 1:32, n = num(b4[40:71, 6])), data.table(SEX = "F", red = TRUE, bin = 1:32, n = num(b4[74:105, 6])),
             data.table(SEX = "M", red = FALSE, bin = 1:32, n = num(b4[40:71, 8])), data.table(SEX = "F", red = FALSE, bin = 1:32, n = num(b4[74:105, 8])))
# conversion PIAs in 2025 (as scripts/18): DI-style relative AIME, eligibility 2025 - (FRA age - onset age)
p18cv <- readRDS("data/paps.rds")$weights[conv == TRUE]
conv_bins <- local({
  e <- copy(d$awardees[ID %in% p18cv$ID]); Mc <- M0[match(e$ID, d$awardees$ID), , drop = FALSE]
  last <- apply(Mc > 0, 1, function(z) { k <- which(z); if (length(k)) 1950L + max(k) else NA_integer_ })
  onset <- pmin(ifelse(is.na(last), e$ent - 1L, last + 1L), e$ent - 1L)
  cyc <- computation_years(e$BY, 1, onset, birth_day = 1, disabled = TRUE)
  am <- aime(Mc, onset, cyc, first_year = 1951, last_year = onset - 1L)
  rl <- am / py$pia_bp1[match(pmax(onset, 1979L), py$year)]
  el <- pmax(1979L, 2025L - (e$ent - onset)); b1c <- py$pia_bp1[match(el, py$year)]; b2c <- py$pia_bp2[match(el, py$year)]
  data.table(SEX = e$SEX, pia25 = floor(10 * pia_formula(rl * b1c, b1c, b2c) * colas(el)) / 10)
})
conv_bins[, bin := as.integer(cut(pia25, brk, right = FALSE))]
conv_dist <- conv_bins[, .N, by = .(SEX, bin)][, share := N / sum(N), by = SEX]
# Targets: PIA bins by sex x reduced/not (conversions removed from the latter), age shares, and
# average award benefit by age (6.A4) - the last because the shuttle alone moves a random slice of
# each sample age, while the 2016-2025 drop in claiming at 62 came mostly from higher earners (PS-03)
a6 <- data.table(t = 62:70, M = num(a4[rows, 8]), F = num(a4[rows, 10]))
tb_all <- rbindlist(lapply(c("M", "F"), function(sx) rbindlist(lapply(c(TRUE, FALSE), function(rd) {
  tb <- tgt[SEX == sx & red == rd]
  if (!rd) { cd <- conv_dist[SEX == sx][tb[, .(bin)], on = "bin"][is.na(share), share := 0]
             tb[, n := pmax(0, n - conv25[[sx]] * cd$share)] }
  tb[, p := n / sum(n)]
}))))
for (it in 1:60) {
  for (sx in c("M", "F")) {
    for (rd in c(TRUE, FALSE)) {
      i <- cp[, which(SEX == sx & red == rd)]; tb <- tb_all[SEX == sx & red == rd]
      sb <- cp[i, .(cur = sum(w)), by = bin][, cur := cur / sum(cur)][tb, on = "bin"][, f := fifelse(!is.na(cur) & cur > 0, p / cur, 1)]
      cp[i, w := w * sb$f[match(bin, sb$bin)]]
    }
    for (tt in 62:70) {                          # exponential tilt to the published average benefit at each age
      i <- cp[, which(SEX == sx & t == tt)]; target <- a6[t == tt][[sx]]
      x <- cp$mba25[i] / target - 1; w <- cp$w[i]
      g <- function(l) sum(w * exp(l * x) * x)
      lam <- tryCatch(uniroot(g, c(-20, 20))$root, error = function(e) 0)
      cp[i, w := w * exp(lam * x)]
    }
    D <- D25[[sx]] / sum(D25[[sx]])
    cur <- cp[SEX == sx, .(c = sum(w)), by = t][order(t)][, c := c / sum(c)]
    cp[SEX == sx, w := w * (D / cur$c)[t - 61]]
  }
}
cat("\n2025 after shuttling and raking: average award benefit by age, model vs 6.A4 (fitted):\n")
chk25 <- melt(a6, id.vars = "t", variable.name = "SEX", value.name = "actual")[cp[, .(model = sum(w * mba25) / sum(w)), by = .(SEX, t)], on = c("t", "SEX")]
chk25[, gap_pct := round(100 * (model / actual - 1), 2)]
print(dcast(chk25, t ~ SEX, value.var = "gap_pct"))
binfit <- cp[, .(cur = sum(w)), by = .(SEX, red, bin)][, cur := cur / sum(cur), by = .(SEX, red)][tb_all, on = c("SEX", "red", "bin")]
cat("PIA bins: largest gap in cumulative share", round(binfit[order(SEX, red, bin)][, .(ks = max(abs(cumsum(coalesce(cur, 0)) - cumsum(p)))), by = .(SEX, red)]$ks, 4), "\n")
cat("Weights (ratio to mean) 99th percentile / max:", round(quantile(cp$w / mean(cp$w), .99), 2), "/", round(max(cp$w / mean(cp$w)), 1), "\n")

# ---- 4. PAPs: base year 2025 from the calibrated copies; later years shuttle the 2025 base -------------------
len <- p18$intervals$len79; lo <- p18$intervals$lo79; fac <- p18$intervals$factor
paps_of <- function(x79, w) sapply(1:30, function(n) sum(w * pmin(pmax(x79 - lo[n], 0), len[n]) / len[n]) / sum(w))
# PAPs of the 2025 copies at age t0 moved to age t1 (AIMEs recomputed from the record's original age, REL)
pst <- rbindlist(lapply(c("M", "F"), function(sx) rbindlist(lapply(62:70, function(t0) rbindlist(lapply(62:70, function(t1) {
  j <- cp[SEX == sx & t == t0]
  data.table(SEX = sx, s = t0, t = t1, n = 1:30, pap = paps_of(180 * REL[cbind(j$k, t1 - 61)], j$w))
}))))))
S_cal <- D25[, .(s = age, M = M / sum(M), F = F / sum(F))] |> melt(id.vars = "s", variable.name = "SEX", value.name = "S")
S_cal[, SEX := as.character(SEX)]
ent <- as.data.table(readRDS("data/rw_entitlement_age.rds")$entitlements)[year >= 2026, .(n = sum(n)), by = .(year, SEX = sex, t = pmin(ae, 70L))]
base14 <- ent[year == 2026, .(SEX, t, n26 = n)]
d25l <- melt(D25, id.vars = "age", variable.name = "SEX", value.name = "n25")[, .(SEX = as.character(SEX), t = age, n25)]
tgtT <- ent[base14, on = c("SEX", "t")][d25l, on = c("SEX", "t")][, D := n25 * n / n26][, D := D / sum(D), by = .(year, SEX)]
paps_year <- function(T, sx) {
  S <- S_cal[SEX == sx][order(s)]$S; D <- if (T == 2025) S else tgtT[year == T & SEX == sx][order(t)]$D
  m <- shuttle(S, D); m <- m / rowSums(m)
  rbindlist(lapply(62:70, function(tt) {
    wts <- m[tt - 61, ]; ss <- which(wts > 0) + 61
    pst[SEX == sx & t == tt & s %in% ss, .(pap = sum(pap * wts[s - 61])), by = n][, `:=`(year = T, SEX = sx, age = tt)]
  }))
}
pap_ret <- rbindlist(lapply(2025:2100, function(T) rbindlist(lapply(c("M", "F"), function(sx) paps_year(T, sx)))))
avg79 <- pap_ret[, .(pia79 = sum(fac * len * pap)), by = .(year, SEX, age)]
cat("\nAverage PIA in 1979 terms by age at entitlement (men), selected years:\n")
print(dcast(avg79[SEX == "M" & year %in% c(2025, 2026, 2030, 2040, 2075, 2100)][, pia79 := round(pia79)], year ~ age, value.var = "pia79"))

dir.create("outputs", showWarnings = FALSE)
write.csv(chk25, "outputs/paps_retired_checks.csv", row.names = FALSE)
saveRDS(list(paps = pap_ret, paps_s_to_t = pst, base_dist = S_cal, targets = tgtT, check_2025 = chk25,
             copies_2025 = cp[, .(ID = r$ID[k], SEX, s, t, w, rel, pia25, mba25)]), "data/paps_retired.rds")
cat("Saved data/paps_retired.rds\n")
