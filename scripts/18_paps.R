# 18_paps.R
#
# Potential AIME percentages (PAPs) for new retired-worker and disabled-worker
# awards, by sex and age at entitlement (methodology 4.2, equation 4.2.4), from
# the BEPUF careers of scripts/17 reweighted to match 2025 awards.
#
# 1. Disability conversions. BEPUF counts a disabled worker converted at full
#    retirement age as a retired worker entitled at FRA with no reduction and no
#    delayed credits, like someone who claimed at FRA. We model conversions
#    separately (they keep their DI PIA), so they must come out of the
#    retired-worker PAPs. Within that group, by sex, records are ranked by how
#    long before entitlement their earnings stopped (then by lower recent
#    earnings), and the top share is flagged as conversions; the share is
#    Supplement 6.B5's conversions / all FRA awards for 2016-2020 (DECISIONS.md
#    PP-01). Flagged records get a DI-style PIA (onset the year after their last
#    earnings, disability computation years) for the reweighting only.
# 2. Reweighting (PP-02). Each record is mapped to a 2025 award at the same age
#    at entitlement (scripts/17) and weights are raked so that 2025 awards match:
#      retired, reduced, by sex      PIA bins (6.B4) x age 62 / 63 / 64 / 65-FRA (6.B5)
#      retired, not reduced, by sex  PIA bins (6.B4) x FRA newly entitled /
#                                    conversions / FRA-69 / 70+ (6.B5)
#      disabled, by sex              benefit bins (6.C1) x age group (6.A4)
# 3. PAPs: 30 AIME intervals in 1979 dollars (four of $45 to $180; nine of $45
#    and five of $100 to $1,085; ten of $200 and two of $1,000 to $5,085 - the
#    disability passage's layout, which sums correctly). For each cell,
#    PAP_n = weighted mean over records of (AIME in interval n) / (interval
#    length), with AIME in 1979 dollars = 180 x AIME / first bend point of the
#    eligibility year.
#      retired workers: ages at entitlement 62-70 (70 = 70 and over), excluding
#                       conversions
#      disabled workers: single ages 18-66, with ages under 25 pooled
# 4. Checks: average PIA from the PAPs equals the records' average; average
#    award benefits by single age (2025, Supplement 6.A4), which were not fitted.
#
# Input:  data/bepuf_awardees.rds (17); data/params_by_year.rds,
#         params_by_cohort.rds (03); Supplement 2026 6.A4, 6.B4, 6.B5, 6.C1
# Output: data/paps.rds, outputs/paps_checks.csv

suppressMessages({library(data.table); library(dplyr); library(tidyr); library(ranypia)})

d <- readRDS("data/bepuf_awardees.rds"); a <- copy(d$awardees); M <- d$earnings
py <- as.data.table(readRDS("data/params_by_year.rds"))
yrs <- as.integer(colnames(M))
a[, last_year := yrs[apply(M > 0, 1, function(z) { i <- which(z); if (length(i)) max(i) else NA_integer_ })]]
a[, gap := ent - pmin(last_year, ent - 1L)]
# recent earnings relative to AWI: mean over the 10 years before entitlement
awi <- setNames(py$awi, py$year)
rel_e <- M / matrix(awi[colnames(M)], nrow(M), ncol(M), byrow = TRUE)
idx <- outer(a$ent, 1:10, "-") - 1950
idx[idx < 1 | idx > ncol(M)] <- NA
a[, recent := rowMeans(matrix(rel_e[cbind(rep(seq_len(nrow(a)), 10), as.vector(idx))], ncol = 10), na.rm = TRUE)]
a[is.na(recent), recent := 0]

# ---- 1. Disability conversions within "at FRA" ---------------------------------------------------------
read_sheet <- function(f, s) as.matrix(readxl::read_excel(f, sheet = s, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))
b5 <- read_sheet("data-raw/supplement/2026/6b.xlsx", "6.B5")
men_rows <- which(b5[, 1] %in% as.character(2016:2025))
yr <- num(b5[men_rows, 1]); sx <- ifelse(seq_along(men_rows) <= length(men_rows) / 2, "M", "F")
b5d <- data.table(year = yr, SEX = sx, a62 = num(b5[men_rows, 6]), a63 = num(b5[men_rows, 7]), a64 = num(b5[men_rows, 8]),
                  a65 = num(b5[men_rows, 9]), fra = num(b5[men_rows, 10]), fra_new = num(b5[men_rows, 11]),
                  fra_conv = num(b5[men_rows, 12]), fra69 = num(b5[men_rows, 13]), a70 = num(b5[men_rows, 14]) + num(b5[men_rows, 15]))
conv_share <- b5d[year %in% 2016:2020, .(c = sum(fra_conv) / sum(fra)), by = SEX]
cat("Conversions / all FRA awards, 2016-2020 (6.B5):\n"); print(conv_share)
a[, conv := FALSE]
for (s in c("M", "F")) {
  i <- a[, which(grp == "at FRA" & SEX == s)]
  o <- i[order(-a$gap[i], a$recent[i])]
  a[o[seq_len(round(conv_share[SEX == s, c] * length(i)))], conv := TRUE]
}
cat("Flagged as conversions:", a[, sum(conv)], "of", a[grp == "at FRA", .N], "at FRA; their median gap",
    a[conv == TRUE, median(gap)], "years vs", a[grp == "at FRA" & !conv, median(gap)], "for the rest\n")
# DI-style PIA basis for conversions
a[, onset := fifelse(conv, pmin(last_year + 1L, ent - 1L), NA_integer_)]
a[conv == TRUE & is.na(onset), onset := ent - 1L]
ci <- a[, which(conv)]
cy_c <- computation_years(a$BY[ci], 1, a$onset[ci], birth_day = 1, disabled = TRUE)
aime_c <- aime(M[ci, , drop = FALSE], a$onset[ci], cy_c, first_year = 1951, last_year = a$onset[ci] - 1)
a[ci, `:=`(aime_dib = aime_c, rel_dib = aime_c / py$pia_bp1[match(pmax(onset, 1979L), py$year)])]   # onsets before 1979: 1979 bend points

# ---- 2. Map to 2025 awards and rake ----------------------------------------------------------------------
pia_formula <- function(x, bp1, bp2) floor(10 * (0.9 * pmin(x, bp1) + 0.32 * pmax(0, pmin(x, bp2) - bp1) + 0.15 * pmax(0, x - bp2))) / 10
colas <- function(from) { u <- unique(from); f <- sapply(u, function(y) if (y > 2024) 1 else prod(1 + py$cola[py$year %in% y:2024] / 100)); f[match(from, u)] }
a[, `:=`(rel_map = fifelse(conv, rel_dib, rel),
         elig25 = pmax(1979L, fifelse(IP == "D", 2024L, fifelse(conv, 2025L - (ent - onset), 2025L - (ACE - 62L)))))]
a[, `:=`(bp1 = py$pia_bp1[match(elig25, py$year)], bp2 = py$pia_bp2[match(elig25, py$year)])]
a[, pia25 := floor(10 * pia_formula(rel_map * bp1, bp1, bp2) * colas(elig25)) / 10]
brk <- c(-Inf, seq(300, 3300, 100), Inf)
a[, bin := as.integer(cut(pia25, brk, right = FALSE))]
a[, cell := fcase(IP == "D", "disabled", grp == "reduced", "reduced", grp %in% c("at FRA", "DRC"), "not reduced", default = NA_character_)]
a[, agrp := fcase(cell == "reduced", fcase(ACE == 62, "a62", ACE == 63, "a63", ACE == 64, "a64", default = "a65"),
                  cell == "not reduced", fcase(conv, "fra_conv", grp == "at FRA", "fra_new", ACE <= 69, "fra69", default = "a70"),
                  cell == "disabled", as.character(cut(pmin(ACE, 66L), c(0, 24, 29, 34, 39, 44, 49, 54, 59, 64, 66))))]

b4 <- read_sheet("data-raw/supplement/2026/6b.xlsx", "6.B4")
c1 <- read_sheet("data-raw/supplement/2026/6c.xlsx", "6.C1")
a4 <- read_sheet("data-raw/supplement/2026/6a.xlsx", "6.A4")
tgt_bins <- rbind(
  data.table(cell = "reduced", SEX = "M", bin = 1:32, n = num(b4[40:71, 6])), data.table(cell = "reduced", SEX = "F", bin = 1:32, n = num(b4[74:105, 6])),
  data.table(cell = "not reduced", SEX = "M", bin = 1:32, n = num(b4[40:71, 8])), data.table(cell = "not reduced", SEX = "F", bin = 1:32, n = num(b4[74:105, 8])),
  data.table(cell = "disabled", SEX = "M", bin = 1:32, n = num(c1[6:37, 5])), data.table(cell = "disabled", SEX = "F", bin = 1:32, n = num(c1[6:37, 7])))
b25 <- b5d[year == 2025]
tgt_age <- rbind(
  melt(b25[, .(SEX, a62, a63, a64, a65)], id.vars = "SEX", variable.name = "agrp", value.name = "n")[, cell := "reduced"],
  melt(b25[, .(SEX, fra_new, fra_conv, fra69, a70)], id.vars = "SEX", variable.name = "agrp", value.name = "n")[, cell := "not reduced"],
  data.table(cell = "disabled", SEX = rep(c("M", "F"), each = 10),
             agrp = rep(levels(cut(1, c(0, 24, 29, 34, 39, 44, 49, 54, 59, 64, 66))), 2),
             n = c(num(a4[c(23:29, 35, 41, 47), 7]), num(a4[c(23:29, 35, 41, 47), 9]))))
tgt_age[, agrp := as.character(agrp)]

a[, w := 1]
for (cl in c("reduced", "not reduced", "disabled")) for (s in c("M", "F")) {
  i <- a[, which(cell == cl & SEX == s)]
  tb <- tgt_bins[cell == cl & SEX == s][, p := n / sum(n)]
  ta <- tgt_age[cell == cl & SEX == s][, p := n / sum(n)]
  for (it in 1:100) {
    sb <- a[i, .(cur = sum(w)), by = bin][, cur := cur / sum(cur)][tb, on = "bin"][, f := fifelse(cur > 0, p / cur, 1)]
    a[i, w := w * sb$f[match(bin, sb$bin)]]
    sa <- a[i, .(cur = sum(w)), by = agrp][, cur := cur / sum(cur)][ta, on = "agrp"][, f := fifelse(cur > 0, p / cur, 1)]
    a[i, w := w * sa$f[match(agrp, sa$agrp)]]
    if (max(abs(sa$f - 1), na.rm = TRUE) < 1e-6) break
  }
  a[i, w := w / mean(w)]
}
cat("\nRaking weights (ratio to mean) by cell: 1st / 50th / 99th percentile, and records in empty bins:\n")
print(a[!is.na(cell), .(p01 = round(quantile(w, .01), 2), p50 = round(median(w), 2), p99 = round(quantile(w, .99), 2), max = round(max(w), 1)), by = .(cell, SEX)])

# ---- 3. PAPs ---------------------------------------------------------------------------------------------------
len <- c(rep(45, 4), rep(45, 9), rep(100, 5), rep(200, 10), rep(1000, 2))
lo <- c(0, cumsum(len))[1:30]
fac <- c(rep(0.9, 4), rep(0.32, 14), rep(0.15, 12))
stopifnot(sum(len[1:4]) == 180, sum(len[1:18]) == 1085)
pap_rows <- function(dd) {
  x <- dd$aime79; w <- dd$w
  sapply(1:30, function(n) sum(w * pmin(pmax(x - lo[n], 0), len[n]) / len[n]) / sum(w))
}
a[, age := fifelse(IP == "D", pmax(ACE, 24L), pmin(ACE, 70L))]   # DI: under 25 pooled as 24
pap_cells <- a[!is.na(cell) & !conv & (IP == "D" | ACE >= 62), {
  p <- pap_rows(.SD); list(n = 1:30, pap = p, records = .N)
}, by = .(type = fifelse(IP == "D", "disabled", "retired"), SEX, age), .SDcols = c("aime79", "w")]
setorder(pap_cells, type, SEX, age, n)

# Check: average PIA (in 1979 bend-point dollars) from PAPs vs records
chk <- a[!is.na(cell) & !conv & (IP == "D" | ACE >= 62), .(rec = sum(w * pia_formula(aime79, 180, 1085)) / sum(w)),
         by = .(type = fifelse(IP == "D", "disabled", "retired"), SEX, age)]
chk <- chk[pap_cells[, .(pap = sum(fac * len * pap)), by = .(type, SEX, age)], on = c("type", "SEX", "age")]
cat("\nAverage PIA in 1979 terms, records vs PAPs: max abs gap", round(chk[, max(abs(rec - pap) / rec)] * 100, 3), "%\n")

# ---- 4. Check against 2025 average award benefits by age (6.A4, not fitted) ----------------------------------
nra_m <- function(b) { pc <- as.data.table(readRDS("data/params_by_cohort.rds")); pc$nra_months[match(b, pc$birth_year)] }
red <- function(ace, b) { m <- pmax(0, nra_m(b) - 12 * ace); 1 - pmin(m, 36) * 5 / 900 - pmax(m - 36, 0) * 5 / 1200 }
drc <- function(ace, b) 1 + pmax(0, 12 * pmin(ace, 70) - nra_m(b)) / 12 * 0.08
a[, b25 := 2025L - ACE]
a[, mba25 := pia25 * fcase(cell == "reduced", red(ACE, b25), cell == "not reduced" & !conv, drc(ACE, b25), default = 1)]
a6 <- data.table(age = c(62:65, 67:69), M = num(a4[c(8:10, 12, 16:18), 8]), F = num(a4[c(8:10, 12, 16:18), 10]))
a6 <- rbind(a6, data.table(age = 66, M = num(a4[15, 8]), F = num(a4[15, 10])))
m6 <- a[cell %in% c("reduced", "not reduced") & !conv & ACE %in% 62:69, .(model = sum(w * mba25) / sum(w)), by = .(SEX, age = ACE)]
cmp_age <- melt(a6, id.vars = "age", variable.name = "SEX", value.name = "actual")[m6, on = c("age", "SEX")][, gap_pct := 100 * (model / actual - 1)]
setorder(cmp_age, SEX, age)
cat("\nAverage monthly benefit of 2025 retired-worker awards by age (66: new entitlements only), model vs 6.A4:\n")
print(cmp_age[, .(SEX, age, model = round(model), actual = round(actual), gap_pct = round(gap_pct, 1))])
conv66 <- a[conv == TRUE, .(model = sum(w * pia25) / sum(w)), by = SEX][, actual := c(M = num(a4[14, 8]), F = num(a4[14, 10]))[SEX]]
setorder(conv66, -SEX); cat("\nConversions (66 in 2025), average PIA: model", paste(round(conv66$model), collapse = " / "), "vs 6.A4", paste(round(conv66$actual), collapse = " / "), "(men / women)\n")
di_age <- a[cell == "disabled", .(model = sum(w * pia25) / sum(w)), by = .(SEX, agrp)]
di_act <- data.table(agrp = rep(levels(cut(1, c(0, 24, 29, 34, 39, 44, 49, 54, 59, 64, 66))), 2), SEX = rep(c("M", "F"), each = 10),
                     actual = c(num(a4[c(23:29, 35, 41, 47), 8]), num(a4[c(23:29, 35, 41, 47), 10])))
di_cmp <- di_act[di_age, on = c("agrp", "SEX")][, gap_pct := round(100 * (model / actual - 1), 1)]
cat("\nDisabled-worker awards, average benefit by age group, model vs 6.A4 (age mix fitted, levels not):\n")
print(dcast(di_cmp, agrp ~ SEX, value.var = "gap_pct"))

dir.create("outputs", showWarnings = FALSE)
write.csv(rbind(cmp_age[, .(check = "retired MBA by age", SEX, group = as.character(age), model, actual)],
                di_cmp[, .(check = "disabled benefit by age group", SEX, group = agrp, model, actual)]),
          "outputs/paps_checks.csv", row.names = FALSE)
saveRDS(list(paps = pap_cells, intervals = data.table(n = 1:30, lo79 = lo, len79 = len, factor = fac),
             weights = a[, .(ID, cell, conv, w, ACE, SEX, IP)], checks = list(retired_age = cmp_age, conversions = conv66, disabled = di_cmp)),
        "data/paps.rds")
cat("Saved data/paps.rds\n")
