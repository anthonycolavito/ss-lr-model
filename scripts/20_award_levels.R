# 20_award_levels.R
#
# Average PIA and monthly benefit (MBA) of new awards by year, sex and age at
# entitlement, 2025-2100 (methodology 4.3.c; build guide Phase 4 steps 3-4).
#
#   PIA(elig) = sum over the 30 intervals of factor_n x length_n(elig) x PAP_n,
#   length_n(elig) = length_n in 1979 dollars x BP1(elig) / $180 (the bend
#   points and the intervals move with the average wage index), then COLAs from
#   the eligibility year through December of the year before the award.
#
# Retired workers: PAPs from scripts/19 (shuttled, by year); eligibility year =
# award year - (age - 62); MBA = PIA x reduction or delayed credits at that
# exact age for the cohort's NRA (ages are exact ages at entitlement, as in
# BEPUF; scripts/14's December ages are about half a year later - EA-03 - and
# are matched when benefits are put together in Phase 5).
# Disabled workers: PAPs from scripts/18 (2025 base, held); eligibility year =
# award year - 1 (onset before entitlement, as the 2025 mapping); PIA x
# (1 - 0.93%), OCACT's adjudication-level adjustment (TF Ops p. 53); MBA = PIA.
#
# Check: 2025 averages vs Supplement 6.A4 (fitted in scripts/18-19).
#
# Input:  data/paps.rds (18), data/paps_retired.rds (19), params (03)
# Output: data/award_levels.rds

suppressMessages({library(data.table)})
py <- as.data.table(readRDS("data/params_by_year.rds")); pc <- as.data.table(readRDS("data/params_by_cohort.rds"))
p18 <- readRDS("data/paps.rds"); pr <- readRDS("data/paps_retired.rds")
iv <- p18$intervals
cola_to <- function(from, to) mapply(function(f, t) if (t - 1 < f) 1 else prod(1 + py$cola[py$year %in% f:(t - 1)] / 100), from, to)
pia_from <- function(paps, elig) {          # paps: data.table with n, pap; one cell
  bp1 <- py$pia_bp1[match(elig, py$year)]
  sum(iv$factor * iv$len79 * bp1 / 180 * paps$pap[match(iv$n, paps$n)])
}
nra_m <- function(b) pc$nra_months[match(b, pc$birth_year)]
drc_rate <- function(b) { x <- pc$drc_annual[match(b, pc$birth_year)] / 100; ifelse(is.na(x), 0.08, x) }
claim_factor <- function(age, b) {
  m <- nra_m(b) - 12 * age
  ifelse(m > 0, 1 - pmin(m, 36) * 5 / 900 - pmax(m - 36, 0) * 5 / 1200, 1 + pmin(-m, 12 * (70 - nra_m(b) / 12)) / 12 * drc_rate(b))
}

ret <- pr$paps[, .(pia_elig = pia_from(.SD, year - (age - 62L))), by = .(year, SEX, age), .SDcols = c("n", "pap")]
ret[, `:=`(elig = year - (age - 62L), b = year - age)]
ret[, pia := pia_elig * cola_to(elig, year)]
ret[, mba := pia * claim_factor(age, b)]
ret[, type := "retired"]

di_paps <- p18$paps[type == "disabled"]
di <- CJ(year = 2025:2100, SEX = c("M", "F"), age = sort(unique(di_paps$age)))
di <- di[, .(pia_elig = pia_from(di_paps[SEX == .BY$SEX & age == .BY$age], year - 1L)), by = .(year, SEX, age)]
di[, `:=`(elig = year - 1L, b = year - age)]
di[, pia := pia_elig * (1 - 0.0093) * cola_to(elig, year)]
di[, `:=`(mba = pia, type = "disabled")]
out <- rbind(ret, di)

# ---- Checks -----------------------------------------------------------------------------------------------------
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))
a4 <- as.matrix(readxl::read_excel("data-raw/supplement/2026/6a.xlsx", sheet = "6.A4", col_names = FALSE, col_types = "text", .name_repair = "minimal"))
rows <- c(`62` = 8, `63` = 9, `64` = 10, `65` = 12, `67` = 16, `68` = 17, `69` = 18)
chk <- data.table(age = as.integer(names(rows)), M = num(a4[rows, 8]), F = num(a4[rows, 10])) |>
  melt(id.vars = "age", variable.name = "SEX", value.name = "actual")
chk <- out[type == "retired" & year == 2025][chk, on = c("age", "SEX")][, gap_pct := round(100 * (mba / actual - 1), 1)]
cat("2025 retired-worker awards, average MBA by age, from PAPs vs 6.A4 (66 left out: it mixes reduced and FRA claims):\n")
print(dcast(chk, age ~ SEX, value.var = "gap_pct"))
cat("\nAverage PIA of new awards at 62 and 67 (men, women), nominal dollars:\n")
print(dcast(out[type == "retired" & age %in% c(62, 67) & year %in% c(2025, 2030, 2050, 2075, 2100), .(year, SEX, age, pia = round(pia))],
            year ~ SEX + age, value.var = "pia"))
cat("\nRatio of average award PIA to the average wage index (men, 67; disabled men 55):\n")
aw <- py[, .(year, awi)]
print(out[SEX == "M" & ((type == "retired" & age == 67) | (type == "disabled" & age == 55)) & year %in% c(2025, 2035, 2050, 2075, 2100)][aw, on = "year", nomatch = 0][
  , .(year, type, ratio = round(12 * pia / awi, 3))] |> dcast(year ~ type, value.var = "ratio"))

saveRDS(out, "data/award_levels.rds")
cat("Saved data/award_levels.rds\n")
