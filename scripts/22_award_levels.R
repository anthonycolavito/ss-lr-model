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
# Retired workers: PAPs from scripts/21 (shuttled in scripts/19 and moved to each year's cohort in 21); eligibility year =
# award year - (age - 62); MBA = PIA x reduction or delayed credits, averaged over the months of that
# exact age, the FRA spike at 1 (R/claim_factor.R; ages are exact ages at entitlement, as in
# BEPUF; scripts/14's December ages are about half a year later - EA-03 - and
# are matched when benefits are put together in Phase 5).
# Disabled workers: PAPs from scripts/21 (scripts/18's 2025 base moved to each cohort); eligibility year =
# award year - 1 (onset before entitlement, as the 2025 mapping); PIA x
# (1 - 0.93%), OCACT's adjudication-level adjustment (TF Ops p. 53); MBA = PIA.
#
# Check: 2025 averages vs Supplement 6.A4 (fitted in scripts/18-19).
#
# Input:  data/paps.rds (18, intervals), data/paps_projected.rds (21), params (03)
# Output: data/award_levels.rds

suppressMessages({library(data.table); library(dplyr)})

py <- as.data.table(readRDS("data/params_by_year.rds")); pc <- as.data.table(readRDS("data/params_by_cohort.rds"))
p18 <- readRDS("data/paps.rds"); pp <- readRDS("data/paps_projected.rds")$paps
iv <- p18$intervals
cola_to <- function(from, to) mapply(function(f, t) if (t - 1 < f) 1 else prod(1 + py$cola[py$year %in% f:(t - 1)] / 100), from, to)
pia_from <- function(paps, elig) {          # paps: data.table with n, pap; one cell
  bp1 <- py$pia_bp1[match(elig, py$year)]
  sum(iv$factor * iv$len79 * bp1 / 180 * paps$pap[match(iv$n, paps$n)])
}
ret <- pp[type == "retired", .(pia_elig = pia_from(.SD, year - (age - 62L))), by = .(year, SEX, age), .SDcols = c("n", "pap")]
ret[, `:=`(elig = year - (age - 62L), b = year - age)]
ret[, pia := pia_elig * cola_to(elig, year)]
# benefit / PIA: claims spread over the months of the exact age, the FRA spike at 1 (R/claim_factor.R,
# the same convention scripts/19 fitted 2025 with; PS-05)
source("R/claim_factor.R")
sp <- readRDS("data/paps_retired.rds")$spike_share[, .(year, SEX, age = t, spike)]
ret <- sp[ret, on = c("year", "SEX", "age")][is.na(spike), spike := 0]
ret[, mba := pia * claim_factor_age(age, b, pc, spike)]
ret[, spike := NULL]
ret[, type := "retired"]

di <- pp[type == "disabled", .(pia_elig = pia_from(.SD, year - 1L)), by = .(year, SEX, age), .SDcols = c("n", "pap")]
di[, `:=`(elig = year - 1L, b = year - age)]
di[, pia := pia_elig * (1 - 0.0093) * cola_to(elig, year)]
di[, `:=`(mba = pia, type = "disabled")]
out <- rbind(ret, di)

# ---- Checks -----------------------------------------------------------------------------------------------------
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))
a4 <- as.matrix(readxl::read_excel("data-raw/supplement/2026/6a.xlsx", sheet = "6.A4", col_names = FALSE, col_types = "text", .name_repair = "minimal"))
rows <- c(`62` = 8, `63` = 9, `64` = 10, `65` = 12, `66` = 15, `67` = 16, `68` = 17, `69` = 18)
chk <- data.table(age = as.integer(names(rows)), M = num(a4[rows, 8]), F = num(a4[rows, 10])) |>
  melt(id.vars = "age", variable.name = "SEX", value.name = "actual")
# 6.A4 is at December 2025 rates (its note a); the model's awards are at award-time rates
c25 <- 1 + py$cola[py$year == 2025] / 100
source("R/dual_excess.R")                   # 6.A4 is combined benefits; the model's awards are net of the excess (DX-03)
xs <- as.data.table(excess_by_age())[, .(age, SEX = sex, share)]
chk[, SEX := as.character(SEX)]
chk <- xs[chk, on = c("age", "SEX")][, actual := actual * (1 - share)]
chk <- out[type == "retired" & year == 2025][chk, on = c("age", "SEX")][, gap_pct := round(100 * (mba * c25 / actual - 1), 1)]
cat("2025 retired-worker awards, average MBA by age at December 2025 rates, from PAPs vs 6.A4 net of the dual-entitlement excess (fitted in 19):\n")
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
