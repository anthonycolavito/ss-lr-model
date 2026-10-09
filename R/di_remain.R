# di_remain.R
#
# Share of a birth cohort's disabled workers still on the DI rolls at year-end (not yet converted at
# NRA), uniform birth months (DP-04): those aged a at the end of year y, born y - a, remain with
# probability min(1, max(0, NRA - a)). Used to spread Study 130's 65-66 age group by single age in
# each historical year (scripts/07, 11, 13; F-34) and to date historical conversions (scripts/13).

di_remain <- function(y, a, pc = readRDS("data/params_by_cohort.rds")) {
  b <- y - a
  v <- pc$nra_months[match(b, pc$birth_year)] / 12
  v <- ifelse(is.na(v), ifelse(b < 1937, 65, 67), v)
  pmin(1, pmax(0, v - a))
}
