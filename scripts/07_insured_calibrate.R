# 07_insured_calibrate.R
#
# Scales the simulated insured rates (06) so they match published figures,
# for both sexes and both kinds of insured status. Simulation gaps that remain
# after fitting (DECISIONS.md F-05, F-06, and the missing disabled-worker
# add-back) would otherwise carry into every later phase.
#
# Targets
#   History   Supplement 4.C2, fully and disability insured by age group and
#             sex, as rates of the Dec 31 population.
#   Projected 2026 TR (Program Assumptions, V.C.3): fully insured at age 62,
#             men 92.6% (2025) and 88.4% (2100), women 88.5% and 87.7%;
#             disability insured at age 50, both sexes, 75.9% (2025) and
#             77.4% (2100).
#
# Method (DECISIONS.md, C-01 to C-04)
#   1. Base factor for each status, sex and 4.C2 age group = 4.C2 rate / simulated
#      rate, pooled over 2013-2022: the latest ten years built on actual earnings
#      data. Later 4.C2 years are estimates (earnings data lag about two
#      years) and differ from OCACT's newer series by up to 1.4%.
#   2. Spread to single ages: linear between group midpoints, flat beyond the
#      first and last midpoints, so there are no steps at group edges.
#   3. Years through 2022 get the base factor. After 2022 a second factor,
#      one per status and sex, grades linearly from 1 in 2022 to the value
#      that hits the TR's 2025 figure, then linearly to the value that hits
#      the TR's 2100 figure (fully: age 62 by sex; disability: age 50, one
#      factor for both sexes since the TR doesn't split it). The TR's 2025
#      figures are its own estimates, as are 4.C2's 2023-2025 values; we
#      match the TR's, since the TR is what we calibrate to.
#   4. Fully insured capped at 99.5%; disability insured capped at fully insured.
#
# Disabled workers (DINADD, methodology 3.1): workers on the rolls more than 3
# years fail the recent-work test only because they have no earnings while on
# benefits; OCACT adds them back to the simulated disability insured rate. We
# do the same, and the factors above apply to the simulated part only:
#   disability insured = simulated x factor + on the rolls 4+ years / population
# History: Study 130 Table 6 (in current pay by age group, 2001-24), split by
# duration and single age as in the December 2025 stock (scripts/09); 2025 the
# stock itself; 1970-2000 V.C5 totals with the 2001 age-sex mix. Projection:
# scripts/10 (rolls by age, duration 4+), so 07 and 10 are run twice (C-07).
#
# Input:  data/insured_rates.rds (06), data/insured_inputs.rds (04),
#         data/population_dec.rds (01), data/di_inputs.rds (08),
#         data/di_stock_2025.rds (09), data/di_projection.rds (10, if present)
# Output: data/insured_rates_calibrated.rds, outputs/insured_calibration_factors.csv

library(dplyr)
library(tidyr)

sim <- readRDS("data/insured_rates.rds")
inp <- readRDS("data/insured_inputs.rds")
pop <- readRDS("data/population_dec.rds") |>
  group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop")
groups <- inp$target_groups
base_years <- 2013:2022   # years built on actual earnings data (C-02)
tr <- tibble(sex = factor(c("M", "F"), levels = c("M", "F")),
             fully_2025 = c(0.926, 0.885), fully_2100 = c(0.884, 0.877))
tr_dis50 <- c(`2025` = 0.759, `2100` = 0.774)

# Disability insured is compared below 65 only, as in 06: at 65+ 4.C2's
# disability-insured count isn't comparable (most are converted to retired-
# worker status). Ages 65-69 take the factor at the last midpoint (62).
long <- sim |> pivot_longer(c(fully, disability), names_to = "status", values_to = "sim") |>
  filter(!is.na(sim))

# ---- 0. DINADD: disabled workers on the rolls 4+ years, by year, sex, age -------------
di <- readRDS("data/di_inputs.rds")
# Fresh build: scripts/09 needs this script's rates before the DI stock exists,
# so the first pass runs without DINADD (scripts/run_all.R: 07 -> 09 -> 10 -> 07 -> 09 -> 10).
bootstrap <- !file.exists("data/di_stock_2025.rds")
if (bootstrap) {
  cat("Bootstrap pass: no DI stock yet (scripts/09), DINADD set to zero\n")
  dinadd <- pop |> transmute(year, sex, age, dinadd_rate = 0)
} else {
st25 <- readRDS("data/di_stock_2025.rds") |> mutate(sex = factor(sex, levels = c("M", "F")))
ag <- c("a15_19", "a20_24", "a25_29", "a30_34", "a35_39", "a40_44", "a45_49", "a50_54", "a55_59", "a60_64", "a65_66")
glo <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65); ghi <- c(19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 66)
shape <- st25 |> mutate(age = pmax(attained_age, 15L), g = ag[findInterval(age, glo)]) |>
  group_by(sex, g, age) |> summarise(all = sum(current_pay), d4 = sum(current_pay[duration >= 4]), .groups = "drop") |>
  group_by(sex, g) |> mutate(w = all / sum(all), share_d4 = ifelse(all > 0, d4 / all, 0)) |> ungroup()
t6 <- di$hist_inforce |> filter(sex != "T") |> mutate(sex = factor(as.character(sex), levels = c("M", "F"))) |>
  pivot_longer(all_of(ag), names_to = "g", values_to = "n")
din_hist <- t6 |> inner_join(shape, by = c("sex", "g"), relationship = "many-to-many") |>
  transmute(year, sex, age, dinadd = n * w * share_d4)
din_2025 <- st25 |> filter(duration >= 4) |> group_by(sex, age = attained_age) |>
  summarise(dinadd = sum(current_pay), .groups = "drop") |> mutate(year = 2025L)
mix01 <- din_hist |> filter(year == 2001) |> mutate(share = dinadd / sum(dinadd)) |> select(sex, age, share)
din_early <- di$vc5 |> filter(year < 2001) |> select(year, dw) |>
  expand_grid(mix01) |> transmute(year, sex, age, dinadd = 1000 * dw * share * sum(din_hist$dinadd[din_hist$year == 2001]) /
                                     (1000 * di$vc5$dw[di$vc5$year == 2001]))
din_proj <- NULL
if (file.exists("data/di_projection.rds")) {
  sa <- readRDS("data/di_projection.rds")$stock_age
  if ("n_d4" %in% names(sa)) din_proj <- sa |> transmute(year, sex = factor(sex, levels = c("M", "F")), age = a, dinadd = n_d4)
}
if (is.null(din_proj)) {
  cat("No projected rolls yet (first pass): 2026-2100 hold the 2025 DINADD rate\n")
  r25 <- din_2025 |> inner_join(pop |> filter(year == 2025), by = c("year", "sex", "age")) |> transmute(sex, age, r = dinadd / pop)
  din_proj <- pop |> filter(year >= 2026) |> inner_join(r25, by = c("sex", "age")) |> transmute(year, sex, age, dinadd = r * pop)
}
dinadd <- bind_rows(din_early, din_hist, din_2025, din_proj) |>
  group_by(year, sex, age) |> summarise(dinadd = sum(dinadd), .groups = "drop") |>
  inner_join(pop, by = c("year", "sex", "age")) |> transmute(year, sex, age, dinadd_rate = dinadd / pop)
}
long <- long |> left_join(dinadd |> mutate(status = "disability"), by = c("year", "sex", "age", "status")) |>
  mutate(dinadd_rate = coalesce(dinadd_rate, 0))
long_cmp <- long |> filter(!(status == "disability" & age >= 65))

# ---- 1. Base factors by age group ---------------------------------------------
# Simulated group rates as 4.C2 measures them: Dec 31 population weights, with
# ages 0-12 (never insured) in the "under 20" denominator.
kids <- expand_grid(year = unique(long$year), age = 0:12, sex = factor(c("M", "F"), levels = c("M", "F")),
                    status = c("fully", "disability"), sim = 0, dinadd_rate = 0)
sim_groups <- bind_rows(long_cmp, kids) |>
  inner_join(pop, by = c("year", "sex", "age")) |>
  inner_join(groups, by = join_by(between(age, lo, hi))) |>
  group_by(year, sex, status, group) |>
  summarise(sim = sum(sim * pop) / sum(pop), add = sum(dinadd_rate * pop) / sum(pop), pop = sum(pop), .groups = "drop")

base <- sim_groups |>
  inner_join(inp$targets |> select(year, status, sex, group, rate),
             by = c("year", "sex", "status", "group")) |>
  filter(year %in% base_years) |>
  group_by(status, sex, group) |>
  summarise(factor = sum((rate - add) * pop) / sum(sim * pop), .groups = "drop") |>
  inner_join(groups, by = "group") |>
  mutate(mid = ifelse(group == "u20", 16, ifelse(group == "75plus", 80, (lo + hi) / 2)))

# ---- 2. Single-age base factors --------------------------------------------------
ages <- sort(unique(long$age))
base_age <- base |>
  group_by(status, sex) |>
  reframe(age = ages, base = approx(mid, factor, xout = ages, rule = 2)$y)

# ---- 3. Projection grade to the TR's 2100 targets ------------------------------
at <- function(st, a, yr) long |> filter(status == st, age == a, year == yr)
b_at <- function(st, a) base_age |> filter(status == st, age == a) |> select(sex, base)

g_at <- function(yr) {
  gf <- at("fully", 62, yr) |> inner_join(b_at("fully", 62), by = "sex") |>
    inner_join(tr |> select(sex, target = !!paste0("fully_", yr)), by = "sex") |>
    transmute(sex, status = "fully", g = target / (sim * base))
  d50 <- at("disability", 50, yr) |> inner_join(b_at("disability", 50), by = "sex") |>
    inner_join(pop |> filter(year == yr, age == 50), by = c("year", "sex", "age"))
  gd <- (tr_dis50[[as.character(yr)]] * sum(d50$pop) - sum(d50$dinadd_rate * d50$pop)) /
    sum(d50$sim * d50$base * d50$pop)
  bind_rows(gf, tibble(sex = factor(c("M", "F"), levels = c("M", "F")), status = "disability", g = gd))
}
grade <- g_at(2025) |> rename(g2025 = g) |>
  inner_join(g_at(2100) |> rename(g2100 = g), by = c("sex", "status"))

calibrated <- long |>
  inner_join(base_age, by = c("status", "sex", "age")) |>
  inner_join(grade, by = c("status", "sex")) |>
  mutate(g = ifelse(year <= 2022, 1,
                    ifelse(year <= 2025, 1 + (g2025 - 1) * (year - 2022) / 3,
                           g2025 + (g2100 - g2025) * (year - 2025) / (2100 - 2025))),
         factor = base * g,
         rate = sim * factor + dinadd_rate) |>
  select(year, age, sex, status, sim, factor, rate, dinadd_rate) |>
  pivot_wider(names_from = status, values_from = c(sim, factor, rate, dinadd_rate)) |>
  mutate(fully = pmin(0.995, rate_fully),
         disability = pmin(rate_disability, fully)) |>
  select(year, age, sex, fully, disability, sim_fully, sim_disability, dinadd = dinadd_rate_disability,
         factor_fully, factor_disability) |>
  arrange(sex, year, age)

# ---- 4. Checks ------------------------------------------------------------------
cat("Base factors (4.C2 / simulated, 2013-2022):\n")
print(base |> select(status, sex, group, factor) |> mutate(factor = round(factor, 3)) |>
        pivot_wider(names_from = c(status, sex), values_from = factor), n = Inf)
cat("\nSecond factor in 2025 and 2100 (1 = none):\n")
print(grade |> mutate(across(c(g2025, g2100), ~ round(.x, 3))))

a62 <- calibrated |> filter(age == 62, year %in% c(2025, 2100)) |>
  select(year, sex, sim = sim_fully, calibrated = fully) |>
  inner_join(tr |> pivot_longer(-sex, names_to = "year", values_to = "tr") |>
               mutate(year = as.integer(sub("fully_", "", year))), by = c("year", "sex"))
cat("\nFully insured at 62 vs TR (both years fitted):\n")
print(a62 |> mutate(across(c(sim, calibrated, tr), ~ round(100 * .x, 1))))

d50c <- calibrated |> filter(age == 50, year %in% c(2025, 2100)) |>
  inner_join(pop, by = c("year", "sex", "age")) |>
  group_by(year) |> summarise(sim = 100 * sum((sim_disability + dinadd) * pop) / sum(pop),
                              calibrated = 100 * sum(disability * pop) / sum(pop)) |>
  mutate(tr = 100 * tr_dis50[as.character(year)])
cat("\nDisability insured at 50, both sexes (both years fitted):\n")
print(d50c |> mutate(across(-year, ~ round(.x, 1))))

hist_gap <- calibrated |>
  select(year, age, sex, fully, disability) |>
  pivot_longer(c(fully, disability), names_to = "status", values_to = "sim") |>
  filter(!is.na(sim), !(status == "disability" & age >= 65)) |>
  bind_rows(kids) |>
  inner_join(pop, by = c("year", "sex", "age")) |>
  inner_join(groups, by = join_by(between(age, lo, hi))) |>
  group_by(year, sex, status, group) |>
  summarise(sim = sum(sim * pop) / sum(pop), .groups = "drop") |>
  inner_join(inp$targets |> select(year, status, sex, group, rate),
             by = c("year", "sex", "status", "group")) |>
  mutate(period = ifelse(year %in% base_years, "2013-2022", ifelse(year > 2022, "2023-2025", ifelse(year >= 1990, "1990-2012", "1970-1989"))))
cat("\nCalibrated vs 4.C2: RMSE across groups, percentage points:\n")
print(hist_gap |> group_by(status, sex, period) |>
        summarise(rmse = round(100 * sqrt(mean((sim - rate)^2)), 2), .groups = "drop") |>
        pivot_wider(names_from = period, values_from = rmse))

# ---- Save ------------------------------------------------------------------------
dir.create("outputs", showWarnings = FALSE)
write.csv(bind_rows(base |> select(status, sex, group, factor) |> mutate(kind = "base, by age group"),
                    grade |> transmute(status, sex, group = "all", factor = g2025,
                                       kind = "second factor, 2025"),
                    grade |> transmute(status, sex, group = "all", factor = g2100,
                                       kind = "second factor, 2100")),
          "outputs/insured_calibration_factors.csv", row.names = FALSE)
saveRDS(calibrated, "data/insured_rates_calibrated.rds")
cat("\nSaved data/insured_rates_calibrated.rds:", nrow(calibrated), "rows\n")
