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
# Input:  data/insured_rates.rds (06), data/insured_inputs.rds (04),
#         data/population_dec.rds (01)
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
long_cmp <- long |> filter(!(status == "disability" & age >= 65))

# ---- 1. Base factors by age group ---------------------------------------------
# Simulated group rates as 4.C2 measures them: Dec 31 population weights, with
# ages 0-12 (never insured) in the "under 20" denominator.
kids <- expand_grid(year = unique(long$year), age = 0:12, sex = factor(c("M", "F"), levels = c("M", "F")),
                    status = c("fully", "disability"), sim = 0)
sim_groups <- bind_rows(long_cmp, kids) |>
  inner_join(pop, by = c("year", "sex", "age")) |>
  inner_join(groups, by = join_by(between(age, lo, hi))) |>
  group_by(year, sex, status, group) |>
  summarise(sim = sum(sim * pop) / sum(pop), pop = sum(pop), .groups = "drop")

base <- sim_groups |>
  inner_join(inp$targets |> select(year, status, sex, group, rate),
             by = c("year", "sex", "status", "group")) |>
  filter(year %in% base_years) |>
  group_by(status, sex, group) |>
  summarise(factor = sum(rate * pop) / sum(sim * pop), .groups = "drop") |>
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
  gd <- tr_dis50[[as.character(yr)]] * sum(d50$pop) / sum(d50$sim * d50$base * d50$pop)
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
         rate = sim * factor) |>
  select(year, age, sex, status, sim, factor, rate) |>
  pivot_wider(names_from = status, values_from = c(sim, factor, rate)) |>
  mutate(fully = pmin(0.995, rate_fully),
         disability = pmin(rate_disability, fully)) |>
  select(year, age, sex, fully, disability, sim_fully, sim_disability,
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
  group_by(year) |> summarise(sim = 100 * sum(sim_disability * pop) / sum(pop),
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
