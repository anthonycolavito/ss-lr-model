# 04_insured_inputs.R
#
# Builds the inputs to the insured-status simulation (methodology section 3.1).
# OCACT simulates work histories for each birth cohort and sex from three
# ingredients, all by single age, sex and calendar year:
#
#   1. covered-worker rate   share of the population with covered earnings
#   2. median earnings       of covered workers, to place a worker's earnings
#                            against the quarter-of-coverage (QC) threshold
#   3. QC amount             earnings needed for one quarter of coverage
#
# plus one fixed distribution, FRAC: the share of covered workers earning less
# than a given multiple of the median. We also assemble the historical insured
# rates (Supplement 4.C2) that the simulation is calibrated to.
#
# Sources
#   Covered workers by age group and sex   Supplement 4.B5 (1937-2023)
#   Median earnings by age group and sex   Supplement 4.B6 (1937-2023)
#   Projected total covered workers        TR Table IV.B4 (2024-2100)
#   July 1 population                      data/population_jul.rds (script 01)
#   AWI, QC amounts                        data/params_by_year.rds (script 03)
#   Earnings distribution                  OCACT wage statistics, 2023
#   Insured workers by age group and sex   Supplement 4.C2 (1970-2025)
#
# Output: data/insured_inputs.rds (a list of tables)

library(dplyr)
library(tidyr)
source("R/read_tr.R")
source("R/read_supplement.R")

years <- 1937:2100    # population (and so the model) ends in 2100
ages  <- 13:84          # OCACT simulates work histories from age 13 to 84

pop_jul <- readRDS("data/population_jul.rds")
pop_dec <- readRDS("data/population_dec.rds")
params  <- readRDS("data/params_by_year.rds")

# ---- Age groups used by Supplement 4.B5 / 4.B6 ------------------------------
# Each group's rate is applied to every single age in it. "Under 20" is spread
# over ages 14-19 (almost no one has covered earnings at 13) and "72 or older"
# over 72-84, the oldest age simulated.

b_groups <- tibble(
  group = c("u20", "20_24", "25_29", "30_34", "35_39", "40_44", "45_49", "50_54",
            "55_59", "60_61", "62_64", "65_69", "70_71", "72plus"),
  lo = c(14, 20, 25, 30, 35, 40, 45, 50, 55, 60, 62, 65, 70, 72),
  hi = c(19, 24, 29, 34, 39, 44, 49, 54, 59, 61, 64, 69, 71, 84)
)
age_to_bgroup <- tibble(age = ages) |>
  left_join(b_groups, by = join_by(between(age, lo, hi))) |>
  select(age, group)

sex_block <- c(Men = "M", Women = "F")

# ---- Step 1: covered workers and medians by age group (history) -------------

read_b <- function(sheet, value_name) {
  read_supp_year_by_age(sheet, b_groups$group) |>
    filter(block %in% names(sex_block)) |>
    mutate(sex = factor(unname(sex_block[block]), levels = c("M", "F"))) |>
    select(year, sex, group, !!value_name := value)
}
cw_hist  <- read_b("4.B5", "covered_thousands")
med_hist <- read_b("4.B6", "median")
cat("Supplement 4.B5/4.B6 years:", paste(range(cw_hist$year), collapse = "-"),
    "(", length(unique(cw_hist$year)), "years; selected years before 1995 )\n")

# ---- Step 2: covered-worker rates by group, history -------------------------
# Rate = covered workers / July 1 population of the ages the group covers.
# July population starts in 1941; 1937-40 use 1941.

pop_group <- pop_jul |>
  group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop") |>
  inner_join(age_to_bgroup, by = "age") |>
  group_by(year, sex, group) |> summarise(pop = sum(pop), .groups = "drop")
pop_group <- bind_rows(
  pop_group,
  expand_grid(year = 1937:1940, pop_group |> filter(year == 1941) |> select(-year))
)

rate_hist <- cw_hist |>
  inner_join(pop_group, by = c("year", "sex", "group")) |>
  mutate(rate = 1000 * covered_thousands / pop)

# ---- Step 3: fill every year 1937-2023 by linear interpolation --------------
# 4.B5 gives selected years before 1995; rates in between are interpolated.

fill_years <- function(d, value, all_years) {
  d |>
    filter(!is.na(.data[[value]])) |>
    group_by(sex, group) |>
    group_modify(~ tibble(year = all_years,
                          "{value}" := approx(.x$year, .x[[value]], xout = all_years, rule = 2)$y)) |>
    ungroup()
}
rate_hist_all <- fill_years(rate_hist, "rate", 1937:2023)

# ---- Step 4: project covered-worker rates, 2024-2100 ------------------------
# Hold each age-sex rate at its 2023 level, then scale all rates in a year by a
# single factor so total covered workers match TR Table IV.B4. (4.B5 and IV.B4
# measure the same thing; we carry their small 2023 difference forward so the
# series joins smoothly.) The factor stays close to 1; drift away from 1 would
# mean the TR's age-sex pattern of work is changing.

ivb4 <- read_tr_single_year("IV.B4", c("covered_workers", "oasi", "di", "oasdi", "ratio", "per100")) |>
  select(year, covered_workers)
link <- sum(cw_hist$covered_thousands[cw_hist$year == 2023]) /
  ivb4$covered_workers[ivb4$year == 2023]

base_rate <- rate_hist_all |> filter(year == 2023) |> select(sex, group, rate)
proj <- expand_grid(year = 2024:2100, base_rate) |>
  inner_join(pop_group, by = c("year", "sex", "group")) |>
  group_by(year) |>
  mutate(implied = sum(rate * pop) / 1000) |>
  ungroup() |>
  left_join(ivb4, by = "year")
target <- proj |> distinct(year, implied, covered_workers) |> arrange(year) |>
  mutate(target = link * covered_workers) |>
  fill(target) |>
  mutate(scale = target / implied)
proj <- proj |> inner_join(target |> select(year, scale), by = "year") |>
  mutate(rate = rate * scale)

cat("Projection scale factor on 2023 rates: range",
    paste(round(range(target$scale), 3), collapse = " to "), "\n")

covered_rate <- bind_rows(rate_hist_all, proj |> select(sex, group, year, rate)) |>
  inner_join(age_to_bgroup, by = "group", relationship = "many-to-many") |>
  select(year, age, sex, rate)

# Within "under 20", work rises steeply with age; spreading the group rate
# evenly would put a 14-year-old at the same rate as a 19-year-old. We reshape
# ages 14-19 using the Trustees' employment-to-population ratios by age group
# (Actuarial Study No. 127, 2022 TR, Tables 1-2: 1981-2096 by sex):
#   ages 16-17  weight = the 16-17 ratio for that year and sex
#   ages 18-19  weight = the 18-19 ratio
#   ages 14-15  not covered by the labor survey (it starts at 16); assumed
#               0.15 and 0.40 times the 16-17 ratio
# Years before 1981 use 1981's shape. The weights are then rescaled so the
# group's total covered workers match Supplement 4.B5 exactly.
#
# Caveat: the ratios are point-in-time (an average month), while a covered
# worker is anyone with earnings at any time in the year. Rescaling to the
# 4.B5 total applies the same annual-vs-monthly uplift at every teen age;
# summer jobs probably make that uplift larger at 16-17 than at 18-19.
source("R/read_studies.R")
as127 <- read_as127_employment() |> filter(group %in% c("16_17", "18_19"))
teen_ratio <- as127 |>
  pivot_wider(names_from = group, values_from = ratio) |>
  right_join(expand_grid(year = years, sex = factor(c("M", "F"), levels = c("M", "F"))),
             by = c("year", "sex")) |>
  group_by(sex) |> arrange(year) |>
  fill(`16_17`, `18_19`, .direction = "updown") |>
  ungroup()
teen_weights <- teen_ratio |>
  reframe(year, sex,
          age = list(14:19),
          w = Map(function(a, b) c(0.15 * a, 0.40 * a, a, a, b, b), `16_17`, `18_19`)) |>
  unnest(c(age, w))

teen_pop <- pop_jul |>
  filter(age %in% 14:19) |>
  group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop")
teen_pop <- bind_rows(teen_pop,
                      expand_grid(year = 1937:1940, teen_pop |> filter(year == 1941) |> select(-year)))
teen_adj <- teen_pop |>
  inner_join(teen_weights, by = c("year", "sex", "age")) |>
  group_by(year, sex) |>
  mutate(adj = w * sum(pop) / sum(w * pop)) |>   # keeps sum(rate * pop) unchanged
  ungroup() |>
  select(year, sex, age, adj)
covered_rate <- covered_rate |>
  left_join(teen_adj, by = c("year", "sex", "age")) |>
  mutate(rate = ifelse(is.na(adj), rate, pmin(rate * adj, 0.95))) |>
  select(-adj)
covered_rate <- bind_rows(
  covered_rate,
  expand_grid(year = years, age = 13L, sex = factor(c("M", "F"), levels = c("M", "F")), rate = 0)
) |> arrange(sex, year, age)
stopifnot(all(covered_rate$rate >= 0 & covered_rate$rate < 1),
          nrow(covered_rate) == length(years) * length(ages) * 2)

# ---- Step 5: median earnings by single age, 1937-2100 -----------------------
# Express each group's median as a ratio to the AWI, interpolate the ratio
# between published years, and hold the 2023 ratio for later years (so medians
# grow with the AWI, as do QC amounts: the QC-to-median ratio stays fixed).

awi <- params |> select(year, awi)
med_ratio <- med_hist |>
  inner_join(awi, by = "year") |>
  mutate(ratio = median / awi) |>
  fill_years("ratio", 1937:2023)
med_ratio <- bind_rows(
  med_ratio,
  expand_grid(year = 2024:2100, med_ratio |> filter(year == 2023) |> select(-year))
)
median_earnings <- med_ratio |>
  inner_join(awi, by = "year") |>
  mutate(median = ratio * awi) |>
  inner_join(age_to_bgroup, by = "group", relationship = "many-to-many") |>
  select(year, age, sex, median)
median_earnings <- bind_rows(
  median_earnings,
  median_earnings |> filter(age == 14) |> mutate(age = 13L)
) |> arrange(sex, year, age)

# ---- Step 6: QC amounts, 1937-2100 ------------------------------------------
# Annual QC amounts start in 1978 ($250). Before then, coverage was credited
# by calendar quarter ($50 of wages in a quarter), which an annual-earnings
# simulation can't reproduce. Following OCACT (methodology 4.2.b, input 19),
# we estimate a 1937-77 annual equivalent by running the indexing rule
# backwards: $250 scaled by the AWI two years earlier relative to 1976.

qc <- params |>
  select(year, awi, qc_amount) |>
  mutate(awi_lag2 = lag(awi, 2, default = awi[1]),
         qc_amount = ifelse(year < 1978, 250 * awi_lag2 / awi[year == 1976], qc_amount)) |>
  select(year, qc_amount)

# ---- Step 7: earnings distribution relative to the median (FRAC) ------------
# From OCACT's 2023 distribution of wage earners by net compensation: the
# cumulative share of workers at each bracket's upper edge, against earnings
# as a multiple of the median ($43,222.81). Below the first edge ($5,000,
# 0.116 x median) we extend the curve as a power function fitted to the first
# two points. The same shape is used for both sexes and all years; sex and age
# differences enter through the medians.

wd <- readxl::read_excel("data-raw/oact_wages/wage_earner_distribution_2023.xlsx",
                         col_names = FALSE, col_types = "text", .name_repair = "minimal")
names(wd) <- c("interval", "number", "cumulative", "percent", "aggregate", "average")[seq_along(wd)]
wd <- wd |>
  mutate(number = suppressWarnings(as.numeric(number)),
         percent = suppressWarnings(as.numeric(percent))) |>
  filter(!is.na(number), !is.na(percent)) |>
  # upper edge = the last number in the label ("5,000.00 - 9,999.99");
  # the open-ended top bracket ("50,000,000.00 and over") has none
  mutate(upper = ifelse(grepl("over", interval), NA,
                        tr_number(sub("^.*[^0-9,.]([0-9,.]+)$", "\\1", interval)) + 0.01))
median_2023 <- 43222.81
frac_points <- wd |>
  filter(!is.na(upper), is.finite(upper)) |>
  transmute(ratio = upper / median_2023, cum_share = percent / 100)
stopifnot(abs(approx(frac_points$ratio, frac_points$cum_share, xout = 1)$y - 0.5) < 0.02)

k <- log(frac_points$cum_share[2] / frac_points$cum_share[1]) /
  log(frac_points$ratio[2] / frac_points$ratio[1])
cat("Earnings distribution: share below 1x median",
    round(approx(frac_points$ratio, frac_points$cum_share, xout = 1)$y, 3),
    "| low-end power", round(k, 3), "\n")

# ---- Step 8: calibration targets, Supplement 4.C2 ---------------------------
# Fully insured and disability insured workers by age group and sex, Dec 31,
# 1970-2025, as rates of the Dec 31 population in the same group.

c_groups <- tibble(
  group = c("u20", "20_24", "25_29", "30_34", "35_39", "40_44", "45_49", "50_54",
            "55_59", "60_64", "65_69", "70_74", "75plus"),
  lo = c(0, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75),
  hi = c(19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 100)
)
c2 <- read_supp_year_by_age("4.C2", c_groups$group, top = "^(Fully|Insured)") |>
  filter(!grepl("/ Total$", block)) |>
  mutate(status = ifelse(grepl("^Fully", block), "fully", "disability"),
         sex = factor(ifelse(grepl("/ Male$", block), "M", "F"), levels = c("M", "F"))) |>
  select(year, status, sex, group, insured_thousands = value)

pop_c <- pop_dec |>
  group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop") |>
  inner_join(c_groups, by = join_by(between(age, lo, hi))) |>
  group_by(year, sex, group) |> summarise(pop = sum(pop), .groups = "drop")

insured_targets <- c2 |>
  filter(!is.na(insured_thousands)) |>
  inner_join(pop_c, by = c("year", "sex", "group")) |>
  mutate(rate = 1000 * insured_thousands / pop)

cat("\nFully insured rate, Supplement 4.C2, 2025:\n")
print(insured_targets |> filter(year == 2025, status == "fully") |>
        select(sex, group, rate) |> mutate(rate = round(rate, 3)) |>
        pivot_wider(names_from = sex, values_from = rate))

# ---- Save ------------------------------------------------------------------
insured_inputs <- list(
  covered_rate = covered_rate,       # year, age, sex, rate
  median_earnings = median_earnings, # year, age, sex, median ($)
  qc_amount = qc,                    # year, qc_amount ($)
  frac_points = frac_points,         # ratio to median, cumulative share
  frac_low_power = k,
  targets = insured_targets,         # year, status, sex, group, rate
  target_groups = c_groups
)
saveRDS(insured_inputs, "data/insured_inputs.rds")
cat("\nSaved data/insured_inputs.rds\n")
