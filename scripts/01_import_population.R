# 01_import_population.R
#
# Reads OCACT's Social Security area population (December 31, 1940-2100,
# 2026 TR intermediate) and saves it as one tidy table:
#   year | age | sex | marital | pop
#
# Every later phase multiplies something by this population, so it comes first.
#
# Input:  data-raw/population/SSPopDec_Alt2_TR2026.csv
# Check:  data-raw/tr2026/SingleYearTRTables_TR2026.xlsx, Table V.A3
# Output: data/population_dec.rds

library(readr)
library(dplyr)
library(tidyr)

# ---- Step 1: read the raw file ---------------------------------------------
# One row per year x age (age 100 means 100 and older). Columns are the total,
# then for each sex: Tot, Sin(gle), Mar(ried), Wid(owed), Div(orced).

raw <- read_csv("data-raw/population/SSPopDec_Alt2_TR2026.csv",
                show_col_types = FALSE)

stopifnot(
  min(raw$Year) == 1940, max(raw$Year) == 2100,
  all(raw$Age %in% 0:100),
  nrow(raw) == (2100 - 1940 + 1) * 101
)

# ---- Step 2: check the file adds up ----------------------------------------
# The total should equal men plus women, and each sex's total should equal the
# sum of its four marital statuses. If not, a column is mislabeled.

gap_total <- with(raw, Total - `M Tot` - `F Tot`)
gap_men   <- with(raw, `M Tot` - (`M Sin` + `M Mar` + `M Wid` + `M Div`))
gap_women <- with(raw, `F Tot` - (`F Sin` + `F Mar` + `F Wid` + `F Div`))

cat("Largest gap, total vs men + women:      ", max(abs(gap_total)), "\n")
cat("Largest gap, men vs sum of marital:     ", max(abs(gap_men)), "\n")
cat("Largest gap, women vs sum of marital:   ", max(abs(gap_women)), "\n")
stopifnot(max(abs(c(gap_total, gap_men, gap_women))) <= 2)  # allow rounding

# ---- Step 3: reshape to one row per year x age x sex x marital status ------
# We drop the total columns (they can always be rebuilt by summing) and keep
# only the eight sex x marital columns.

marital_codes <- c(Sin = "single", Mar = "married", Wid = "widowed", Div = "divorced")

population <- raw |>
  select(year = Year, age = Age, matches("^[MF] (Sin|Mar|Wid|Div)$")) |>
  pivot_longer(-c(year, age), names_to = c("sex", "marital"),
               names_sep = " ", values_to = "pop") |>
  mutate(marital = factor(unname(marital_codes[marital]),
                          levels = c("single", "married", "widowed", "divorced")),
         sex = factor(sex, levels = c("M", "F"))) |>
  arrange(year, sex, marital, age)

stopifnot(nrow(population) == nrow(raw) * 8,
          sum(population$pop) == sum(raw$`M Tot` + raw$`F Tot`))

# ---- Step 4: check against the Trustees Report (Table V.A3) ----------------
# V.A3 reports July 1 population, in thousands, by broad age group. July 1 of
# year y sits halfway between December 31 of y-1 and December 31 of y, so the
# average of those two should land very close to the published figure.

# V.A3 stacks historical, intermediate, low-cost and high-cost blocks; the
# helper keeps historical + intermediate, one row per year.
source("R/read_tr.R")
va3 <- read_tr_single_year("V.A3", c("under20", "age20_64", "age65plus", "total")) |>
  select(-section)

ours_dec <- population |>
  mutate(group = cut(age, c(-1, 19, 64, Inf), labels = c("under20", "age20_64", "age65plus"))) |>
  group_by(year, group) |>
  summarise(pop = sum(pop) / 1000, .groups = "drop") |>
  pivot_wider(names_from = group, values_from = pop) |>
  mutate(total = under20 + age20_64 + age65plus)

ours_jul <- ours_dec |>
  arrange(year) |>
  mutate(across(-year, ~ (.x + lag(.x)) / 2)) |>
  filter(!is.na(total))

comparison <- inner_join(ours_jul, va3, by = "year", suffix = c("_ours", "_tr")) |>
  mutate(pct_gap_total = 100 * (total_ours / total_tr - 1),
         pct_gap_65plus = 100 * (age65plus_ours / age65plus_tr - 1))

cat("\nJuly 1 total, our estimate vs V.A3 (selected years):\n")
print(comparison |>
        filter(year %in% c(1950, 1980, 2000, 2025, 2026, 2050, 2075, 2100)) |>
        select(year, total_ours, total_tr, pct_gap_total, pct_gap_65plus),
      n = Inf)
# The average of two Decembers can't follow a jump inside a year, so a few
# historical years miss by more than rounding:
#   1946, 1950, 1956  abrupt changes in early history (demobilization,
#                     changes in who the SS area population counts)
#   2011 (65+ only)   the first baby boomers (born 1946) turn 65 mid-year
# Those are reported, not enforced. The years we project (2025 on) must match
# to within rounding.
history <- comparison |> filter(year < 2025)
projection <- comparison |> filter(year >= 2025)

cat("\nHistorical years with gaps above 0.1% (total) or 0.3% (65+):\n")
print(history |>
        filter(abs(pct_gap_total) > 0.1 | abs(pct_gap_65plus) > 0.3) |>
        select(year, total_ours, total_tr, pct_gap_total, pct_gap_65plus),
      n = Inf)

cat("\nProjection years (2025-2100), largest gap:\n")
cat("  total: ", round(max(abs(projection$pct_gap_total)), 3), "%\n")
cat("  65+:   ", round(max(abs(projection$pct_gap_65plus)), 3), "%\n")
stopifnot(max(abs(projection$pct_gap_total)) < 0.05,
          max(abs(projection$pct_gap_65plus)) < 0.25)

# ---- Save ------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
saveRDS(population, "data/population_dec.rds")
cat("\nSaved data/population_dec.rds:", nrow(population), "rows\n")
