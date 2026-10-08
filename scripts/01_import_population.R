# 01_import_population.R
#
# Reads OCACT's Social Security area population (2026 TR intermediate) at two
# points in each year and saves each as one tidy table:
#   year | age | sex | marital | pop
#
#   December 31 (1940-2100): end-of-year stocks. Beneficiary exposures use these.
#   July 1      (1941-2100): midyear. Covered-worker rates and awards use these.
#
# Every later phase multiplies something by this population, so it comes first.
#
# Input:  data-raw/population/SSPop{Dec,Jul}_Alt2_TR2026.csv
# Check:  data-raw/tr2026/SingleYearTRTables_TR2026.xlsx, Table V.A3 (July 1)
# Output: data/population_dec.rds, data/population_jul.rds

library(readr)
library(dplyr)
library(tidyr)
source("R/read_tr.R")

# ---- Step 1: read a raw file and check it adds up --------------------------
# One row per year x age (age 100 means 100 and older). Columns are the total,
# then for each sex: Tot, Sin(gle), Mar(ried), Wid(owed), Div(orced).
# The total should equal men plus women, and each sex's total should equal the
# sum of its four marital statuses. If not, a column is mislabeled.

read_population <- function(path) {
  raw <- read_csv(path, show_col_types = FALSE)
  stopifnot(all(raw$Age %in% 0:100),
            nrow(raw) == length(unique(raw$Year)) * 101)

  gaps <- with(raw, c(Total - `M Tot` - `F Tot`,
                      `M Tot` - (`M Sin` + `M Mar` + `M Wid` + `M Div`),
                      `F Tot` - (`F Sin` + `F Mar` + `F Wid` + `F Div`)))
  cat(basename(path), ": years", min(raw$Year), "-", max(raw$Year),
      "| largest adding-up gap:", max(abs(gaps)), "\n")
  stopifnot(max(abs(gaps)) <= 2)  # allow rounding
  raw
}

# ---- Step 2: reshape to one row per year x age x sex x marital status ------
# We drop the total columns (they can always be rebuilt by summing) and keep
# only the eight sex x marital columns.

tidy_population <- function(raw) {
  marital_codes <- c(Sin = "single", Mar = "married", Wid = "widowed", Div = "divorced")
  out <- raw |>
    select(year = Year, age = Age, matches("^[MF] (Sin|Mar|Wid|Div)$")) |>
    pivot_longer(-c(year, age), names_to = c("sex", "marital"),
                 names_sep = " ", values_to = "pop") |>
    mutate(marital = factor(unname(marital_codes[marital]),
                            levels = c("single", "married", "widowed", "divorced")),
           sex = factor(sex, levels = c("M", "F"))) |>
    arrange(year, sex, marital, age)
  stopifnot(nrow(out) == nrow(raw) * 8,
            sum(out$pop) == sum(raw$`M Tot` + raw$`F Tot`))
  out
}

population_dec <- tidy_population(read_population("data-raw/population/SSPopDec_Alt2_TR2026.csv"))
population_jul <- tidy_population(read_population("data-raw/population/SSPopJul_Alt2_TR2026.csv"))

# ---- Step 3: check July 1 against the Trustees Report (Table V.A3) ---------
# V.A3 reports July 1 population, in thousands, by broad age group. Our July
# file is the same concept, so the only differences should be rounding to the
# nearest thousand.

va3 <- read_tr_single_year("V.A3", c("under20", "age20_64", "age65plus", "total")) |>
  select(-section)

ours <- population_jul |>
  mutate(group = cut(age, c(-1, 19, 64, Inf), labels = c("under20", "age20_64", "age65plus"))) |>
  group_by(year, group) |>
  summarise(pop = sum(pop) / 1000, .groups = "drop") |>
  pivot_wider(names_from = group, values_from = pop) |>
  mutate(total = under20 + age20_64 + age65plus)

comparison <- inner_join(ours, va3, by = "year", suffix = c("_ours", "_tr")) |>
  mutate(gap_total = total_ours - total_tr,
         gap_under20 = under20_ours - under20_tr,
         gap_20_64 = age20_64_ours - age20_64_tr,
         gap_65plus = age65plus_ours - age65plus_tr)

cat("\nJuly 1 population (thousands), ours vs V.A3, selected years:\n")
print(comparison |>
        filter(year %in% c(1950, 1980, 2000, 2025, 2050, 2075, 2100)) |>
        select(year, total_ours, total_tr, gap_total, gap_65plus) |>
        mutate(across(where(is.double), ~ round(.x, 1))),
      n = Inf)

largest <- max(abs(unlist(comparison[c("gap_total", "gap_under20", "gap_20_64", "gap_65plus")])))
cat("\nLargest gap in any year and age group:", round(largest, 2), "thousand people\n")
stopifnot(largest <= 2)  # V.A3 is rounded to the nearest thousand

# ---- Step 4: the two files should agree with each other --------------------
# July 1 of year y sits between December 31 of y-1 and y. The average of those
# two Decembers is only an approximation (a cohort-size jump mid-year breaks
# it), but across all ages it should be very close.

dec_total <- population_dec |> group_by(year) |> summarise(dec = sum(pop))
jul_total <- population_jul |> group_by(year) |> summarise(jul = sum(pop))
consistency <- jul_total |>
  inner_join(dec_total |> mutate(year = year + 1, prev = dec) |> select(year, prev), by = "year") |>
  inner_join(dec_total, by = "year") |>
  mutate(pct_gap = 100 * ((prev + dec) / 2 / jul - 1)) |>
  filter(year >= 2025)
cat("Projection years: average of Decembers vs July, largest gap:",
    round(max(abs(consistency$pct_gap)), 3), "%\n")
stopifnot(max(abs(consistency$pct_gap)) < 0.05)

# ---- Save ------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
saveRDS(population_dec, "data/population_dec.rds")
saveRDS(population_jul, "data/population_jul.rds")
cat("\nSaved data/population_dec.rds:", nrow(population_dec), "rows\n")
cat("Saved data/population_jul.rds:", nrow(population_jul), "rows\n")
