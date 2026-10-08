# 02_import_mortality.R
#
# Reads OCACT's period death probabilities by sex and single age 0-119:
# historical (1900-2023) and projected (2024-2100, 2026 TR intermediate), and
# saves them as one continuous tidy table:
#   year | age | sex | qx | source ("historical" or "projected")
#
# qx is the probability that someone who has reached exact age x during the
# calendar year dies before reaching x + 1, at that year's mortality rates.
# We use it to age the disabled-worker and widow(er) populations and to count
# deaths of insured workers.
#
# Input:  data-raw/mortality/DeathProbsE_{M,F}_{Hist,Alt2}_TR2026.csv
# Check:  data-raw/tr2026/SingleYearTRTables_TR2026.xlsx, Table V.A4
# Output: data/death_probs.rds

library(readr)
library(dplyr)
library(tidyr)

# ---- Step 1: read the four files -------------------------------------------
# Each file has one title line, then a header row: Year, 0, 1, ..., 119.
# Historical files run 1900-2023; projected files pick up at 2024.

read_qx <- function(path, sex, source) {
  read_csv(path, skip = 1, show_col_types = FALSE) |>
    pivot_longer(-Year, names_to = "age", values_to = "qx") |>
    transmute(year = as.integer(Year), age = as.integer(age), sex = sex, qx,
              source = source)
}

death_probs <- bind_rows(
  read_qx("data-raw/mortality/DeathProbsE_M_Hist_TR2026.csv", "M", "historical"),
  read_qx("data-raw/mortality/DeathProbsE_F_Hist_TR2026.csv", "F", "historical"),
  read_qx("data-raw/mortality/DeathProbsE_M_Alt2_TR2026.csv", "M", "projected"),
  read_qx("data-raw/mortality/DeathProbsE_F_Alt2_TR2026.csv", "F", "projected")
) |>
  mutate(sex = factor(sex, levels = c("M", "F"))) |>
  arrange(year, sex, age)

# ---- Step 2: basic checks --------------------------------------------------
# Every year 1900-2100 exactly once at every age 0-119 for both sexes (so the
# historical and projected files meet without a gap or overlap); probabilities
# between 0 and 1.

stopifnot(
  min(death_probs$year) == 1900, max(death_probs$year) == 2100,
  nrow(death_probs) == (2100 - 1900 + 1) * 120 * 2,
  !anyDuplicated(death_probs[c("year", "age", "sex")]),
  all(death_probs$qx > 0 & death_probs$qx <= 1)
)
cat("Historical:", paste(range(death_probs$year[death_probs$source == "historical"]), collapse = "-"),
    "| projected:", paste(range(death_probs$year[death_probs$source == "projected"]), collapse = "-"), "\n")
cat("q at age 119, range across years and sexes:",
    range(death_probs$qx[death_probs$age == 119]), "\n")

# ---- Step 3: check against the Trustees Report (Table V.A4) ----------------
# Period life expectancy follows from the death probabilities. Start 100,000
# people at age x, survive them year by year with (1 - qx), and add up the
# person-years lived, counting deaths as living half the year:
#   e(x) = 0.5 + sum over k >= 1 of l(x + k) / l(x)
# V.A4 reports e(0) and e(65) by sex to one decimal, so we should match to
# within about 0.1 years. (OCACT treats the first year of life more precisely
# than "half a year", so e(0) may be a touch off.)

life_expectancy <- function(qx, from_age) {
  q <- qx[(from_age + 1):length(qx)]
  l <- cumprod(c(1, 1 - q))          # survivors at from_age, from_age + 1, ...
  0.5 + sum(l[-1])
}

ours <- death_probs |>
  group_by(year, sex) |>
  arrange(age, .by_group = TRUE) |>
  summarise(e0 = life_expectancy(qx, 0), e65 = life_expectancy(qx, 65),
            .groups = "drop")

va4 <- readxl::read_excel("data-raw/tr2026/SingleYearTRTables_TR2026.xlsx",
                          sheet = "V.A4", col_names = FALSE, col_types = "text",
                          .name_repair = "minimal")[, 1:5]
names(va4) <- c("label", "e0_M", "e0_F", "e65_M", "e65_F")
va4 <- va4 |>
  mutate(year = suppressWarnings(as.integer(sub("[a-z]+$", "", label)))) |>
  filter(!is.na(year)) |>
  select(-label) |>
  mutate(across(-year, as.numeric)) |>
  pivot_longer(-year, names_to = c(".value", "sex"), names_sep = "_") |>
  mutate(sex = factor(sex, levels = c("M", "F")))

comparison <- inner_join(ours, va4, by = c("year", "sex"), suffix = c("_ours", "_tr")) |>
  mutate(gap_e0 = e0_ours - e0_tr, gap_e65 = e65_ours - e65_tr)

cat("\nPeriod life expectancy, ours vs V.A4 (selected years):\n")
print(comparison |>
        filter(year %in% c(1940, 1980, 2000, 2023, 2024, 2050, 2100)) |>
        mutate(across(where(is.double), ~ round(.x, 2))),
      n = Inf)
cat("\nLargest gap, all years:  e(0)", round(max(abs(comparison$gap_e0)), 2),
    "  e(65)", round(max(abs(comparison$gap_e65)), 2), "years\n")
stopifnot(max(abs(comparison$gap_e65)) <= 0.1,
          max(abs(comparison$gap_e0)) <= 0.15)

# ---- Save ------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
saveRDS(death_probs, "data/death_probs.rds")
cat("\nSaved data/death_probs.rds:", nrow(death_probs), "rows\n")
