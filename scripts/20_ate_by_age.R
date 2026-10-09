# 21_ate_by_age.R
#
# Average taxable earnings by age and sex relative to the average wage index,
# G(age, sex, year) = ATE / AWI, 1951-2100: the economy-wide earnings levels
# that OCACT's AWARDS uses to move the sample's careers to future awardees
# (methodology 4.2.1, "Earnings experience in the CWHS"). OCACT tabulates ATE by
# single age and sex from the CWHS; the public substitutes (DECISIONS.md PE-01):
#
#   2012-2023  Supplement Table 4.B13 (taxable earnings by sex and 10-year age
#              group; editions 2014-2025) / Table 4.B5 (workers by sex and age)
#   1951-2011  2012's G scaled back by the change in the median-to-AWI ratio of
#              the same group (Table 4.B6: medians by sex and 5-year group,
#              annual from 1995, selected years before, interpolated)
#   2024-2100  held at the 2019-2023 average, as OCACT holds its normalized
#              five-year average (example 3.1)
# Single ages: each group's value at its midpoint, linear between midpoints.
#
# Input:  data-raw/supplement/4b_vintages/4b_2014-2024.xlsx, supplement25_all.xlsx
#         (4.B5, 4.B6, 4.B13); data/params_by_year.rds (03)
# Output: data/ate_by_age.rds

suppressMessages({library(dplyr); library(tidyr)})
source("R/read_supplement.R")
py <- readRDS("data/params_by_year.rds") |> select(year, awi)
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))

# ---- 4.B13: taxable earnings by sex and 10-year group, 2012-2023 ----------------------------------------
g10 <- c("u20", "20_29", "30_39", "40_49", "50_59", "60_61", "62_64", "65_69", "70p")
read_b13 <- function(path, sheet = "4.B13") {
  x <- as.matrix(readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  yr <- as.integer(sub(".*(\\d{4})\\s*$", "\\1", trimws(x[2, 1])))
  rm <- which(trimws(x[, 1]) == "Men")[1]; rf <- which(trimws(x[, 1]) == "Women")[1]
  tibble(year = yr, sex = rep(c("M", "F"), each = 9), group = rep(g10, 2),
         amount = c(num(x[rm, 4:12]), num(x[rf, 4:12])) * 1e6)
}
b13 <- bind_rows(lapply(list.files("data-raw/supplement/4b_vintages", full.names = TRUE), read_b13),
                 read_b13("data-raw/supplement/supplement25_all.xlsx"))
stopifnot(all(2012:2023 %in% b13$year), !anyNA(b13$amount))

# ---- 4.B5 workers and 4.B6 medians by 5-year group ---------------------------------------------------------
b_groups <- c("u20", "20_24", "25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", "60_61", "62_64", "65_69", "70_71", "72plus")
sex_block <- c(Men = "M", Women = "F")
read_b <- function(sheet) read_supp_year_by_age(sheet, b_groups) |> filter(block %in% names(sex_block)) |>
  mutate(sex = unname(sex_block[block])) |> select(year, sex, group, value)
cw <- read_b("4.B5") |> rename(workers = value) |> mutate(workers = 1000 * workers)
med <- read_b("4.B6") |> rename(median = value)
to10 <- c(u20 = "u20", `20_24` = "20_29", `25_29` = "20_29", `30_34` = "30_39", `35_39` = "30_39", `40_44` = "40_49",
          `45_49` = "40_49", `50_54` = "50_59", `55_59` = "50_59", `60_61` = "60_61", `62_64` = "62_64",
          `65_69` = "65_69", `70_71` = "70p", `72plus` = "70p")
cw10 <- cw |> mutate(group = to10[group]) |> group_by(year, sex, group) |> summarise(workers = sum(workers), .groups = "drop")

# ATE and G, 2012-2023
G10 <- b13 |> inner_join(cw10, by = c("year", "sex", "group")) |> inner_join(py, by = "year") |>
  mutate(ate = amount / workers, G = ate / awi)

# Before 2012: the level follows the economy-wide average taxable wage per worker relative to the AWI
# (4.B2), and the pattern across age and sex follows medians relative to the all-worker median (4.B6),
# so G(g, s, y) = A(y) x R(g, s, y) x k(g, s) with k set so 2012 matches 4.B13. Medians alone would
# carry the drop of medians relative to means since the 1970s into the level (PE-01).
b2 <- as.matrix(readxl::read_excel("data-raw/supplement/supplement25_all.xlsx", sheet = "4.B2", col_names = FALSE,
                                   col_types = "text", .name_repair = "minimal"))
A <- tibble(year = suppressWarnings(as.integer(sub("\\s.*$", "", b2[, 1]))), avg = num(b2[, 9])) |>
  filter(!is.na(year), !is.na(avg)) |> inner_join(py, by = "year") |> transmute(year, A = avg / awi)
b6 <- as.matrix(readxl::read_excel("data-raw/supplement/supplement25_all.xlsx", sheet = "4.B6", col_names = FALSE,
                                   col_types = "text", .name_repair = "minimal"))
allrows <- which(trimws(b6[, 3]) == "All workers")[1]:(which(trimws(b6[, 3]) == "Men")[1] - 1)
mall <- tibble(year = suppressWarnings(as.integer(sub("\\s.*$", "", b6[allrows, 1]))), mall = num(b6[allrows, 3])) |>
  filter(!is.na(year), !is.na(mall))
R <- med |> inner_join(cw, by = c("year", "sex", "group")) |> inner_join(mall, by = "year") |>
  mutate(group = to10[group]) |> group_by(year, sex, group) |>
  summarise(R = sum(median / mall * workers) / sum(workers), .groups = "drop")
R_all <- R |> group_by(sex, group) |>
  reframe(yy = 1951:2023, R = exp(approx(year, log(R), xout = 1951:2023, rule = 2)$y)) |> rename(year = yy)
k <- G10 |> filter(year == 2012) |> select(sex, group, G) |> inner_join(R_all |> filter(year == 2012), by = c("sex", "group")) |>
  mutate(k = G / (A$A[A$year == 2012] * R)) |> select(sex, group, k)
back <- R_all |> filter(year < 2012) |> inner_join(A, by = "year") |> inner_join(k, by = c("sex", "group")) |>
  transmute(year, sex, group, G = A * R * k)
fwd_level <- G10 |> filter(year %in% 2019:2023) |> group_by(sex, group) |> summarise(G = mean(G), .groups = "drop")
fwd <- expand_grid(year = 2024:2100, fwd_level)
Gg <- bind_rows(back, G10 |> select(year, sex, group, G), fwd)

# ---- Single ages 15-69 (70 for completeness) ----------------------------------------------------------------
mid <- c(u20 = 17.5, `20_29` = 24.5, `30_39` = 34.5, `40_49` = 44.5, `50_59` = 54.5, `60_61` = 60.5, `62_64` = 63, `65_69` = 67, `70p` = 72)
G <- Gg |> mutate(m = mid[group]) |> group_by(year, sex) |>
  reframe(aa = 15:70, G = approx(m, G, xout = 15:70, rule = 2)$y) |> rename(age = aa)

cat("ATE / AWI by group, men and women, selected years:\n")
print(Gg |> filter(year %in% c(1975, 1990, 2005, 2012, 2019, 2023), group %in% c("20_29", "30_39", "40_49", "50_59", "60_61")) |>
        mutate(G = round(G, 3)) |> pivot_wider(names_from = c(sex, group), values_from = G) |> as.data.frame())
cat("\nCheck: 2023 total taxable earnings / workers vs Supplement 4.B1-based average (both sexes):",
    round(sum(G10$amount[G10$year == 2023]) / sum(G10$workers[G10$year == 2023])), "\n")

saveRDS(list(G = G, G_group = Gg, ate_2012_2023 = G10), "data/ate_by_age.rds")
cat("Saved data/ate_by_age.rds\n")
