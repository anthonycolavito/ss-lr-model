# 17_bepuf_aime.R
#
# AIMEs of recent new worker beneficiaries from the BEPUF 2020 earnings
# histories (Phase 4, methodology 4.2: OCACT uses a 10% sample of 2022 awardees
# from the MBR with their MEF earnings; we use the public synthetic file).
#
# Sample (data-raw/bepuf/, built by extract_bepuf_earnings.R): worker
# beneficiaries in December 2020 entitled 2016-2020 (birth year + age at
# entitlement): retired workers one in four by ID, all disabled workers.
# BEPUF puts every birthday on January 1, so ages and calendar years line up.
# The file's AIME and PIA columns are not used: SSA withdrew them (April 2026
# errata) and they don't match the earnings records (DECISIONS.md PB-02).
#
# AIME (ranypia):
#   retired workers  eligibility year = birth year + 62; 35 computation years
#   disabled workers eligibility year = year of entitlement (onset isn't in the
#                    file); computation years for disability
#   earnings counted through the year before entitlement (award basis)
# Each AIME is also expressed relative to its eligibility year's first bend
# point, which is how it enters the PAPs (1979 dollars x AIME / BP1).
#
# Check (this script): map each record to a 2025 award with the same age at
# entitlement (same relative AIME, 2025-award bend points and COLAs) and compare
# the PIA distribution with Supplement 2026 Tables 6.B4 (retired-worker awards
# by PIA, by sex, with and without reduction) and 6.C1 (disabled-worker awards
# by monthly benefit).
#
# Input:  data-raw/bepuf/BEPUF-2020-benefits.csv, bepuf_awardee_earnings.csv.gz;
#         data/params_by_year.rds, data/params_by_cohort.rds (03)
# Output: data/bepuf_awardees.rds, outputs/bepuf_vs_awards_2025.csv

suppressMessages({library(data.table); library(dplyr); library(tidyr); library(ranypia)})

b <- fread("data-raw/bepuf/BEPUF-2020-benefits.csv", showProgress = FALSE,
           select = c("ID", "BY", "SEX", "BT", "IP", "BT2", "Redux", "DRC", "ACE"))
setnames(b, trimws(gsub("^﻿", "", names(b))))
b[, ent := BY + ACE]
sel <- b[BT == "A" & IP %in% c("R", "D") & ent %in% 2016:2020 & (IP == "D" | ID %% 4 == 0)]
sel[, `:=`(Redux = Redux %in% c("T", TRUE), DRC = DRC %in% c("T", TRUE), di = IP == "D")]
sel[, grp := fifelse(di, "DI", fifelse(Redux & !DRC, "reduced", fifelse(DRC & !Redux, "DRC",
                     fifelse(!Redux & !DRC, "at FRA", "reduced and DRC"))))]
setorder(sel, ID); sel[, row := .I]
e <- fread("data-raw/bepuf/bepuf_awardee_earnings.csv.gz", showProgress = FALSE)
stopifnot(all(sel$ID %in% e$ID))
yrs <- 1951:2020
M <- matrix(0, nrow(sel), length(yrs), dimnames = list(NULL, yrs))
e <- e[sel[, .(ID, row)], on = "ID", nomatch = 0]
M[cbind(e$row, e$YEAR - 1950)] <- e$EARNINGS
cat("Records:", nrow(sel), " earnings rows:", nrow(e), "\n")

py <- as.data.table(readRDS("data/params_by_year.rds"))
sel[, elig := fifelse(di, ent, BY + 62L)]
sel[, cy := computation_years(BY, 1, elig, birth_day = 1, disabled = di)]
sel[, aime := aime(M, elig, cy, first_year = 1951, last_year = ent - 1)]
sel[, bp1 := py$pia_bp1[match(elig, py$year)]]
sel[, rel := aime / bp1]                      # AIME in units of the first bend point
sel[, aime79 := 180 * rel]                    # 1979 dollars, as the PAP intervals are
cat("AIME, 1979 dollars, by group and sex (median / mean):\n")
print(sel[, .(n = .N, median = round(median(aime79)), mean = round(mean(aime79)), zero = round(mean(aime == 0), 4)),
          by = .(grp, SEX)][order(grp, SEX)])

# ---- Map to 2025 awards and compare with Supplement 6.B4 / 6.C1 ------------------------------------
pia_formula <- function(a, bp1, bp2) floor(10 * (0.9 * pmin(a, bp1) + 0.32 * pmax(0, pmin(a, bp2) - bp1) + 0.15 * pmax(0, a - bp2))) / 10
colas <- function(from) sapply(from, function(y) if (y > 2024) 1 else prod(1 + py$cola[py$year %in% y:2024] / 100))
m25 <- sel[, .(ID, SEX, grp, di, ACE, rel)]
m25[, elig25 := fifelse(di, 2024L, 2025L - (ACE - 62L))]
m25[, `:=`(bp1 = py$pia_bp1[match(elig25, py$year)], bp2 = py$pia_bp2[match(elig25, py$year)])]
m25[, pia25 := floor(10 * pia_formula(rel * bp1, bp1, bp2) * colas(elig25)) / 10]
brk <- c(-Inf, seq(300, 3300, 100), Inf)
lab <- c("<300", paste0(seq(300, 3200, 100), "-", seq(399.9, 3299.9, 100)), "3300+")
m25[, bin := cut(pia25, brk, labels = lab, right = FALSE)]

read_bins <- function(path, sheet, block_rows, cols) {
  x <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  out <- lapply(names(block_rows), function(nm) {
    r <- block_rows[[nm]]
    tibble(sex = nm, bin = lab, !!!lapply(cols, function(cc) as.numeric(gsub("[^0-9.]", "", x[[cc]][r]))))
  })
  bind_rows(out)
}
b4 <- read_bins("data-raw/supplement/2026/6b.xlsx", "6.B4", list(T = 6:37, M = 40:71, F = 74:105),
                c(total = 4, reduced = 6, unreduced = 8))
c1 <- read_bins("data-raw/supplement/2026/6c.xlsx", "6.C1", list(T = 6:37), c(total = 3, M = 5, F = 7))
stopifnot(b4$bin[1] == "<300", nrow(b4) == 96)

share <- function(d) d[, .N, by = bin][, .(bin, model = N / sum(N))]
cmp <- bind_rows(
  lapply(c("M", "F"), function(s) {
    bind_rows(
      share(m25[!di & grp == "reduced" & SEX == s]) |> as_tibble() |> mutate(type = "Retired, reduced", sex = s) |>
        inner_join(b4 |> filter(sex == s) |> transmute(bin, actual = reduced / sum(reduced)), by = "bin"),
      share(m25[!di & grp != "reduced" & SEX == s]) |> as_tibble() |> mutate(type = "Retired, not reduced", sex = s) |>
        inner_join(b4 |> filter(sex == s) |> transmute(bin, actual = unreduced / sum(unreduced)), by = "bin"),
      share(m25[di & SEX == s]) |> as_tibble() |> mutate(type = "Disabled", sex = s) |>
        inner_join(c1 |> transmute(bin, actual = .data[[s]] / sum(.data[[s]])), by = "bin"))
  })) |> mutate(bin = factor(bin, levels = lab)) |> arrange(type, sex, bin)

summ <- cmp |> group_by(type, sex) |>
  summarise(mean_model = sum(model * c(250, seq(350, 3250, 100), 3600)[as.integer(bin)]),
            mean_actual = sum(actual * c(250, seq(350, 3250, 100), 3600)[as.integer(bin)]),
            top_model = model[bin == "3300+"], top_actual = actual[bin == "3300+"],
            under_1000_model = sum(model[as.integer(bin) <= 8]), under_1000_actual = sum(actual[as.integer(bin) <= 8]),
            ks = max(abs(cumsum(model) - cumsum(actual))), .groups = "drop")
cat("\n2025 awards, model (BEPUF careers mapped to 2025) vs Supplement 6.B4/6.C1:\n")
print(summ |> mutate(across(where(is.numeric), ~ round(.x, 3))), width = 200)
avg <- m25[, .(avg_pia = round(mean(pia25))), by = .(type = fifelse(di, "Disabled", fifelse(grp == "reduced", "Retired, reduced", "Retired, not reduced")), SEX)]
cat("\nAverage PIA, model mapped to 2025 (6.B4 averages: reduced M 2352 F 1861; not reduced M 2642 F 2067; 6.C1 average benefit M 1989 F 1612):\n")
print(avg[order(type, SEX)])

dir.create("outputs", showWarnings = FALSE)
write.csv(cmp, "outputs/bepuf_vs_awards_2025.csv", row.names = FALSE)
saveRDS(list(awardees = sel[, .(ID, BY, SEX, IP, grp, BT2, ACE, ent, elig, cy, aime, rel, aime79)],
             earnings = M, vs_2025 = cmp), "data/bepuf_awardees.rds")
cat("Saved data/bepuf_awardees.rds\n")
