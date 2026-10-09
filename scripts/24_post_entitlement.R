# 24_post_entitlement.R
#
# Post-entitlement factors (methodology 4.3, "Post-Entitlement Adjustments"; DECISIONS.md PF-01 to PF-04).
#
# OCACT compares consecutive Decembers of the 1 percent MBR sample, 2014-15 to 2023-24, and
# computes by sex and duration the change in a cohort's average benefit beyond the COLA
# (recomputations for post-entitlement work, and the lower mortality of higher earners).
# The public stand-in is the Supplement's December tables by year of entitlement and sex,
# one edition per December:
#
#   PE(sex, dur, pair t-1 -> t) = MBA(cohort e, Dec t) / MBA(cohort e, Dec t-1) / (1 + COLA_t)
#   dur = t - 1 - e  (duration at the earlier December)
#   COLA_t = the COLA effective December t (params_by_year: year t); the Supplement's December
#            averages already include it (Dec 2024 $1,975 = after the 2.5% COLA)
#
#   retired workers  Table 5.B4, durations 0-12+ (12+ pooled, weighted by the earlier December's count)
#   disabled workers Table 5.D1, durations 0-9+
#   DI conversions   no public table; the retired-worker factors are used (PF-03)
#
# Composition by age at entitlement, which the model tracks itself, is divided out (PF-02).
# Initial factors = average of the latest 3 pairs; ultimate = average of the latest 10 pairs,
# OCACT's window (2014-15 to 2023-24). Women's retired ultimate = 90% men's + 10% women's.
# Initial applies to the first projection year (2026) and grades linearly to ultimate in 2045.
#
# Input:  data-raw/supplement/5b_vintages/5b_2014-2024.xlsx, supplement25_all.xlsx (December
#         2024), 2026/5b.xlsx; 5.D1 from 5d1_vintages, supplement25_all.xlsx, 2026/5d.xlsx;
#         data/params_by_year.rds, award_levels.rds (22), di_projection.rds (10), rw_entitlement_age.rds (14)
# Output: data/post_entitlement.rds

suppressMessages({library(dplyr); library(tidyr)})
source("R/read_di.R")
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))
py <- readRDS("data/params_by_year.rds") |> select(year, cola)
win <- 2015:2024            # pairs ending in these Decembers (2014-15 ... 2023-24), OCACT's window
yr0 <- 2026; yr_ult <- 2045; years <- yr0:2100

# ---- Retired workers: 5.B4 ------------------------------------------------------------------------------------
read_b4 <- function(f) {
  m <- as.matrix(readxl::read_excel(f, sheet = "5.B4", col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  dec <- as.integer(tail(regmatches(m[2, 1], gregexpr("[0-9]{4}", m[2, 1]))[[1]], 1))
  y <- suppressWarnings(as.integer(trimws(m[, 1]))); ok <- !is.na(y)
  bind_rows(tibble(dec = dec, e = y[ok], sex = "M", n = num(m[ok, 7]), mba = num(m[ok, 10])),
            tibble(dec = dec, e = y[ok], sex = "F", n = num(m[ok, 11]), mba = num(m[ok, 14])))
}
b4 <- bind_rows(lapply(c(sprintf("data-raw/supplement/5b_vintages/5b_%d.xlsx", 2014:2024),
                         "data-raw/supplement/supplement25_all.xlsx", "data-raw/supplement/2026/5b.xlsx"), read_b4))

# ---- Disabled workers: 5.D1 -----------------------------------------------------------------------------------
d1f <- c(sprintf("data-raw/supplement/5d1_vintages/5d_%d.xlsx", 2013:2016), sprintf("data-raw/supplement/5d1_vintages/5d_%d.xlsx", 2017:2024),
         "data-raw/supplement/supplement25_all.xlsx", "data-raw/supplement/2026/5d.xlsx")
d1f <- d1f[file.exists(d1f)]
dec_of <- function(f) { m <- readxl::read_excel(f, sheet = "5.D1", col_names = FALSE, col_types = "text", range = "A2:A2")[[1]]
  as.integer(tail(regmatches(m, gregexpr("[0-9]{4}", m))[[1]], 1)) }
d1 <- bind_rows(
  read_supp_5d1_pdf("data-raw/supplement/5d1_vintages/5D1_2012.pdf") |> mutate(dec = 2011L),
  bind_rows(lapply(d1f, function(f) read_supp_5d1(f) |> mutate(dec = dec_of(f))))) |>
  filter(!before) |> transmute(dec, e = ent_year, sex = as.character(sex), n = number, mba)

# ---- Consecutive-December ratios ------------------------------------------------------------------------------
ratios <- function(x, top) {
  x |> inner_join(x |> transmute(dec = dec + 1L, e, sex, n0 = n, mba0 = mba), by = c("dec", "e", "sex")) |>
    inner_join(py |> transmute(dec = year, cola), by = "dec") |>
    mutate(dur = pmin(dec - 1L - e, top)) |> filter(dur >= 0) |>
    group_by(dec, sex, dur) |>
    summarise(pe = sum(n0 * mba / mba0) / sum(n0) / (1 + first(cola) / 100), surv = sum(n) / sum(n0), .groups = "drop")
}
hist <- bind_rows(ratios(b4, 12L) |> mutate(type = "retired"), ratios(d1, 9L) |> mutate(type = "disabled"))
cat("Pairs available (December of the later edition):\n"); print(hist |> distinct(type, dec) |> group_by(type) |> summarise(decs = paste(dec, collapse = " ")))

# ---- Composition the model already tracks --------------------------------------------------------------------
# A cohort's December average also moves because its mix of ages at entitlement changes: DI workers
# entitled older (higher PIAs) leave sooner by conversion at NRA and death; retired workers entitled
# at 70 (largest benefits) die sooner. The model carries benefits by age at entitlement and removes
# those people itself, so that part is taken out of the factors (PF-02): comp = the change in the
# cohort's average award level (2025 award PIA or MBA by age at entitlement, scripts/22) implied by
# the model's own projected rolls, 2026-2031.
aw <- readRDS("data/award_levels.rds") |> filter(year == 2025)
wd <- aw |> filter(type == "disabled") |> transmute(sex = SEX, ek = age, w = pia)
wr <- aw |> filter(type == "retired") |> transmute(sex = SEX, ae = age, w = mba)
comp_of <- function(cell, top) cell |> inner_join(cell |> transmute(sex, year = year + 1L, d = d + 1L, m0 = m), by = c("sex", "year", "d")) |>
  mutate(dur = pmin(d - 1L, top)) |> group_by(sex, dur) |> summarise(comp = weighted.mean(m / m0, n), .groups = "drop")
comp <- bind_rows(
  readRDS("data/di_projection.rds")$state_by_year |> filter(year %in% 2026:2031) |> mutate(ek = pmin(pmax(e, min(wd$ek)), max(wd$ek))) |>
    inner_join(wd, by = c("sex", "ek")) |> group_by(sex, year, d) |> summarise(m = sum(n * w) / sum(n), n = sum(n), .groups = "drop") |>
    comp_of(9L) |> mutate(type = "disabled"),
  readRDS("data/rw_entitlement_age.rds")$rw_ae |> filter(class == "retired", year %in% 2026:2031) |> mutate(sex = as.character(sex), d = as.integer(age - ae)) |>
    inner_join(wr, by = c("sex", "ae")) |> group_by(sex, year, d) |> summarise(m = sum(number * w) / sum(number), n = sum(number), .groups = "drop") |>
    comp_of(12L) |> mutate(type = "retired"))

# ---- Initial (latest 3 pairs) and ultimate (10 pairs) ----------------------------------------------------------
# DI: the 5.D1 editions for December 2016-2023 aren't in hand yet; until they are, the available pairs
# before the 2024-25 one (2011-12 to 2014-15) stand in for both (PF-04).
use <- hist |> group_by(type) |> filter(if (sum(unique(dec) %in% win) >= 10) dec %in% win else dec < 2025) |> ungroup()
fac <- use |> group_by(type, sex, dur) |>
  summarise(n_pairs = n(), obs_ult = mean(pe), obs_init = mean(pe[dec %in% tail(sort(unique(dec)), 3)]), .groups = "drop") |>
  left_join(comp, by = c("type", "sex", "dur")) |>
  mutate(ultimate = obs_ult / comp, initial = obs_init / comp)
# women's retired ultimate: 90% men's + 10% women's
fac <- fac |> group_by(type, dur) |>
  mutate(ultimate = ifelse(type == "retired" & sex == "F", 0.9 * ultimate[sex == "M"] + 0.1 * ultimate, ultimate)) |> ungroup()
fac <- bind_rows(fac, fac |> filter(type == "retired") |> mutate(type = "conversion"))
cat("\nObserved (Supplement) and model-composition ratios, and the factors (observed / composition):\n")
print(as.data.frame(fac |> filter(type != "conversion") |> mutate(across(c(obs_init, obs_ult, comp, initial, ultimate), ~ round(.x, 4))) |>
                      select(type, sex, dur, n_pairs, obs_init, obs_ult, comp, initial, ultimate) |> arrange(type, sex, dur)))

# Out of sample: the 2024-25 pair against the factors
chk <- hist |> filter(dec == 2025) |> inner_join(fac |> filter(type != "conversion"), by = c("type", "sex", "dur"))
cat("\n2024-25 pair (outside OCACT's window; Social Security Fairness Act) vs the observed initial ratios, mean absolute difference by type and sex:\n")
print(chk |> group_by(type, sex) |> summarise(mad = round(mean(abs(pe - obs_init)), 4), .groups = "drop"))

# ---- Graded by year ------------------------------------------------------------------------------------------
by_year <- fac |> crossing(year = years) |> mutate(pe = initial + (ultimate - initial) * pmin(1, (year - yr0) / (yr_ult - yr0))) |>
  select(type, sex, dur, year, pe)
saveRDS(list(factors = fac, by_year = by_year, history = hist, composition = comp), "data/post_entitlement.rds")
cat("Saved data/post_entitlement.rds\n")
