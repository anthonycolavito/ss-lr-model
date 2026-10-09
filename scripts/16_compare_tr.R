# 16_compare_tr.R
#
# Every projection we make, set against what OCACT publishes for the 2026
# Trustees Report. Each comparison is tagged by how it relates to the target:
#
#   input          we take OCACT's projection as given (population, mortality,
#                  covered workers); the check is that we read it correctly
#   calibrated     fitted to this target (insured anchors, V.C5 and V.C4 totals
#                  2026-2035, the 2026 memo's DI death and recovery rates)
#   by construction  set equal to the target (V.C4/V.C5 dependent levels)
#   out of sample  not fitted: a test of the model
#
# Gaps are shown with the spread between the Trustees' low-cost and high-cost
# alternatives, as a sense of scale (that spread comes from different
# assumptions, not from model error).
#
# Input:  data/*.rds from scripts 01-15; TR single-year tables; Supplement 4.C2
# Output: data/tr_comparison.rds, outputs/tr_comparison.csv

suppressMessages({library(dplyr); library(tidyr)})
source("R/read_tr.R")

D <- function(f) readRDS(file.path("data", paste0(f, ".rds")))
years <- 2025:2100
popd <- D("population_dec") |> mutate(sex = as.character(sex), age = as.integer(age))
pop_sa <- popd |> group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop")
out <- list()
add <- function(df) out[[length(out) + 1]] <<- df
row <- function(area, measure, year, ours, ocact, status, low = NA, high = NA, unit = "count")
  tibble(area, measure, year = as.integer(year), ours, ocact, low, high, status, unit)

alt <- function(sheet, cols) read_tr_single_year_all(sheet, cols) |>
  mutate(alt = recode(section, "Historical data" = "int", "Intermediate" = "int", "Low-cost" = "low", "High-cost" = "high"))
alt_wide <- function(d, col) {
  h <- d |> filter(section == "Historical data") |> select(year, v = all_of(col))
  bind_rows(h |> mutate(alt = "int"), h |> mutate(alt = "low"), h |> mutate(alt = "high"),
            d |> filter(section != "Historical data") |> select(year, alt, v = all_of(col))) |>
    pivot_wider(names_from = alt, values_from = v)
}

# ---- 1. Inputs ----------------------------------------------------------------------------------
va3 <- alt("V.A3", c("u20", "a20_64", "a65p", "total", "dr_aged", "dr_total"))
ours_pop <- D("population_jul") |> mutate(age = as.integer(age)) |> group_by(year) |>
  summarise(total = sum(pop), a65p = sum(pop[age >= 65]))
for (v in c("total", "a65p")) {
  w <- alt_wide(va3, v)
  add(ours_pop |> inner_join(w, by = "year") |> filter(year %in% years) |>
        transmute(area = "Population", measure = ifelse(v == "total", "Population, July 1", "Population 65+, July 1"),
                  year, ours = .data[[v]], ocact = 1000 * int, low = 1000 * low, high = 1000 * high,
                  status = "input", unit = "count"))
}
va4 <- readxl::read_excel("data-raw/tr2026/SingleYearTRTables_TR2026.xlsx", sheet = "V.A4", col_names = FALSE,
                          col_types = "text", .name_repair = "minimal")[, 1:5]
names(va4) <- c("label", "e0_M", "e0_F", "e65_M", "e65_F")
va4 <- va4 |> mutate(year = suppressWarnings(as.integer(label))) |> filter(!is.na(year)) |>
  distinct(year, .keep_all = TRUE) |> mutate(across(e0_M:e65_F, tr_number))
q <- D("death_probs") |> mutate(sex = as.character(sex))
le <- function(qx, from) { l <- cumprod(c(1, 1 - qx[(from + 1):length(qx)])); sum((head(l, -1) + l[-1]) / 2) }
e65 <- q |> filter(year %in% years) |> arrange(year, sex, age) |> group_by(year, sex) |>
  summarise(e65 = le(qx, 65), .groups = "drop") |> pivot_wider(names_from = sex, values_from = e65)
add(e65 |> inner_join(va4, by = "year") |>
      transmute(area = "Population", measure = "Period life expectancy at 65, men", year, ours = M, ocact = e65_M,
                low = NA, high = NA, status = "input", unit = "years"))
add(e65 |> inner_join(va4, by = "year") |>
      transmute(area = "Population", measure = "Period life expectancy at 65, women", year, ours = F, ocact = e65_F,
                low = NA, high = NA, status = "input", unit = "years"))

# ---- 2. Insured status ------------------------------------------------------------------------------
ins <- D("insured_rates_calibrated") |> mutate(sex = as.character(sex), age = as.integer(age)) |>
  inner_join(pop_sa, by = c("year", "sex", "age"))
anchors <- tribble(~measure, ~sex, ~age, ~status_col, ~y2025, ~y2100,
  "Fully insured at 62, men", "M", 62, "fully", 0.926, 0.884,
  "Fully insured at 62, women", "F", 62, "fully", 0.885, 0.877)
for (i in seq_len(nrow(anchors))) {
  a <- anchors[i, ]
  v <- ins |> filter(sex == a$sex, age == a$age, year %in% c(2025, 2100))
  add(row("Insured", a$measure, v$year, v[[a$status_col]], c(a$y2025, a$y2100), "calibrated", unit = "rate"))
}
d50 <- ins |> filter(age == 50, year %in% c(2025, 2100)) |> group_by(year) |> summarise(r = sum(disability * pop) / sum(pop))
add(row("Insured", "Disability insured at 50", d50$year, d50$r, c(0.759, 0.774), "calibrated", unit = "rate"))
# Supplement 4.C2 by age group: 2013-2022 calibrated (C-02); 2023-2025 are OCACT estimates, not fitted
tg <- D("insured_inputs")$targets |> mutate(sex = as.character(sex))
grp <- D("insured_inputs")$target_groups
ig <- ins |> inner_join(grp, by = join_by(between(age, lo, hi))) |> group_by(year, sex, group) |>
  summarise(fully = sum(fully * pop) / sum(pop), disability = sum(disability * pop) / sum(pop), .groups = "drop")
c2 <- tg |> filter(year >= 1990, group %in% c("25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", "60_64")) |>
  inner_join(ig |> pivot_longer(c(fully, disability), names_to = "status", values_to = "ours"), by = c("year", "sex", "group", "status"))
add(c2 |> transmute(area = "Insured", measure = paste0(ifelse(status == "fully", "Fully", "Disability"), " insured ",
                                                       sub("_", "-", group), ", ", ifelse(sex == "M", "men", "women")),
                    year, ours, ocact = rate, low = NA, high = NA,
                    status = case_when(year %in% 2013:2022 ~ "calibrated", year >= 2023 ~ "out of sample", TRUE ~ "in sample"),
                    unit = "rate"))

# ---- 3. Disabled workers and their dependents (V.C5) --------------------------------------------
vc5 <- alt("V.C5", c("dw", "spouse", "child", "total", "prev_gross", "prev_adj"))
dp <- D("di_projection"); st25 <- D("di_stock_2025")
dw <- bind_rows(tibble(year = 2025L, dw = sum(st25$current_pay)), dp$flows |> transmute(year = as.integer(year), dw = stock))
w <- alt_wide(vc5, "dw")
add(dw |> inner_join(w, by = "year") |> transmute(area = "Disabled workers", measure = "Disabled workers in current pay",
      year, ours = dw, ocact = 1000 * int, low = 1000 * low, high = 1000 * high,
      status = ifelse(year <= 2035, "calibrated", "out of sample"), unit = "count"))
da <- D("di_auxiliaries")
for (g in c("spouse", "child")) {
  w <- alt_wide(vc5, g)
  m <- da$aux |> filter(group == g) |> group_by(year) |> summarise(n = sum(n), model = sum(n_model))
  add(m |> inner_join(w, by = "year") |> filter(year %in% years) |>
        transmute(area = "Disabled workers", measure = paste0("DI ", g, "ren"[g == "child"], "s"[g == "spouse"]),
                  year, ours = n, ocact = 1000 * int, low = 1000 * low, high = 1000 * high, status = "by construction", unit = "count"))
  add(da$model_vs_tr |> filter(group == g, year %in% years) |>
        transmute(area = "Model alone", measure = paste0("DI ", g, "ren"[g == "child"], "s"[g == "spouse"], " (model alone)"),
                  year = as.integer(year), ours = n, ocact = tr, low = NA, high = NA, status = "out of sample", unit = "count"))
}
# Prevalence per 1,000 disability insured: gross, and adjusted to the 2000 age-sex mix of the disability insured.
# Disability insured includes beneficiaries on the rolls (DINADD), as OCACT's denominator does.
sa <- dp$stock_age |> transmute(year = as.integer(year), sex = as.character(sex), age = as.integer(a), cp)
cp25 <- st25 |> group_by(sex, age = as.integer(attained_age)) |> summarise(cp = sum(current_pay), .groups = "drop") |>
  mutate(year = 2025L, sex = as.character(sex))
cpa <- bind_rows(cp25, sa)
nra <- D("params_by_cohort") |> transmute(b = birth_year, nra = nra_months / 12)
dins <- ins |> mutate(b = year - age) |> left_join(nra, by = "b") |> filter(age >= 15, age < coalesce(nra, 67)) |>
  transmute(year, sex, age, di_ins = disability * pop)
pv <- dins |> left_join(cpa, by = c("year", "sex", "age")) |> mutate(cp = coalesce(cp, 0))
w2000 <- pv |> filter(year == 2000) |> transmute(sex, age, w = di_ins / sum(di_ins))
prev <- pv |> filter(year %in% years) |> left_join(w2000, by = c("sex", "age")) |> group_by(year) |>
  summarise(gross = 1000 * sum(cp) / sum(di_ins), adj = 1000 * sum(coalesce(w, 0) * ifelse(di_ins > 0, cp / di_ins, 0)))
for (v in c("gross", "adj")) {
  w <- alt_wide(vc5, paste0("prev_", v))
  add(prev |> inner_join(w, by = "year") |> transmute(area = "Disabled workers",
        measure = paste0("Disabled-worker prevalence, ", ifelse(v == "gross", "gross", "age-sex-adjusted"), " (per 1,000 insured)"),
        year, ours = .data[[v]], ocact = int, low, high, status = "out of sample", unit = "per 1,000"))
}
# 2026 Disability Assumptions memo: ultimate death and recovery rates (fitted, DP-05/06)
ck <- dp$checks |> filter(year %in% c(2026, 2100))
add(row("Disabled workers", "DI death rate, age-sex-adjusted (memo)", ck$year, ck$death_adj, c(26.3, 12.5), "calibrated", unit = "per 1,000"))
add(row("Disabled workers", "DI recovery rate, age-sex-adjusted (memo)", ck$year, ck$recovery_adj, c(18.7, 11.1), "calibrated", unit = "per 1,000"))

# ---- 4. OASI beneficiaries (V.C4) -----------------------------------------------------------------
vc4 <- alt("V.C4", c("rw", "rw_spouse", "rw_child", "widow", "mother", "surv_child", "parent", "total"))
rw <- D("retired_workers")
rwt <- bind_rows(rw$hist |> filter(year == 2025) |> group_by(year) |> summarise(n = sum(rw)),
                 rw$proj |> group_by(year) |> summarise(n = sum(rw)))
w <- alt_wide(vc4, "rw")
add(rwt |> inner_join(w, by = "year") |> transmute(area = "OASI", measure = "Retired workers", year = as.integer(year),
      ours = n, ocact = 1000 * int, low = 1000 * low, high = 1000 * high,
      status = case_when(year == 2025 ~ "by construction", year <= 2035 ~ "calibrated", TRUE ~ "out of sample"), unit = "count"))
oa <- D("oasi_auxiliaries"); aw <- D("aged_widows")
lab <- c(rw_spouse = "Spouses of retired workers", rw_child = "Children of retired workers", mother = "Widowed mothers and fathers",
         surv_child = "Children of deceased workers", parent = "Parents of deceased workers")
for (g in names(lab)) {
  w <- alt_wide(vc4, g)
  m <- oa$aux |> filter(group == g) |> group_by(year) |> summarise(n = sum(n))
  add(m |> inner_join(w, by = "year") |> transmute(area = "OASI", measure = lab[[g]], year = as.integer(year), ours = n,
        ocact = 1000 * int, low = 1000 * low, high = 1000 * high, status = "by construction", unit = "count"))
  if (g != "parent") add(oa$model_vs_tr |> filter(group == g) |> transmute(area = "Model alone", measure = paste0(lab[[g]], " (model alone)"),
        year = as.integer(year), ours = model, ocact = tr, low = NA, high = NA, status = "out of sample", unit = "count"))
}
wt <- aw$aged |> group_by(year) |> summarise(aged = sum(aged)) |>
  inner_join(aw$disabled |> group_by(year) |> summarise(dis = sum(disabled)), by = "year")
w <- alt_wide(vc4, "widow")
add(wt |> inner_join(w, by = "year") |> transmute(area = "OASI", measure = "Widow(er)s, aged and disabled", year = as.integer(year),
      ours = aged + dis, ocact = 1000 * int, low = 1000 * low, high = 1000 * high, status = "by construction", unit = "count"))
wm <- aw$model |> group_by(year) |> summarise(m = sum(wid + div)) |>
  inner_join(aw$disabled |> group_by(year) |> summarise(dis = sum(disabled)), by = "year")
add(wm |> inner_join(w |> select(year, int), by = "year") |> filter(year %in% years) |>
      transmute(area = "Model alone", measure = "Widow(er)s, aged and disabled (model alone)", year = as.integer(year),
                ours = m + dis, ocact = 1000 * int, low = NA, high = NA, status = "out of sample", unit = "count"))
# OASI total
oasi_tot <- rwt |> rename(rw = n) |>
  inner_join(oa$aux |> group_by(year) |> summarise(aux = sum(n)), by = "year") |>
  inner_join(wt |> transmute(year, wid = aged + dis), by = "year") |> mutate(n = rw + aux + wid)
w <- alt_wide(vc4, "total")
add(oasi_tot |> inner_join(w, by = "year") |> transmute(area = "OASI", measure = "OASI beneficiaries, total", year = as.integer(year),
      ours = n, ocact = 1000 * int, low = 1000 * low, high = 1000 * high,
      status = ifelse(year <= 2035, "calibrated", "out of sample"), unit = "count"))

# ---- 5. All beneficiaries, mid-year (IV.B4: June 30) ------------------------------------------------
di_tot <- dw |> inner_join(da$aux |> group_by(year) |> summarise(aux = sum(n)), by = "year") |> mutate(n = dw + aux)
dec <- oasi_tot |> select(year, oasi = n) |> inner_join(di_tot |> select(year, di = n), by = "year") |> arrange(year)
jun <- dec |> mutate(oasi = (oasi + lag(oasi)) / 2, di = (di + lag(di)) / 2) |> filter(!is.na(oasi))
b4 <- alt("IV.B4", c("workers", "oasi", "di", "oasdi", "workers_per_ben", "ben_per_100"))
for (v in c("oasi", "di")) {
  w <- alt_wide(b4, v)
  add(jun |> inner_join(w, by = "year") |> transmute(area = "All beneficiaries",
        measure = paste0(toupper(v), " beneficiaries, June 30 (December average)"), year = as.integer(year), ours = .data[[v]],
        ocact = 1000 * int, low = 1000 * low, high = 1000 * high,
        status = ifelse(year <= 2035, "calibrated", "out of sample"), unit = "count"))
}
wk <- alt_wide(b4, "workers") |> select(year, workers = int)
wb <- alt_wide(b4, "ben_per_100")
add(jun |> inner_join(wk, by = "year") |> inner_join(wb, by = "year") |>
      transmute(area = "All beneficiaries", measure = "Beneficiaries per 100 covered workers", year = as.integer(year),
                ours = 100 * (oasi + di) / (1000 * workers), ocact = int, low, high,
                status = ifelse(year <= 2035, "calibrated", "out of sample"), unit = "ratio"))

# ---- 6. New retired-worker entitlements by age (Supplement 6.B5.1 actual vs model) ---------------
source("R/read_oasi.R")
t6 <- read_supp_6b51() |> filter(year >= 2021) |>
  mutate(tot = a62 + a63 + a64 + a65 + a66 + a67_69 + a70p) |>
  pivot_longer(c(a62, a63, a64, a65, a66, a67_69, a70p), names_to = "ae", values_to = "v") |>
  group_by(year, ae) |> summarise(share = sum(v) / sum(tot), .groups = "drop")
ent <- D("rw_entitlement_age")$entitlements |> filter(year %in% c(2026, 2030, 2035, 2050, 2075, 2100)) |>
  mutate(ae = case_when(ae %in% 67:69 ~ "a67_69", ae >= 70 ~ "a70p", TRUE ~ paste0("a", ae))) |>
  group_by(year, ae) |> summarise(n = sum(n), .groups = "drop") |> group_by(year) |> mutate(share = n / sum(n)) |> ungroup()

res <- bind_rows(out) |> mutate(gap = ours - ocact, gap_pct = 100 * (ours / ocact - 1),
                                in_range = ifelse(is.na(low), NA, ours >= pmin(low, high) & ours <= pmax(low, high)))
dir.create("outputs", showWarnings = FALSE)
write.csv(res, "outputs/tr_comparison.csv", row.names = FALSE)
saveRDS(list(comparison = res, entitlement_ages = list(actual = t6, model = ent)), "data/tr_comparison.rds")

# ---- Summary ------------------------------------------------------------------------------------
sel <- c(2025, 2030, 2035, 2040, 2050, 2060, 2075, 2090, 2100)
show <- res |> filter(year %in% sel, unit == "count", area != "Insured") |>
  transmute(measure, year, v = round(gap_pct, 1)) |> pivot_wider(names_from = year, values_from = v)
cat("Percent gap to the Trustees' intermediate projection:\n"); print(as.data.frame(show))
cat("\nRates and ratios (ours / OCACT):\n")
print(as.data.frame(res |> filter(unit != "count", area != "Insured" | grepl("at 62|at 50", measure), year %in% sel) |>
  transmute(measure, year, v = paste0(signif(ours, 3), " / ", signif(ocact, 3))) |> pivot_wider(names_from = year, values_from = v)))
cat("\nSupplement 4.C2, fully insured 2023-2025 (OCACT estimates, not fitted): gap in points\n")
print(as.data.frame(res |> filter(area == "Insured", grepl("^Fully insured [0-9]", measure), year %in% 2023:2025) |>
  transmute(measure, year, gap = round(100 * gap, 1)) |> pivot_wider(names_from = year, values_from = gap)))
cat("Saved outputs/tr_comparison.csv,", nrow(res), "rows\n")
