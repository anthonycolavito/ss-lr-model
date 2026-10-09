# 12_aged_widows.R
#
# Aged widow(er)s (methodology 3.3, equation 3.3.1) and disabled widow(er)s,
# 2012-2100, by single age, sex of the beneficiary and marital status:
#
#   ASDW = population (widowed, divorced) x P(account holder deceased)
#          x P(account holder fully insured at death) x residual
#
# Linkages (DECISIONS.md AW-01 to AW-06)
#   deceased   widowed: 1; divorced: widowed / (widowed + married) at the same
#              age and sex (methodology).
#   insured    OCACT's weighting over the spouse's age: for a widow aged AW the
#              husband is AW-6 to AW+12, weight 10 - |AW + 3 - AH|; for a
#              widower aged AH the wife is AH-12 to AH+6, weight
#              10 - |AH - 3 - AW|; ages bounded to 60-100; fully insured rates
#              of the spouse's sex that year (scripts/07).
#   residual   everything else (remarriage, 10-year marriage for divorced,
#              insured status of the widow(er) and whether she draws her own
#              benefit, government pension offset): history 2012-2025 from
#              Supplement 5.A1.6 by age and sex, split by marital status to
#              each year's published totals; least-squares trend over the last
#              ten years with a 2020-21 dummy, evaluated at 2025, graduated over
#              age; held constant.
#   2026-2035  one factor per year (all ages, both sexes) so aged + disabled
#              widow(er)s match TR V.C4; the 2035 factor grades to 1 by 2045
#              (methodology); 2046-2100 is the out-of-sample test.
#   Levels     the saved projection uses V.C4's widow(er) total every year,
#              split by the model (AW-06, F-15).
# OCACT also splits insured and uninsured widow(er)s and models the SSFA's
# end of the government pension offset explicitly; the counts needed aren't
# published, so both are in the residual and the 2026-2035 match (AW-04).
# Disabled widow(er)s (50-66): 2025 rates per widowed + divorced person by
# age and sex, held (AW-05).
#
# Input:  data/population_dec.rds (01), data/insured_rates_calibrated.rds (07),
#         Supplement 5.A1.6 and 5.A1.7 (December 2012-2025), TR V.C4
# Output: data/aged_widows.rds, outputs/aged_widows_checks.csv

library(dplyr)
library(tidyr)
source("R/read_tr.R")
source("R/read_oasi.R")

sexes <- c("M", "F")
hist_years <- 2012:2025
years <- 2012:2100

popm <- readRDS("data/population_dec.rds") |>
  mutate(sex = as.character(sex), age = as.integer(age), year = as.integer(year), marital = as.character(marital))
pop_ms <- popm |> filter(year %in% years) |> select(year, sex, age, marital, pop) |>
  pivot_wider(names_from = marital, values_from = pop, values_fill = 0)
fi <- readRDS("data/insured_rates_calibrated.rds") |> mutate(sex = as.character(sex)) |>
  filter(year %in% years) |> select(year, sex, age, fully)
vc4 <- read_tr_single_year("V.C4", c("rw", "rw_spouse", "rw_child", "widow", "mother", "surv_child", "parent", "total"))

# ---- Linkage: spouse fully insured at death (OCACT's age weights) ----------------------------
FI <- array(0, c(2, 121, length(years)), dimnames = list(sexes, 0:120, years))
FI[cbind(match(fi$sex, sexes), fi$age + 1, fi$year - min(years) + 1)] <- fi$fully
p_fia <- function(sex_b, a, t) {
  if (sex_b == "F") { sp <- "M"; s_ages <- (a - 6):(a + 12); w <- 10 - abs(a + 3 - s_ages) }
  else              { sp <- "F"; s_ages <- (a - 12):(a + 6); w <- 10 - abs(a - 3 - s_ages) }
  s_ages <- pmin(100, pmax(60, s_ages)); w <- pmax(0, w)
  sum(w * FI[sp, s_ages + 1, t - min(years) + 1]) / sum(w)
}
link <- expand_grid(year = years, sex = sexes, age = 60:100) |>
  mutate(p_fia = mapply(p_fia, sex, age, year)) |>
  inner_join(pop_ms, by = c("year", "sex", "age")) |>
  inner_join(fi |> rename(fi_own = fully), by = c("year", "sex", "age")) |>
  transmute(year, sex, age, fi_own,
            exp_wid = widowed * p_fia,
            exp_div = divorced * widowed / pmax(1, widowed + married) * p_fia)

# ---- History: aged widow(er)s by age, sex and marital status --------------------------------
files <- oasi_vintage_files()
spread <- function(d) {
  single <- d |> filter(age == age_hi) |> select(year, age, sex, number)
  grp <- d |> filter(age_hi > age) |> mutate(g = paste(age, age_hi)) |> rowwise() |>
    reframe(year, sex, g, number, age = age:min(age_hi, 100)) |>
    left_join(link |> transmute(year, sex, age, e = exp_wid + exp_div), by = c("year", "sex", "age")) |>
    group_by(year, sex, g) |> mutate(number = number * e / sum(e)) |> ungroup() |> select(year, age, sex, number)
  bind_rows(single, grp)
}
aw_hist <- bind_rows(lapply(files, read_supp_5a16)) |> mutate(sex = as.character(sex)) |> spread()
aw_mar <- bind_rows(lapply(files, read_supp_widow_marital)) |> mutate(sex = as.character(sex))
dw_hist <- bind_rows(lapply(files, function(f) read_supp_5a16(f, "5.A1.7"))) |> mutate(sex = as.character(sex)) |>
  select(year, age, sex, number)
stopifnot(all(sort(unique(aw_hist$year)) == hist_years))

# Split each age by marital status in proportion to exposure, scaled so each
# year's divorced total matches 5.A1.6 (AW-03).
split_marital <- function(h) {
  d <- h |> inner_join(link, by = c("year", "sex", "age"))
  tot_div <- aw_mar |> filter(marital == "Divorced") |> select(year, sex, div_total = number)
  d |> inner_join(tot_div, by = c("year", "sex")) |> group_by(year, sex) |>
    group_modify(function(g, k) {
      f <- function(kk) sum(g$number * kk * g$exp_div / (g$exp_wid + kk * g$exp_div)) - g$div_total[1]
      kk <- uniroot(f, c(1e-4, 1e4))$root
      g |> mutate(div = number * kk * exp_div / (exp_wid + kk * exp_div), wid = number - div)
    }) |> ungroup() |>
    select(year, sex, age, wid, div, exp_wid, exp_div, fi_own)
}
# Insured / uninsured (AW-04): widow(er)s insured on their own record and
# entitled to their own benefit are counted as retired workers, so past 70 all
# widow(er) beneficiaries are taken as uninsured. Uninsured residual =
# count / (exposure x (1 - own fully insured)) at 71+, carried flat from 71
# back to 60-70; the insured residual (60-70: insured widow(er)s still
# delaying their own benefit) takes the rest.
hist <- split_marital(aw_hist) |>
  pivot_longer(c(wid, div), names_to = "m", values_to = "n") |>
  mutate(e = ifelse(m == "wid", exp_wid, exp_div), e_un = e * (1 - fi_own), e_in = e * fi_own) |>
  group_by(year, sex, m) |>
  mutate(res_un = ifelse(age >= 71, ifelse(e_un > 0, n / e_un, 0), NA_real_),
         res_un = ifelse(age < 71, res_un[age == 71], res_un),
         n_un = e_un * res_un, n_in = pmax(0, n - n_un),
         res_in = ifelse(age <= 70 & e_in > 0, n_in / e_in, 0)) |> ungroup()

# ---- Residuals: 10-year trend at 2025, graduated over age (AW-02) -------------------------
fit_res <- function(d) {
  d <- d |> filter(year >= 2014) |> mutate(pand = as.integer(year %in% 2020:2021))
  predict(lm(res ~ year + pand, data = d), newdata = data.frame(year = 2025, pand = 0))
}
grad <- function(age, y) pmax(0, predict(smooth.spline(age, y, df = min(10, length(age) - 1)), age)$y)
res25 <- hist |> pivot_longer(c(res_un, res_in), names_to = "kind", values_to = "res") |>
  filter(!(kind == "res_in" & age > 70)) |>
  group_by(sex, m, kind, age) |> group_modify(function(g, k) tibble(res_reg = fit_res(g))) |> ungroup() |>
  group_by(sex, m, kind) |> mutate(res = grad(age, res_reg)) |> ungroup()

# ---- Projection ----------------------------------------------------------------------------------
dis_rate <- dw_hist |> filter(year == 2025) |>
  inner_join(pop_ms |> filter(year == 2025), by = c("year", "sex", "age")) |>
  transmute(sex, age, dr = number / pmax(1, widowed + divorced))
rr <- res25 |> select(sex, m, kind, age, res) |> pivot_wider(names_from = c(kind, m), values_from = res, values_fill = 0)
proj_raw <- link |> filter(year >= 2026) |> inner_join(rr, by = c("sex", "age")) |>
  mutate(wid = exp_wid * ((1 - fi_own) * res_un_wid + fi_own * res_in_wid * (age <= 70)),
         div = exp_div * ((1 - fi_own) * res_un_div + fi_own * res_in_div * (age <= 70)))
disabled <- pop_ms |> filter(year >= 2026, age >= 50, age <= 66) |> inner_join(dis_rate, by = c("sex", "age")) |>
  transmute(year, sex, age, disabled = (widowed + divorced) * dr)

tot <- proj_raw |> group_by(year) |> summarise(aged = sum(wid + div)) |>
  inner_join(disabled |> group_by(year) |> summarise(dis = sum(disabled)), by = "year") |>
  inner_join(vc4 |> transmute(year, tr = 1000 * widow), by = "year")
sr <- tot |> mutate(f = ifelse(year <= 2035, (tr - dis) / aged, NA_real_))
f35 <- sr$f[sr$year == 2035]
sr <- sr |> mutate(f = case_when(year <= 2035 ~ f, year <= 2045 ~ f35 + (1 - f35) * (year - 2035) / 10, TRUE ~ 1))
proj_model <- proj_raw |> inner_join(sr |> select(year, f), by = "year") |> mutate(wid = wid * f, div = div * f)
# Levels (AW-06): aged + disabled = V.C4's widow(er) total every year; the
# model splits it by age, sex and marital status. Through 2035 this equals the
# short-range match; after 2035 it replaces the phase-out, which runs +4% to
# +11% high out of sample (F-15).
lev <- proj_model |> group_by(year) |> summarise(aged = sum(wid + div)) |>
  inner_join(disabled |> group_by(year) |> summarise(dis = sum(disabled)), by = "year") |>
  inner_join(vc4 |> transmute(year, tr = 1000 * widow), by = "year") |> transmute(year, k = (tr - dis) / aged)
proj <- proj_model |> inner_join(lev, by = "year") |> mutate(wid = wid * k, div = div * k)

# ---- Checks --------------------------------------------------------------------------------------
cmp <- proj_model |> group_by(year) |> summarise(aged = sum(wid + div)) |>
  inner_join(disabled |> group_by(year) |> summarise(dis = sum(disabled)), by = "year") |>
  inner_join(vc4 |> transmute(year, tr = 1000 * widow), by = "year") |>
  mutate(ours = aged + dis, gap_pct = 100 * (ours / tr - 1))
raw_cmp <- tot |> mutate(gap_pct = 100 * ((aged + dis) / tr - 1))
cat("Widow(er) residual at 2025 (graduated), selected ages:\n")
print(res25 |> filter(age %in% c(60, 62, 65, 68, 70, 75, 80, 85, 90, 95)) |> select(sex, m, kind, age, res) |>
        mutate(res = round(res, 3)) |> pivot_wider(names_from = age, values_from = res))
cat("\nShort-range factors 2026-2035:", paste(round(sr$f[sr$year <= 2035], 3), collapse = " "), "\n")
cat("\nWidow(er)s (aged + disabled), thousands, vs TR V.C4: model with OCACT's 2026-2045 factors, and model alone.\nSaved levels use V.C4 (AW-06).\n")
print(cmp |> filter(year %in% c(2026, 2030, 2035, 2040, 2045, 2050, 2060, 2075, 2090, 2099)) |>
        inner_join(raw_cmp |> select(year, raw_gap = gap_pct), by = "year") |>
        transmute(year, ours_k = round(ours / 1000), tr_k = round(tr / 1000), gap_pct = round(gap_pct, 1),
                  model_alone_gap = round(raw_gap, 1)))

dir.create("outputs", showWarnings = FALSE)
write.csv(cmp, "outputs/aged_widows_checks.csv", row.names = FALSE)
out <- bind_rows(hist |> group_by(year, sex, age) |> summarise(wid = sum(n[m == "wid"]), div = sum(n[m == "div"]), .groups = "drop") |>
                   transmute(year, sex, age, aged = wid + div, wid, div),
                 proj |> transmute(year, sex, age, aged = wid + div, wid, div))
saveRDS(list(aged = out, disabled = bind_rows(dw_hist |> rename(disabled = number), disabled),
             model = proj_model, residual = res25, sradj = sr), "data/aged_widows.rds")
cat("Saved data/aged_widows.rds\n")
