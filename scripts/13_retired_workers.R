# 12_retired_workers.R
#
# Retired-worker beneficiaries, 2015-2100 (methodology 3.3, equation 3.3.2):
#
#   RW = population x fully insured x not on DI or converted from DI
#        x not drawing a widow(er) benefit x retirement prevalence
#
# by single age (62-100) and sex; retired workers converted from DI at NRA are
# added back, so totals compare with Supplement 5.A1.1 and TR V.C4.
#
# Prevalence (DECISIONS.md RW-01 to RW-09)
#   History    2007-2025 from Supplement 5.A1.1 (nineteen December editions),
#              as retired workers net of converted DI over the exposure.
#   Age 62     OCACT's printed regression on the employment rate at 62 and the
#              months from 62 to NRA. The age-62 employment rate isn't
#              published: we apply the regression to changes from 2025 in the
#              employment-to-population ratio at 60-64 (Actuarial Study 127),
#              keeping 2025's actual prevalence as the level (RW-03).
#   63-69      ESTPR = p62 + (1 - p62) x (MBA/PIA at N - at 62) / (at 70 - at 62),
#              by cohort, plus DIFFADJ: the 2021-2025 trend of actual - ESTPR
#              evaluated at 2025, held constant (RW-04).
#   70         grades linearly from 2025's actual to 0.995 over 20 years.
#   71+        each cohort keeps its age-70 value (cohorts past 70 in 2025 keep
#              their 2025 value).
#   2026-2035  one factor per year on prevalence at 65 and over so the total
#              matches TR V.C4 (OCACT's short-range adjustment, SRADJ, which
#              OCACT applies to 62-64 and 65-69 by sex; we have only totals,
#              RW-11); the 2035 factor is held after. 2036-2100 is then out of
#              sample.
#
# Input:  scripts 01-03, 07, 08, 10, 12 outputs; Supplement 5.A1.1 and 5.A1.6
#         vintages; Study 127; 6.F1, 6.F2; Study 130 Table 5
# Output: data/retired_workers.rds, outputs/retired_workers_checks.csv

library(dplyr)
library(tidyr)
source("R/read_tr.R")
source("R/read_oasi.R")
source("R/read_studies.R")

sexes <- c("M", "F")
hist_years <- 2007:2025
years <- 2007:2100
ages <- 60:100

popm <- readRDS("data/population_dec.rds") |> mutate(sex = as.character(sex), age = as.integer(age), year = as.integer(year))
pop  <- popm |> group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop")
ins  <- readRDS("data/insured_rates_calibrated.rds") |> mutate(sex = as.character(sex))
qgen <- readRDS("data/death_probs.rds") |> mutate(sex = as.character(sex))
pc   <- readRDS("data/params_by_cohort.rds")
di   <- readRDS("data/di_inputs.rds")
dip  <- readRDS("data/di_projection.rds")
st25 <- readRDS("data/di_stock_2025.rds")

# Spread published age groups (90-94, 95-99, 100+) to single ages by population
spread_groups <- function(d) {
  single <- d |> filter(age == age_hi) |> select(year, age, sex, number)
  grp <- d |> filter(age_hi > age)
  if (!nrow(grp)) return(single)
  sp <- grp |> mutate(g = paste(age, age_hi)) |> rowwise() |> reframe(year, sex, g, number, age = age:min(age_hi, 100)) |>
    left_join(pop, by = c("year", "sex", "age")) |>
    group_by(year, sex, g) |> mutate(number = number * pop / sum(pop)) |> ungroup() |> select(year, age, sex, number)
  bind_rows(single, sp)
}

# ---- 1. History: retired workers and nondisabled widow(er)s ------------------------------
files <- oasi_vintage_files()
rw_hist <- bind_rows(lapply(files, read_supp_5a11), lapply(oasi_vintage_pdfs(), read_supp_5a11_pdf)) |>
  mutate(sex = as.character(sex)) |> spread_groups()
wid_hist <- bind_rows(lapply(files, read_supp_5a16)) |> mutate(sex = as.character(sex)) |> spread_groups()
# 5.A1.6 isn't in our files before December 2012: 2007-2011 hold 2012's rates by age (RW-05)
w12 <- wid_hist |> filter(year == 2012) |> inner_join(pop, by = c("year", "sex", "age")) |> transmute(sex, age, wr = number / pop)
wid_hist <- bind_rows(pop |> filter(year %in% 2007:2011, age >= 60) |> inner_join(w12, by = c("sex", "age")) |>
                        transmute(year, age, sex, number = pop * wr), wid_hist)
stopifnot(all(sort(unique(rw_hist$year)) == hist_years))
chk <- rw_hist |> group_by(year) |> summarise(t = sum(number))
cat("Retired workers after spreading age groups, 2007/2025 (published 31,527,728 / 53,624,664):",
    round(chk$t[chk$year == 2007]), "/", round(chk$t[chk$year == 2025]), "\n")
stopifnot(abs(chk$t[chk$year == 2025] - 53624664) < 10)

# ---- 2. Disabled workers 62-66 in current pay -----------------------------------------------
# 2025: the stock; 2007-2024: Study 130 Table 6 groups (60-64, 65-66) spread with the 2025 single-age
# shape, the 65-66 group by each year's cohort NRA: at year-end those aged a born y - a remain on DI
# with probability f(y, a) = min(1, max(0, NRA - a)) (DP-04), so the 2025 counts are first divided by
# f(2025, a) (F-34; with the 2025 shape alone, 66 held half the group in years when NRA-66 cohorts had
# all converted by December); 2026+: scripts/10.
nra_b <- function(b) { v <- pc$nra_months[match(b, pc$birth_year)] / 12; ifelse(is.na(v), ifelse(b < 1937, 65, 67), v) }
source("R/di_remain.R"); f_on <- function(y, a) di_remain(y, a, pc)
cp25 <- st25 |> mutate(sex = as.character(sex)) |> group_by(sex, age = attained_age) |>
  summarise(n = sum(current_pay), .groups = "drop")
shape <- cp25 |> filter(age >= 60) |> mutate(g = ifelse(age <= 64, "a60_64", "a65_66"), u = n / pmax(f_on(2025L, age), 1e-9)) |>
  select(sex, g, age, u)
di_hist <- di$hist_inforce |> filter(sex != "T", year >= 2007) |> mutate(sex = as.character(sex)) |>
  select(year, sex, a60_64, a65_66) |> pivot_longer(c(a60_64, a65_66), names_to = "g", values_to = "n") |>
  inner_join(shape, by = c("sex", "g"), relationship = "many-to-many") |>
  mutate(w = u * f_on(year, age)) |> group_by(year, sex, g) |> mutate(w = if (sum(w) > 0) w / sum(w) else 0 * w) |> ungroup() |>
  transmute(year, sex, age, dib = n * w)
dib <- bind_rows(di_hist, cp25 |> transmute(year = 2025L, sex, age, dib = n),
                 dip$stock_age |> transmute(year, sex = as.character(sex), age = a, dib = cp)) |>
  filter(age >= 60) |> group_by(year, sex, age) |> summarise(dib = sum(dib), .groups = "drop")

# ---- 3. Retired workers converted from DI, by age (RW-02) ---------------------------------
# Conversions by year and sex: 2001-2024 Study 130 Table 5; 2025 Supplement
# 6.F2 (split by 2024's sex shares); 1975-2000 DI terminations (6.F1) x the
# 2001 conversion share by sex; 2026+ scripts/10 (by age). Age at conversion:
# the year-end age a for which a cohort born y - a reaches NRA during the year.
# Survival: general-population mortality times a ratio grading from the
# disabled-worker ultimate rate at 66 (Study 130) down to 1 at 90 (methodology
# 3.2.c).
t5 <- di$hist_terms |> filter(sex != "T") |> mutate(sex = as.character(sex)) |> select(year, sex, conversion, total)
f61 <- read_text_sheet(supp26("6f.xlsx"), "6.F1")
f61 <- tibble(year = suppressWarnings(as.integer(f61[[1]])), di_term = suppressWarnings(as.numeric(f61[[5]]))) |>
  filter(!is.na(year), !is.na(di_term))
sh01 <- t5 |> filter(year == 2001) |> mutate(share = conversion / sum(total), sexsh = conversion / sum(conversion))
conv_early <- f61 |> filter(year >= 1975, year <= 2000) |> expand_grid(sh01 |> select(sex, share, sexsh)) |>
  transmute(year, sex, conv = di_term * sum(sh01$conversion) / sum(sh01$total) * sexsh)
c24 <- t5 |> filter(year == 2024)
conv_hist <- bind_rows(conv_early, t5 |> filter(year <= 2024) |> transmute(year, sex, conv = conversion),
                       tibble(year = 2025L, sex = c24$sex, conv = 457350 * c24$conversion / sum(c24$conversion)))
# Each year's conversions by year-end age: the cohort born y - a converts f(y - 1, a - 1) - f(y, a) of
# its DI rolls during year y (DP-04's remain probabilities; F-34: the whole year had gone to one cohort)
conv_age_of <- function(y) {
  cand <- 64:68
  w <- pmax(0, f_on(y - 1L, cand - 1L) - f_on(y, cand))
  if (sum(w) == 0) return(tibble(age = 66L, w = 1))
  tibble(age = cand[w > 0], w = w[w > 0] / sum(w))
}
conv_hist_age <- conv_hist |> rowwise() |> reframe(year, sex, conv, conv_age_of(year)) |>
  transmute(year, sex, age, conv = conv * w)
conv_all <- bind_rows(conv_hist_age, dip$conv_age |> transmute(year, sex = as.character(sex), age, conv))

q_di66 <- di$su_death |> filter(duration == 10, attained_age == 66) |> transmute(sex = as.character(sex), q66 = q)
q_g66 <- qgen |> filter(age == 66, year == 2018) |> select(sex, qg = qx)
ratio66 <- q_di66 |> inner_join(q_g66, by = "sex") |> transmute(sex, r66 = q66 / qg)
qconv <- qgen |> filter(year >= 1975, age >= 62, age <= 100) |> inner_join(ratio66, by = "sex") |>
  mutate(r = ifelse(age >= 90, 1, 1 + (r66 - 1) * pmax(0, 90 - age) / (90 - 66)), q = pmin(0.99, qx * r)) |>
  select(year, sex, age, q)
conv_stock <- {
  out <- list(); cur <- tibble(sex = character(), age = integer(), n = numeric())
  for (y in 1975:2100) {
    if (nrow(cur)) {
      cur <- cur |> mutate(year = y) |> left_join(qconv, by = c("year", "sex", "age")) |>
        mutate(n = n * (1 - coalesce(q, 0.3)), age = age + 1L) |> filter(age <= 100) |> select(sex, age, n)
    }
    new <- conv_all |> filter(year == y) |> transmute(sex, age = as.integer(age), n = conv)
    cur <- bind_rows(cur, new) |> group_by(sex, age) |> summarise(n = sum(n), .groups = "drop")
    out[[length(out) + 1]] <- cur |> mutate(year = y)
  }
  bind_rows(out) |> rename(conv_stock = n)
}

# ---- 4. Exposure and historical prevalence -----------------------------------------------------
# Exposure = population x fully insured - DI in current pay - converted DI -
# widow(er) beneficiaries x fully insured (insured widow(er)s drawing the widow
# benefit instead of their own, RW-05). OCACT subtracts its insured aged widow(er)s at every age
# (equation 3.3.2, pRWWBB; its insured widows run to 85+), so this stays at all ages (RW-12).
vc4 <- read_tr_single_year("V.C4", c("rw", "rw_spouse", "rw_child", "widow", "mother", "surv_child", "parent", "total"))
# Aged widow(er)s: scripts/12 (history 2012-2025 from 5.A1.6, projection at
# V.C4 levels); 2007-2011 hold 2012's rates by age (RW-05).
aw <- readRDS("data/aged_widows.rds")$aged |> transmute(year = as.integer(year), sex, age = as.integer(age), wid = aged)
wid_all <- bind_rows(wid_hist |> filter(year < 2012) |> rename(wid = number), aw)

base <- expand_grid(year = years, sex = sexes, age = 62:100) |>
  left_join(pop, by = c("year", "sex", "age")) |>
  left_join(ins |> select(year, sex, age, fully), by = c("year", "sex", "age")) |>
  left_join(dib, by = c("year", "sex", "age")) |>
  left_join(conv_stock, by = c("year", "sex", "age")) |>
  left_join(wid_all, by = c("year", "sex", "age")) |>
  mutate(across(c(dib, conv_stock, wid), ~ coalesce(.x, 0)),
         fully = coalesce(fully, 0),
         exposure = pmax(0, pop * fully - dib - conv_stock - wid * fully))
hist <- base |> filter(year %in% hist_years) |>
  inner_join(rw_hist |> rename(rw = number), by = c("year", "sex", "age")) |>
  mutate(rwn = pmax(0, rw - conv_stock), prev = rwn / exposure)
cat("Historical prevalence (non-converted retired workers / exposure), men:\n")
print(hist |> filter(sex == "M", age %in% c(62, 63, 64, 65, 66, 67, 68, 70, 75, 85)) |>
        select(year, age, prev) |> mutate(prev = round(prev, 3)) |> pivot_wider(names_from = age, values_from = prev))

# ---- 5. MBA/PIA by cohort and age (RW-06) ------------------------------------------------------------
mbapia <- function(b, N) {
  nra <- nra_b(b); drc <- pc$drc_annual[match(b, pc$birth_year)] / 100
  drc <- ifelse(is.na(drc), 0.08, drc)
  m_early <- pmax(0, (nra - N) * 12)
  red <- pmin(m_early, 36) * 5 / 900 + pmax(0, m_early - 36) * 5 / 1200
  ifelse(N >= nra, 1 + (pmin(N, 70) - nra) * drc, 1 - red)
}

# ---- 6. Projection by cohort -----------------------------------------------------------------------
# Age-62 prevalence (RW-03): OCACT's coefficients on changes from 2025.
coef62 <- tibble(sex = sexes, b_emp = c(-1.35619, -0.42007), b_nra = c(-0.00412, -0.01004))
emp <- read_as127_employment() |> filter(group == "60_64") |> mutate(sex = as.character(sex), ratio = ratio / 100) |>
  select(year, sex, ratio)                     # Study 127 reports percent; the regression uses a fraction
emp <- bind_rows(emp, emp |> filter(year == max(year)) |> select(-year) |> expand_grid(year = (max(emp$year) + 1):2100))
emp_rel <- emp |> group_by(sex) |> mutate(d_emp = ratio - ratio[year == 2025]) |> ungroup() |> select(year, sex, d_emp)
# Study 127 is a ratio for 60-64; OCACT's regressor is the age-62 employment
# rate. We use its change (RW-03).
p62_hist <- hist |> filter(age == 62) |> select(year, sex, p62 = prev)
p62_2025 <- p62_hist |> filter(year == 2025)
months_nra <- function(b) (nra_b(b) - 62) * 12
p62_proj <- expand_grid(year = 2026:2100, sex = sexes) |>
  left_join(emp_rel, by = c("year", "sex")) |> left_join(coef62, by = "sex") |>
  left_join(p62_2025 |> select(sex, p62_25 = p62), by = "sex") |>
  mutate(p62 = p62_25 + b_emp * coalesce(d_emp, 0) + b_nra * (months_nra(year - 62) - months_nra(2025 - 62))) |>
  select(year, sex, p62)
p62_all <- bind_rows(p62_hist, p62_proj)

# Cohort p62 lookup (cohort b reaches 62 in b + 62; cohorts before 1953 aren't
# needed for 63-69 after 2025)
cohort_p62 <- p62_all |> transmute(sex, b = year - 62L, p62)

# ESTPR for N = 63-69 by cohort, and DIFFADJ (RW-04)
est <- expand_grid(sex = sexes, b = 1946:2038, N = 63:69) |>
  inner_join(cohort_p62, by = c("sex", "b")) |>
  mutate(mp62 = mbapia(b, 62), mpN = mbapia(b, N), mp70 = mbapia(b, 70),
         estpr = p62 + (1 - p62) * pmax(0, mpN - mp62) / (mp70 - mp62), year = b + N)
diffs <- est |> inner_join(hist |> select(year, sex, N = age, prev), by = c("year", "sex", "N")) |>
  mutate(diff = prev - estpr) |> filter(year >= 2021)
diffadj <- diffs |> group_by(sex, N) |>
  summarise(diffadj = predict(lm(diff ~ year), newdata = data.frame(year = 2025)), .groups = "drop")
cat("\nDIFFADJ (actual - estimated prevalence, 2021-2025 trend at 2025):\n")
print(diffadj |> mutate(diffadj = round(diffadj, 3)) |> pivot_wider(names_from = N, values_from = diffadj))

# Age-66 adjustment for the NRA-67 cohorts (methodology; RW-10): the 1960
# cohort's increase from 65 to 66 is set to the 1943 cohort's increase from 64
# to 65 (first NRA-66 cohort, December 2007 -> 2008). The factor that brings the
# 1960 cohort's age-66 prevalence to that target applies to later cohorts too
# (1958-1959 are history here).
d43 <- hist |> filter((year == 2007 & age == 64) | (year == 2008 & age == 65)) |>
  group_by(sex) |> summarise(d43 = prev[age == 65] - prev[age == 64])
p65_1960 <- hist |> filter(year == 2025, age == 65) |> select(sex, p65 = prev)
est66_1960 <- est |> filter(b == 1960, N == 66) |> inner_join(diffadj, by = c("sex", "N")) |>
  transmute(sex, p66 = estpr + diffadj)
adj66 <- d43 |> inner_join(p65_1960, by = "sex") |> inner_join(est66_1960, by = "sex") |>
  transmute(sex, d43, target = p65 + d43, model = p66, f66 = target / model)
cat("\nAge-66 adjustment (1943 cohort 64->65 step applied to the 1960 cohort):\n")
print(adj66 |> mutate(across(where(is.numeric), ~ round(.x, 3))))

p70_25 <- hist |> filter(age == 70, year == 2025) |> select(sex, p70_25 = prev)
proj_prev <- function(sradj) {
  # sradj: tibble(year, f) multiplying prevalence at 62-69
  p6369 <- est |> inner_join(diffadj, by = c("sex", "N")) |>
    left_join(adj66 |> select(sex, f66), by = "sex") |>
    mutate(p = estpr + diffadj, p = ifelse(N == 66 & b >= 1960, p * f66, p)) |>
    transmute(year, sex, age = N, p = pmin(0.995, p)) |> filter(year >= 2026)
  p62p <- p62_proj |> transmute(year, sex, age = 62L, p = p62)
  p70 <- expand_grid(year = 2026:2100, sex = sexes) |> inner_join(p70_25, by = "sex") |>
    transmute(year, sex, age = 70L, p = 0.995 - (0.995 - p70_25) * pmax(0, (2026 + 19 - year) / 20))
  young <- bind_rows(p62p, p6369, p70)
  # 71+: cohort keeps its age-70 value; cohorts past 70 in 2025 keep their 2025 value
  c70 <- bind_rows(hist |> filter(age == 70) |> transmute(b = year - 70L, sex, p70 = prev),
                   young |> filter(age == 70) |> transmute(b = year - 70L, sex, p70 = p))
  h25 <- hist |> filter(year == 2025, age >= 71) |> transmute(b = 2025L - age, sex, p25 = prev)
  old <- expand_grid(year = 2026:2100, sex = sexes, age = 71:100) |> mutate(b = year - age) |>
    left_join(c70, by = c("b", "sex")) |> left_join(h25, by = c("b", "sex")) |>
    transmute(year, sex, age, p = coalesce(p25, p70))
  # Where exposure is near zero (the oldest ages) historical prevalence isn't
  # usable: fall back to 0.995, and cap at 1.2 (values a little above 1 absorb
  # small understatements of insured rates at old ages) (RW-08).
  # Short-range factor (RW-11): applied in each year to prevalence at ages sr_lo
  # and over, after the age-70 and 71+ rules, so it moves the level without
  # changing the age pattern. Capped at 0.995 below 70 and 1.2 at 70+.
  bind_rows(young, old) |> mutate(p = ifelse(is.finite(p) & p >= 0, pmin(p, 1.2), 0.995)) |>
    left_join(sradj, by = "year") |>
    mutate(p = ifelse(age >= sr_lo & age <= sr_hi, pmin(ifelse(age < 70, 0.995, 1.2), p * coalesce(f, 1)), p)) |>
    select(-f)
}
project <- function(sradj) {
  pp <- proj_prev(sradj)
  base |> filter(year >= 2026) |> inner_join(pp, by = c("year", "sex", "age")) |>
    mutate(rwn = p * exposure, rw = rwn + conv_stock)
}

sr_lo <- as.integer(Sys.getenv("SR_LO", "65"))
sr_hi <- as.integer(Sys.getenv("SR_HI", "100"))  # ages the short-range factor applies to (RW-11)
# SRADJ: one factor per year 2026-2035 on prevalence at 65 and over so the
# total matches V.C4; the 2035 factor is held after (RW-11).
sr <- tibble(year = 2026:2100, f = 1)
tot_at <- function(sr, y) project(sr) |> filter(year == y) |> summarise(t = sum(rw)) |> pull(t)
for (y in 2026:2035) {
  tgt <- 1000 * vc4$rw[vc4$year == y]
  g <- function(x) { s2 <- sr; s2$f[s2$year == y] <- x; tot_at(s2, y) - tgt }
  sr$f[sr$year == y] <- uniroot(g, c(0.5, 1.5), tol = 1e-5)$root
}
sr$f[sr$year > 2035] <- sr$f[sr$year == 2035]
proj <- project(sr)

# ---- 7. Checks ---------------------------------------------------------------------------------------
tot <- proj |> group_by(year) |> summarise(rw = sum(rw), conv = sum(conv_stock), .groups = "drop") |>
  inner_join(vc4 |> select(year, tr = rw), by = "year") |> mutate(tr = 1000 * tr, gap_pct = 100 * (rw / tr - 1))
cat("\nSRADJ factors 2026-2035:", paste(round(sr$f[sr$year <= 2035], 3), collapse = " "), "\n")
cat("\nRetired workers (millions) vs TR V.C4; 2026-2035 matched, 2036+ out of sample:\n")
print(tot |> filter(year %in% c(2026, 2030, 2035, 2040, 2050, 2060, 2075, 2090, 2099)) |>
        transmute(year, ours = round(rw / 1e6, 2), tr = round(tr / 1e6, 2), gap_pct = round(gap_pct, 1),
                  converted = round(conv / 1e6, 2)))
cat("\nProjected prevalence, men, selected ages:\n")
print(proj |> filter(sex == "M", year %in% c(2026, 2035, 2050, 2075, 2100), age %in% c(62, 64, 66, 67, 68, 70, 80)) |>
        select(year, age, p) |> mutate(p = round(p, 3)) |> pivot_wider(names_from = age, values_from = p))

dir.create("outputs", showWarnings = FALSE)
write.csv(tot, "outputs/retired_workers_checks.csv", row.names = FALSE)
saveRDS(list(hist = hist, proj = proj, sradj = sr, diffadj = diffadj, p62 = p62_all,
             conv_stock = conv_stock, exposure = base), "data/retired_workers.rds")
cat("Saved data/retired_workers.rds\n")
