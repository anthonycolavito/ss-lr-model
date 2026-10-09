# 26_rw_benefits.R
#
# Retired-worker benefits in current pay, December 2025-2100 (methodology 4.3, "Retired-Worker
# Benefits" and "DI Conversions"; DECISIONS.md RB-01 to RB-07, DX-01).
#
# OCACT's matrix: age in current pay (62-94, 95+) x age at entitlement (62-70) plus a column for DI
# conversions, by sex, started from the 100% MBR and scaled to Table 1-A totals; each year every cell
# moves one year of age with the COLA and the post-entitlement factor; the diagonal (new entitlements)
# comes from the award PIAs x reduction or credits. This model uses the same matrix (ages 62-100,
# 100 the open group) with scripts/14's counts.
#
#   All amounts are worker benefits: the Supplement's averages include the dual-entitlement excess, which
#     is taken out (R/dual_excess.R, DX-01) and projected separately. PIAs are carried alongside.
#   Start (December 2025): each cell split into its reduced and not-reduced parts, r = 2025 award PIA
#     at the exact entitlement age x the cohort's reduction or credit factor (conversions: the DI
#     average at 66, 5.A1.2, net of the December COLA), scaled by age and part to Supplement 5.A3a and
#     5.A1.1, then a last factor by age to 5.A1.1 (RB-01). 5.B4 by year of entitlement is a check (entitlement year = 2025 - (a - ae)).
#   Roll forward: mba(t, a, ae) = mba(t-1, a-1, ae) x (1 + COLA_t) x PE(retired, sex, a-1-ae, t);
#     age 100 = weighted mean of the 99 and 100 cells moved forward (weights: last year's counts).
#   New entitlements: mba(t, ae, ae) = award(t, ae) x k0(sex) x (1 + COLA_t), award at December age ae
#     = mean of the awards at exact ages ae - 1 and ae (62: the award at 62; RB-02); k0 = 5.B4's
#     December 2025 average for the 2025 cohort / 6.A4's average 2025 award, net of the COLA (RB-03).
#   Conversions: new conversions carry their DI cohort benefit (scripts/25), pooled with the
#     converted workers moved forward from a-1 (conversion PE factors, duration a-1-67).
#
# Input:  data/rw_entitlement_age.rds (14), award_levels.rds (22), post_entitlement.rds (24),
#         di_benefits.rds (25), params_by_year.rds; Supplement 2026 5.A1.1, 5.A1.2, 5.A3a, 5.B4, 5.B7, 6.A4; R/dual_excess.R
# Output: data/rw_benefits.rds

suppressMessages({library(dplyr); library(tidyr)})
source("R/read_di.R")
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))
py <- readRDS("data/params_by_year.rds") |> select(year, cola, awi)
cola <- setNames(py$cola / 100, py$year)
pe <- readRDS("data/post_entitlement.rds")$by_year |> filter(type %in% c("retired", "conversion")) |> select(type, sex, dur, year, pe)
aw <- readRDS("data/award_levels.rds") |> filter(type == "retired") |> transmute(year, sex = SEX, age, award = mba, apia = pia)
# award at December age of entitlement ae: mean of exact ages ae-1 and ae (62: 62)
awd <- aw |> left_join(aw |> transmute(year, sex, age = age + 1L, prev = award, prev_p = apia), by = c("year", "sex", "age")) |>
  transmute(year, sex, ae = age, award = ifelse(is.na(prev), award, (award + prev) / 2), apia = ifelse(is.na(prev_p), apia, (apia + prev_p) / 2))
rw <- readRDS("data/rw_entitlement_age.rds")$rw_ae |> mutate(sex = as.character(sex), age = as.integer(age), ae = as.integer(ae), year = as.integer(year))
cv <- readRDS("data/di_benefits.rds")$conversions      # year, sex, a, mba, conv

# ---- Supplement readers (single ages 62-89; groups 90-94, 95-99, 100+) --------------------------------------
read_by_age <- function(f, sheet, cn = c(6, 7, 8, 9)) {
  m <- as.matrix(readxl::read_excel(f, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  lab <- ifelse(!is.na(m[, 2]) & grepl("^[0-9]+$", trimws(m[, 2])), trimws(m[, 2]), trimws(m[, 1]))
  lab <- gsub("[^0-9a-z ]", "-", lab)
  lo <- suppressWarnings(as.integer(sub("^([0-9]+).*", "\\1", lab)))
  hi <- ifelse(grepl("^[0-9]+$", lab), lo, ifelse(grepl("older", lab), 100L, suppressWarnings(as.integer(sub("^[0-9]+-+([0-9]+)$", "\\1", lab)))))
  single <- grepl("^[0-9]+$", lab)
  keep <- !is.na(lo) & !is.na(hi) & (single | lo >= 90) & lo >= 62
  out <- bind_rows(tibble(sex = "M", lo = lo[keep], hi = hi[keep], n = num(m[keep, cn[1]]), mba = num(m[keep, cn[2]])),
                   tibble(sex = "F", lo = lo[keep], hi = hi[keep], n = num(m[keep, cn[3]]), mba = num(m[keep, cn[4]])))
  out |> filter(!(lo < 90 & lo != hi), !is.na(n)) |> distinct(sex, lo, hi, .keep_all = TRUE)
}
f5a <- "data-raw/supplement/2026/5a.xlsx"; f5b <- "data-raw/supplement/2026/5b.xlsx"
a11 <- read_by_age(f5a, "5.A1.1")
b4 <- {
  m <- as.matrix(readxl::read_excel(f5b, sheet = "5.B4", col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  lab <- trimws(m[, 1]); y <- suppressWarnings(as.integer(lab)); y[grepl("^Before", lab)] <- 1986L
  ok <- !is.na(y)
  bind_rows(tibble(sex = "M", yk = y[ok], n = num(m[ok, 7]), mba = num(m[ok, 10])), tibble(sex = "F", yk = y[ok], n = num(m[ok, 11]), mba = num(m[ok, 14])))
}
agek <- function(a) case_when(a < 90 ~ a, a < 95 ~ 90L, a < 100 ~ 95L, TRUE ~ 100L)

# ---- December 2025 start ----------------------------------------------------------------------------------------
# Through 2025 scripts/14 dates entitlement by exact age (EA-02), so in the 2025 stock ae is the exact
# age (integer part) at entitlement. Each cell is split into a reduced part and the rest, the shares
# fitted to 5.A3a's counts by age (RB-06):
#   r = 2025 award PIA at exact age ae (scripts/22; the PIA gradient with claiming age) x the cohort's
#       reduction or credit factor (its own NRA and credit rate) at the part's midpoint
# and every (sex, age, part) is scaled to Supplement 5.A3a (reduced, by age) and 5.A1.1 minus 5.A3a
# (not reduced, by age; conversions belong here) (RB-01). 5.B4 by year of entitlement is a check.
pc <- readRDS("data/params_by_cohort.rds")
cf <- function(x, b) {                      # benefit / PIA at exact claiming age x for birth cohort b
  nra <- pc$nra_months[match(b, pc$birth_year)] / 12; drc <- pc$drc_annual[match(b, pc$birth_year)] / 100
  nra <- ifelse(is.na(nra), 65, nra); drc <- ifelse(is.na(drc), 0.03, drc)
  m <- 12 * (nra - x)
  ifelse(m > 0, 1 - pmin(m, 36) * 5 / 900 - pmax(m - 36, 0) * 5 / 1200, 1 + pmin(-m, 12 * (70 - nra)) / 12 * drc)
}
nra_of <- function(b) { x <- pc$nra_months[match(b, pc$birth_year)] / 12; ifelse(is.na(x), 65, x) }
di66 <- read_supp_5a12() |> filter(age == 66) |> transmute(sex = as.character(sex), r_conv = mba / (1 + cola[["2025"]]))
pia25 <- readRDS("data/award_levels.rds") |> filter(type == "retired", year == 2025) |> transmute(sex = SEX, ae = age, pia)
a3a <- read_by_age(f5a, "5.A3a")
# The published averages are combined benefits including the dual-entitlement excess (DX-01); the
# matrices carry the worker benefit, so the excess per retired worker at each age is taken out of the
# targets (and added back for the 5.B4 check). The excess is projected separately (DX-04).
source("R/dual_excess.R")
xa <- excess_by_age() |> select(sex, age, ex = excess, ex_tot = excess_total, n_pub = n)
exk <- xa |> mutate(ak = agek(age)) |> group_by(sex, ak) |> summarise(exk = sum(ex_tot) / sum(n_pub), .groups = "drop")
# 5.A3a's top group is 95+, 5.A1.1's are 95-99 and 100+: the reduced / not-reduced split works on
# akp = 62-89, 90-94, 95+ (RB-07)
akp_of <- function(ak) pmin(ak, 95L)
a11p <- a11 |> mutate(akp = akp_of(lo)) |> group_by(sex, akp) |> summarise(mba = sum(n * mba) / sum(n), n = sum(n), .groups = "drop")
exkp <- xa |> mutate(akp = akp_of(agek(age))) |> group_by(sex, akp) |> summarise(exk = sum(ex_tot) / sum(n_pub), .groups = "drop")
# The excess is larger among reduced workers: 78% of dually entitled women are reduced (5.G1) against
# 64% of retired women. Excess per worker by part: ex x f_part / (p x f_red + (1 - p) x f_non),
# f_red = dual reduced share / worker reduced share, f_non likewise, p = the age's reduced share (RB-07)
g1 <- as.matrix(readxl::read_excel("data-raw/supplement/2026/5g.xlsx", sheet = "5.G1", col_names = FALSE, col_types = "text", .name_repair = "minimal")); g1[is.na(g1)] <- ""
r_all <- which(grepl("All dually", apply(g1[, 1:3], 1, paste, collapse = " ")))[1]; r_w <- which(trimws(g1[, 2]) == "Women")[1]
dual_red <- c(F = num(g1[r_w, 6]) / num(g1[r_w, 4]), M = (num(g1[r_all, 6]) - num(g1[r_w, 6])) / (num(g1[r_all, 4]) - num(g1[r_w, 4])))
wrk_red <- sapply(c(F = "F", M = "M"), function(sx) sum(a3a$n[a3a$sex == sx]) / sum(a11$n[a11$sex == sx]))
fpart <- tibble(sex = c("F", "M"), f_red = dual_red[c("F", "M")] / wrk_red[c("F", "M")], f_non = (1 - dual_red[c("F", "M")]) / (1 - wrk_red[c("F", "M")]))
cat("Dually entitled reduced share (5.G1) vs retired workers': women", round(dual_red[["F"]], 3), round(wrk_red[["F"]], 3), "; men", round(dual_red[["M"]], 3), round(wrk_red[["M"]], 3), "\n")
s25 <- rw |> filter(year == 2025, number > 0) |>
  mutate(col = ifelse(class == "converted", "conv", as.character(ae)), b = 2025L - age, nra = nra_of(b),
         y = ifelse(class == "converted", 2025L - (age - 67L), 2025L - (age - ae)), ak = agek(age), akp = akp_of(ak),
         grp = case_when(class == "converted" ~ "conv", ae < floor(nra) ~ "below", ae < nra ~ "straddle", TRUE ~ "above")) |>
  left_join(pia25, by = c("sex", "ae")) |> left_join(di66, by = "sex")
# Reduced share fitted to 5.A3a's counts by age (RB-06, F-28): the cell whose year of age contains the
# NRA takes the reduced count left after the cells entitled earlier (s_str, 0-1); where those cells
# alone exceed it, part of them is treated as not reduced (s_below < 1: early benefits withheld under
# the earnings test, reduction recomputed away at NRA).
shares <- s25 |> group_by(sex, akp) |> summarise(below = sum(number[grp == "below"]), straddle = sum(number[grp == "straddle"]), .groups = "drop") |>
  inner_join(a3a |> transmute(sex, akp = lo, red = n), by = c("sex", "akp")) |>
  mutate(s_str = ifelse(straddle > 0, pmin(pmax((red - below) / straddle, 0), 1), 0), s_below = pmin(1, red / pmax(below, 1)))
s25 <- s25 |> left_join(shares |> select(sex, akp, s_str, s_below), by = c("sex", "akp")) |>
  mutate(s_red = case_when(grp == "below" ~ coalesce(s_below, 1), grp == "straddle" ~ coalesce(s_str, pmin(pmax(nra - ae, 0), 1)), TRUE ~ 0))
parts <- bind_rows(
  s25 |> filter(s_red > 0) |> mutate(part = "red", n = number * s_red,
                                     r = pia * cf(ifelse(grp == "straddle", (ae + nra) / 2, ae + 0.5), b)),
  s25 |> filter(s_red < 1) |> mutate(part = "non", n = number * (1 - s_red),
                                     r = case_when(class == "converted" ~ r_conv,
                                                   grp == "below" ~ pia,                     # reduction recomputed away
                                                   grp == "straddle" ~ pia * cf((nra + ae + 1) / 2, b),
                                                   TRUE ~ pia * cf(ae + 0.5, b)))) |>
  mutate(yk = pmax(y, 1986L), cfp = ifelse(class == "converted", 1, r / pia))   # benefit / PIA of the part
cat("Reduced share by age: straddling cell s_str and cells entitled earlier s_below (fitted to 5.A3a):\n")
print(as.data.frame(shares |> filter(akp %in% c(66:70, 75, 80, 85, 90, 95)) |> transmute(sex, age = akp, s_str = round(s_str, 2), s_below = round(s_below, 3)) |>
  pivot_wider(names_from = sex, values_from = c(s_str, s_below))))
tgt <- a11p |> left_join(a3a |> transmute(sex, akp = lo, n_red = n, m_red = mba), by = c("sex", "akp")) |>
  mutate(n_red = coalesce(n_red, 0), m_red = coalesce(m_red, 0), p = n_red / n,
         n_non = n - n_red, m_non = ifelse(n_non > 0, (n * mba - n_red * m_red) / n_non, NA)) |>
  left_join(exkp, by = c("sex", "akp")) |> left_join(fpart, by = "sex") |>
  mutate(den = p * f_red + (1 - p) * f_non, red = m_red - exk * f_red / den, non = m_non - exk * f_non / den) |>
  select(sex, akp, red, non) |> pivot_longer(c(red, non), names_to = "part", values_to = "t")
parts <- parts |> mutate(akp = akp_of(ak)) |> left_join(tgt, by = c("sex", "akp", "part")) |> group_by(sex, akp, part) |>
  mutate(beta = ifelse(is.na(first(t)) | first(t) <= 0 | sum(n) == 0, NA, first(t) / (sum(n * r) / sum(n)))) |> ungroup()
# a part the model has but the tables don't: scaled like the other part at that age
parts <- parts |> group_by(sex, akp) |> mutate(beta = ifelse(is.na(beta), weighted.mean(beta, n, na.rm = TRUE), beta)) |> ungroup() |>
  mutate(mba = r * beta)
# The model's reduced share at an age can differ from the tables' (exact-age split, F-28), so a last
# factor by age brings each age's average to 5.A1.1
fin <- parts |> group_by(sex, ak) |> summarise(m = sum(n * mba) / sum(n), .groups = "drop") |>
  inner_join(a11 |> transmute(sex, ak = lo, t = mba), by = c("sex", "ak")) |> left_join(exk, by = c("sex", "ak")) |>
  mutate(t = t - coalesce(exk, 0), g = t / m)
cat("Last factor by age to 5.A1.1 net of the excess: range", round(range(fin$g), 3), "; ages beyond 3%:\n"); print(fin |> filter(abs(g - 1) > 0.03))
parts <- parts |> left_join(fin |> select(sex, ak, g), by = c("sex", "ak")) |> mutate(mba = mba * g, beta = beta * g) |> select(-g)
cat("Scaling by age and part (reduced / not): range", round(range(parts$beta), 3), "\n")
print(as.data.frame(parts |> group_by(sex, ak, part) |> summarise(beta = weighted.mean(beta, n), .groups = "drop") |> filter(ak %in% c(62, 64, 66, 67, 68, 70, 75, 80, 85, 90, 95, 100)) |>
  mutate(beta = round(beta, 3)) |> pivot_wider(names_from = c(sex, part), values_from = beta)))
# k0 (RB-03): December 2025 average of the 2025 entitlement cohort (5.B4) / average 2025 award (6.A4,
# retired workers including conversions) x (1 + COLA): the step from award amounts to December current
# pay, on the actual mix (both include conversions). The DI equivalent (5.D1 / 6.A4) is 1.022 men,
# 1.016 women, the same as scripts/25's k0.
a4 <- as.matrix(readxl::read_excel("data-raw/supplement/2026/6a.xlsx", sheet = "6.A4", col_names = FALSE, col_types = "text", .name_repair = "minimal"))
i4 <- grep("^Total", trimws(a4[, 4]))[1]      # retired workers (the DI total follows)
k0 <- tibble(sex = c("M", "F"), award = num(a4[i4, c(8, 10)])) |> left_join(b4 |> filter(yk == 2025) |> select(sex, dec = mba), by = "sex") |>
  transmute(sex, k0 = dec / award / (1 + cola[["2025"]]))
cnt <- parts |> filter(part == "red") |> group_by(sex, ak = akp) |> summarise(n_red_model = sum(n), .groups = "drop") |>
  inner_join(a3a |> transmute(sex, ak = lo, n_red_pub = n), by = c("sex", "ak"))
s25 <- parts |> group_by(year, sex, age, ae, class, col, y, yk, ak) |>
  summarise(r = sum(n * r) / sum(n), pia = sum(n * mba / cfp) / sum(n), mba = sum(n * mba) / sum(n), number = sum(n), .groups = "drop") |>
  left_join(xa |> select(sex, age, ex), by = c("sex", "age")) |> mutate(ex = coalesce(ex, 0))
fit_y <- s25 |> group_by(sex, yk) |> summarise(model = sum(number * (mba + ex)) / sum(number), .groups = "drop") |> inner_join(b4, by = c("sex", "yk"))
fit_a <- s25 |> group_by(sex, ak) |> summarise(model = sum(number * (mba + ex)) / sum(number), n_model = sum(number), .groups = "drop") |> inner_join(a11 |> rename(ak = lo), by = c("sex", "ak"))
cat("December 2025 start, worker benefit + excess: largest gap by age", round(100 * max(abs(fit_a$model / fit_a$mba - 1)), 2), "% (fitted)\n")
cat("Check, 5.B4 by year of entitlement, worker benefit + excess (not fitted; through 2025 cells are dated by exact age):\n")
print(as.data.frame(fit_y |> filter(yk %% 4 == 1 | yk == 1986) |> transmute(sex, yk, model = round(model), pub = round(mba), gap_pct = round(100 * (model / mba - 1), 1)) |>
  pivot_wider(names_from = sex, values_from = c(model, pub, gap_pct))))
tot25 <- s25 |> group_by(sex) |> summarise(worker = sum(number * mba) / 1e3, excess = sum(number * ex) / 1e3, pia = sum(number * pia) / sum(number), mba = sum(number * mba) / sum(number))
cat("Total December 2025 monthly benefits, $ thousands: worker benefits", round(sum(tot25$worker)), "+ excess", round(sum(tot25$excess)),
    "=", round(sum(tot25$worker + tot25$excess)), "vs 5.A1.1", round(sum(a11$n * a11$mba) / 1e3), "\n")
b7 <- as.matrix(readxl::read_excel(f5b, sheet = "5.B7", col_names = FALSE, col_types = "text", .name_repair = "minimal"))
b7a <- num(b7[grep("^Average primary", b7[, 1]), 4])            # all, men, women
cat("Average PIA, December 2025 (check, not fitted): model men", round(tot25$pia[tot25$sex == "M"]), "women", round(tot25$pia[tot25$sex == "F"]),
    "vs 5.B7", b7a[2], b7a[3], "; worker benefit / PIA: men", round(tot25$mba[tot25$sex == "M"] / tot25$pia[tot25$sex == "M"], 3),
    "women", round(tot25$mba[tot25$sex == "F"] / tot25$pia[tot25$sex == "F"], 3), "\n")

cat("\nReduced share by age, model vs 5.A3a / 5.A1.1 (fitted, RB-06):\n")
print(as.data.frame(cnt |> left_join(a11 |> transmute(sex, ak = lo, n_all = n), by = c("sex", "ak")) |> filter(ak %in% c(64, 65, 66, 67, 68, 70, 75, 80, 90)) |>
  transmute(sex, age = ak, model = round(n_red_model / n_all, 3), pub = round(n_red_pub / n_all, 3)) |> pivot_wider(names_from = sex, values_from = c(model, pub))))

coh25 <- s25 |> transmute(year = 2025L, sex, age, col, n = number, mba, pia)
dia <- k0
cat("\nk0 (December 2025 new entitlements / their award level, net of the COLA):\n"); print(dia)


# ---- Roll forward ----------------------------------------------------------------------------------------------
cur <- coh25; out <- list(cur)
for (t in 2026:2100) {
  nt <- rw |> filter(year == t, number > 0) |> mutate(col = ifelse(class == "converted", "conv", as.character(ae))) |> select(sex, age, col, ae, n = number)
  moved <- cur |> mutate(age = pmin(age + 1L, 100L)) |>
    mutate(type = ifelse(col == "conv", "conversion", "retired"),
           dur = pmin(ifelse(col == "conv", pmax(age - 1L - 67L, 0L), age - 1L - suppressWarnings(as.integer(col))), 12L)) |>
    left_join(pe |> filter(year == t) |> select(type, sex, dur, pe), by = c("type", "sex", "dur")) |>
    mutate(g = (1 + cola[[as.character(t)]]) * pe, m = mba * g, p = pia * g) |>
    group_by(sex, age, col) |> summarise(m = sum(n * m) / sum(n), p = sum(n * p) / sum(n), w_old = sum(n), .groups = "drop")
  newc <- cv |> filter(year == t) |> transmute(sex, age = a, col = "conv", m_new = mba, w_new = conv)
  diag <- nt |> filter(col != "conv", age == ae) |> left_join(awd |> filter(year == t) |> select(sex, ae, award, apia), by = c("sex", "ae")) |>
    left_join(dia, by = "sex") |> transmute(sex, age, col, m_diag = award * k0 * (1 + cola[[as.character(t)]]), p_diag = apia * k0 * (1 + cola[[as.character(t)]]))
  cur <- nt |> left_join(moved, by = c("sex", "age", "col")) |> left_join(newc, by = c("sex", "age", "col")) |> left_join(diag, by = c("sex", "age", "col")) |>
    mutate(mba = case_when(!is.na(m_diag) ~ m_diag,
                           col == "conv" & !is.na(m_new) & !is.na(m) ~ (w_old * m + w_new * m_new) / (w_old + w_new),
                           col == "conv" & !is.na(m_new) ~ m_new,
                           TRUE ~ m),
           pia = case_when(!is.na(p_diag) ~ p_diag,
                           col == "conv" & !is.na(m_new) & !is.na(p) ~ (w_old * p + w_new * m_new) / (w_old + w_new),
                           col == "conv" & !is.na(m_new) ~ m_new,
                           TRUE ~ p), year = t) |>
    select(year, sex, age, col, n, mba, pia)
  out[[length(out) + 1]] <- cur
}
cells <- bind_rows(out)
cat("\nCells without a benefit (should be none):", sum(is.na(cells$mba)), "of", nrow(cells), "; their count", round(sum(cells$n[is.na(cells$mba)])), "\n")

tot <- cells |> filter(!is.na(mba)) |> group_by(year, sex) |> summarise(mba = sum(n * mba) / sum(n), pia = sum(n * pia) / sum(n), n = sum(n), .groups = "drop") |>
  left_join(py |> transmute(year = year + 1L, awi_prev = awi), by = "year")
new_aw <- cells |> filter(col != "conv") |> filter(age == as.integer(col)) |> group_by(year, sex) |> summarise(mba_new = sum(n * mba) / sum(n), .groups = "drop")
cat("\nAverage monthly benefit (all, and new entitlements), and 12 x average / AWI of the year before:\n")
print(as.data.frame(tot |> left_join(new_aw, by = c("year", "sex")) |> filter(year %in% c(2025, 2026, 2030, 2035, 2040, 2050, 2060, 2075, 2090, 2100)) |>
  transmute(year, sex, n_m = round(n / 1e6, 2), mba = round(mba), mba_new = round(mba_new), rel = round(12 * mba / awi_prev, 3), rel_new = round(12 * mba_new / awi_prev, 3)) |>
  pivot_wider(names_from = sex, values_from = c(n_m, mba, mba_new, rel, rel_new))))

# Trend against the Trustees: OASI cost rate (IV.B1) vs retired-worker benefits / taxable payroll,
# ratio to 2026 (OASI also pays dependents and survivors; annualizing comes later)
source("R/read_tr.R")
ib1 <- read_tr_single_year("IV.B1", c("oasi_inc", "oasi_cost", "oasi_bal", "di_inc", "di_cost", "di_bal", "oasdi_inc", "oasdi_cost", "oasdi_bal"))
g1 <- read_tr_single_year("VI.G1", c("cpi", "awi", "payroll", "gdp", "ratio", "interest"))
mon <- tot |> group_by(year) |> summarise(monthly = sum(n * mba))
oc <- mon |> mutate(annual = 12 * (monthly + lag(monthly)) / 2) |> filter(year >= 2026) |>
  inner_join(g1 |> select(year, payroll), by = "year") |> inner_join(ib1 |> select(year, oasi_cost), by = "year") |>
  mutate(model_rate = 100 * annual / (payroll * 1e9), ratio = model_rate / oasi_cost, ratio = ratio / ratio[year == 2026])
cat("\nRetired-worker benefits / taxable payroll vs the Trustees' OASI cost rate, ratio relative to 2026:\n")
print(as.data.frame(oc |> filter(year %in% c(2026, 2030, 2035, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) |>
  transmute(year, model_rate = round(model_rate, 3), tr_oasi_cost = oasi_cost, ratio = round(ratio, 3))))

saveRDS(list(cells = cells, totals = tot, new_awards = new_aw, k0 = dia, cost_check = oc,
             fit_2025 = list(by_ent_year = fit_y, by_age = fit_a)), "data/rw_benefits.rds")
cat("Saved data/rw_benefits.rds\n")
