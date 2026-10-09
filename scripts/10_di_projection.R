# 10_di_projection.R
#
# Projects disabled-worker beneficiaries, 2026-2100 (methodology 3.2,
# equations 3.2.1-3.2.3), by sex, age at entitlement, duration and attained
# age:
#
#   new entitlements = exposure at start of year x incidence rate
#   exits            = deaths + recoveries + conversions at NRA
#   stock(end)       = stock(start) + new entitlements - exits
#
# Rates (DECISIONS.md DP-01 to DP-07)
#   Incidence   Actuarial Note 2026.6 single-age rates on the note's own age
#               convention (DI-03): exposure aged a at the start of the year
#               uses the note's row a + 1. From 2036 these are the ultimate
#               rates. For 2026-2035 one factor per year, all ages and both
#               sexes, set so the year-end total matches TR V.C5 (OCACT's
#               short-range reconciliation, IPROJG). For 2036-2100 one constant
#               factor so the average gap to V.C5 is zero (DP-08, F-10).
#   Deaths      Study 130 select-and-ultimate base x a factor by sex set to the
#               memo's 2025 age-sex-adjusted death termination rate (26.3 per
#               1,000), then improved at the general population's rate by age
#               and sex (memo section 3; DPROJG).
#   Recoveries  Study 130 base x a factor by sex grading linearly from the
#               memo's 2025 adjusted rate (18.7) to its ultimate (11.1) in 2035,
#               constant after (memo section 4; RPROJG). None past NRA.
#   Conversions At NRA by birth year (ranypia NRA schedule); at the end of each
#               year, those aged a born in year b remain with probability
#               min(1, max(0, NRA_b - a)).
#   Exposure    Disability insured (scripts/07) x population at the start of the
#               year, less disabled workers on the rolls at that age.
#   Timing      New entrants are exposed to death and recovery for half a year
#               (the note's convention) and split evenly between entitlement
#               ages a and a + 1.
#   IBNR        The projection runs on the currently entitled; in current pay =
#               entitled x IBNR(duration), which is what TR V.C5 counts. IBNR
#               grades from today's processing (2024 -> 2025 vintages of 5.D1)
#               to normal processing (2011 -> 2015 vintages) by 2029 (scripts/09).
#               Exposure subtracts the entitled (methodology 3.2.c).
#
# Age-sex-adjusted rates use OCACT's standard populations as far as we can
# rebuild them: disabled workers at December 1999 (Supplement 5.D4) for death
# and recovery; disability insured at December 1999 (4.C2) for prevalence.
#
# Input:  data/di_inputs.rds (08), data/di_stock_2025.rds, data/di_ibnr.rds (09),
#         data/insured_rates_calibrated.rds (07), data/population_dec.rds (01),
#         data/death_probs.rds (02), data/params_by_cohort.rds (03)
# Output: data/di_projection.rds, outputs/di_projection_checks.csv

library(dplyr)
library(tidyr)

di    <- readRDS("data/di_inputs.rds")
stock <- readRDS("data/di_stock_2025.rds")
ibnr  <- readRDS("data/di_ibnr.rds")
sexes <- c("M", "F")
# IBNR by year (DP-07): today's processing (December 2024 -> 2025 vintages)
# grading linearly to normal processing (2011 -> 2015 vintages) by 2029, the
# year the TR expects recent pending awards to be realized.
ibnr_year <- function(t) {
  w <- min(1, max(0, (t - 2025) / (2029 - 2025)))
  a <- matrix(1, 2, 121, dimnames = list(sexes, 0:120))
  r <- ibnr$recent; n <- ibnr$normal
  a[cbind(match(as.character(r$sex), sexes), r$duration + 1)] <- (1 - w) * r$ibnr + w * n$ibnr[match(paste(r$sex, r$duration), paste(n$sex, n$duration))]
  a
}
ins   <- readRDS("data/insured_rates_calibrated.rds")
pop   <- readRDS("data/population_dec.rds") |>
  group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop")
qgen  <- readRDS("data/death_probs.rds")
nra   <- readRDS("data/params_by_cohort.rds") |> transmute(birth_year, nra = nra_months / 12)
sexes <- c("M", "F")
years <- 2026:2100

# ---- Lookup arrays --------------------------------------------------------------
# Study 130: [sex, entitlement age 16-66, duration 0-9] select; [sex, attained
# age 0-120] ultimate (duration 10+: Tables 7A/B last column to 75, 7C beyond).
sel_arr <- function(tab) {
  a <- array(0, c(2, 51, 10), dimnames = list(sexes, 16:66, 0:9))
  t <- tab[tab$duration <= 9 & !is.na(tab$q), ]
  a[cbind(match(as.character(t$sex), sexes), t$entl_age - 15, t$duration + 1)] <- t$q
  a
}
ult_arr <- function(tab, extra = NULL) {
  a <- matrix(0, 2, 121, dimnames = list(sexes, 0:120))
  t <- tab[tab$duration == 10 & !is.na(tab$q), ]
  a[cbind(match(as.character(t$sex), sexes), t$attained_age + 1)] <- t$q
  if (!is.null(extra)) {
    a[cbind(match(as.character(extra$sex), sexes), extra$attained_age + 1)] <- extra$q
    for (s in 1:2) for (x in 96:121) if (a[s, x] == 0) a[s, x] <- a[s, x - 1]
  }
  a
}
qd_sel <- sel_arr(di$su_death); qd_ult <- ult_arr(di$su_death, di$death_76)
qr_sel <- sel_arr(di$su_recov); qr_ult <- ult_arr(di$su_recov)
base_q <- function(sel, ult, s, e, d) {
  e <- pmin(pmax(e, 16), 66)
  ifelse(d <= 9, sel[cbind(s, e - 15, pmin(d, 9) + 1)], ult[cbind(s, pmin(e + d, 120) + 1)])
}

# General-population mortality improvement relative to 2025, by sex, age, year
qg <- qgen |> filter(year >= 2025) |> select(year, sex, age, qx)
impr <- qg |> group_by(sex, age) |> mutate(impr = qx / qx[year == 2025]) |> ungroup() |>
  select(year, sex, age, impr)
impr_arr <- array(1, c(2, 121, length(2025:2100)), dimnames = list(sexes, 0:120, 2025:2100))
impr_arr[cbind(match(as.character(impr$sex), sexes), pmin(impr$age, 120) + 1, impr$year - 2024)] <- impr$impr

# Incidence by start-of-year age a (0-120): the note's row a + 1 (DI-03).
# Rows under 20: half the age-20 rate for 16-17, full for 18-19 (DS-04);
# row 67 (start-of-year age 66, which reaches NRA 67 during the year) gets half
# of row 66: exposed until the birthday.
an <- di$an_rates
inc_row <- matrix(0, 2, 121, dimnames = list(sexes, 0:120))      # indexed by row age
for (s in sexes) {
  r <- an[an$sex == s, ]
  inc_row[s, r$age + 1] <- r$incidence
  inc_row[s, c(17, 18) + 1] <- inc_row[s, 21] / 2
  inc_row[s, c(19, 20) + 1] <- inc_row[s, 21]
  inc_row[s, 68] <- inc_row[s, 67] / 2
}
inc_boy <- cbind(inc_row[, -1], 0)                                # start-of-year age a -> row a + 1

# Disability-insured population at the end of each year, by sex and age
dins <- ins |> select(year, sex, age, disability) |>
  inner_join(pop, by = c("year", "sex", "age")) |>
  transmute(year, sex, age, dins = coalesce(disability, 0) * pop)
dins_arr <- array(0, c(2, 121, length(2025:2100)), dimnames = list(sexes, 0:120, 2025:2100))
d0 <- dins |> filter(year >= 2025)
dins_arr[cbind(match(as.character(d0$sex), sexes), d0$age + 1, d0$year - 2024)] <- d0$dins

nra_of <- function(b) { v <- nra$nra[match(b, nra$birth_year)]; ifelse(is.na(v), 67, v) }

# ---- Standard populations for age-sex-adjusted rates -----------------------------
# Disabled workers, December 1999 (Supplement 5.D4: men 2,802k, women 2,071k).
std_dib <- tibble(sex = rep(sexes, each = 7),
                  band = rep(c("u30", "30_39", "40_44", "45_49", "50_54", "55_59", "60p"), 2),
                  lo = rep(c(0, 30, 40, 45, 50, 55, 60), 2), hi = rep(c(29, 39, 44, 49, 54, 59, 120), 2),
                  w = c(2.9, 12.6, 11.6, 13.8, 16.7, 19.7, 22.9, 2.8, 12.1, 11.5, 14.3, 17.5, 20.4, 21.5) *
                    rep(c(2802, 2071), each = 7))
# Disability insured, December 1999 (4.C2), for prevalence
inp <- readRDS("data/insured_inputs.rds")
std_dins <- inp$targets |> filter(year == 1999, status == "disability", group %in%
                                    c("20_24", "25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", "60_64")) |>
  inner_join(inp$target_groups, by = "group") |> transmute(sex = as.character(sex), lo, hi, w = insured_thousands)
adj_rate <- function(num, den, std) {
  # num, den: data frames sex, age, value; rate per 1,000 adjusted to std
  d <- num |> rename(n = value) |> inner_join(den |> rename(dd = value), by = c("sex", "age")) |>
    inner_join(std, by = join_by(sex, between(age, lo, hi))) |>
    group_by(sex, lo, w) |> summarise(r = sum(n) / sum(dd), .groups = "drop")
  c(total = 1000 * sum(d$r * d$w) / sum(d$w),
    M = 1000 * with(d[d$sex == "M", ], sum(r * w) / sum(w)),
    F = 1000 * with(d[d$sex == "F", ], sum(r * w) / sum(w)))
}

# ---- One year of the projection ------------------------------------------------------
# state: sex, e (entitlement age), d (completed duration at year end), a (age at
# year end), n. Returns the new state and the year's flows.
step <- function(state, t, inc_factor, dfac, rfac) {
  s_i <- match(state$sex, sexes)
  # decrements during year t for those on the rolls at the start (duration d,
  # attained age a at the start of the year)
  impr_t <- impr_arr[cbind(s_i, pmin(state$a, 120) + 1, t - 2024)]
  qd <- pmin(0.95, base_q(qd_sel, qd_ult, s_i, state$e, state$d) * dfac[s_i] * impr_t)
  qr <- base_q(qr_sel, qr_ult, s_i, state$e, state$d) * rfac[s_i]
  qr[state$a >= 66] <- 0
  qr <- pmin(qr, 0.95 - qd)
  deaths <- state$n * qd; recov <- state$n * qr
  st <- state |> mutate(n = n - deaths - recov, d = d + 1L, a = a + 1L)

  # new entitlements: exposure at the start of year t aged a
  boy_on <- state |> group_by(sex, a) |> summarise(on = sum(n), .groups = "drop")
  ex <- expand_grid(sex = sexes, a = 15:66) |>
    left_join(boy_on, by = c("sex", "a")) |>
    mutate(on = coalesce(on, 0),
           dins = dins_arr[cbind(match(sex, sexes), a + 1, t - 1 - 2024)],
           exposure = pmax(0, dins - on),
           rate = inc_boy[cbind(match(sex, sexes), a + 1)] * inc_factor,
           new = exposure * rate)
  sx <- match(ex$sex, sexes)
  ent <- bind_rows(ex |> transmute(sex, e = a, n = new / 2), ex |> transmute(sex, e = a + 1L, n = new / 2)) |>
    mutate(d = 0L, a = NA_integer_)
  ent$a <- rep(ex$a + 1L, 2)
  # half-year exposure for new entrants
  s_e <- match(ent$sex, sexes)
  qd0 <- base_q(qd_sel, qd_ult, s_e, ent$e, 0L) * dfac[s_e] * impr_arr[cbind(s_e, pmin(ent$a, 120) + 1, t - 2024)]
  qr0 <- base_q(qr_sel, qr_ult, s_e, ent$e, 0L) * rfac[s_e]
  deaths0 <- ent$n * qd0 / 2; recov0 <- ent$n * qr0 / 2
  ent$n <- ent$n - deaths0 - recov0
  st <- bind_rows(st, ent |> select(sex, e, d, a, n))

  # conversions at NRA (year-end)
  b <- t - st$a
  keep <- pmin(1, pmax(0, nra_of(b) - st$a))
  conv <- st$n * (1 - keep)
  conv_age_raw <- tibble(sex = st$sex, a = st$a, conv = conv)
  conv_detail <- tibble(sex = st$sex, e = st$e, d = st$d, a = st$a, conv = conv) |> filter(conv > 1e-6) |>
    group_by(sex, e, d, a) |> summarise(conv = sum(conv), .groups = "drop")
  st$n <- st$n * keep
  st <- st |> filter(n > 1e-6) |> group_by(sex, e, d, a) |> summarise(n = sum(n), .groups = "drop")

  cp <- sum(st$n * ibnr_year(t)[cbind(match(st$sex, sexes), pmin(st$d, 120) + 1)])
  flows <- tibble(year = t, entitlements = sum(ent$n + deaths0 + recov0),
                  deaths = sum(deaths) + sum(deaths0), recoveries = sum(recov) + sum(recov0),
                  conversions = sum(conv), entitled = sum(st$n), stock = cp)
  age_flows <- bind_rows(
    tibble(sex = state$sex, age = state$a, deaths = deaths, recov = recov, avg = state$n),
    tibble(sex = ent$sex, age = ent$a - 1L, deaths = deaths0, recov = recov0, avg = (ent$n + deaths0 + recov0) / 2))
  conv_age <- conv_age_raw |> rename(age = a) |> filter(conv > 0) |>
    group_by(sex, age) |> summarise(conv = sum(conv), .groups = "drop")
  list(state = st, flows = flows, age_flows = age_flows, conv_age = conv_age, conv_detail = conv_detail,
       entrants = ex |> select(sex, a, exposure, new),
       ent_detail = ent |> mutate(n = n + deaths0 + recov0) |> group_by(sex, e) |> summarise(n = sum(n), .groups = "drop"))
}

# ---- Starting state, factors -------------------------------------------------------------
state0 <- stock |> transmute(sex = as.character(sex), e = as.integer(entl_age), d = as.integer(duration),
                             a = as.integer(attained_age), n = entitled) |>
  group_by(sex, e, d, a) |> summarise(n = sum(n), .groups = "drop")

# Death and recovery factors at 2025 levels: age-sex-adjusted rates of the base
# tables on the December 2025 stock, scaled to the memo's 2025 values with the
# sex split of Study 130 Table 5 (2024).
t5 <- di$hist_terms |> filter(year == 2024)
probe <- step(state0, 2026, 0, c(1, 1), c(1, 1))$age_flows
base_d <- adj_rate(probe |> group_by(sex, age) |> summarise(value = sum(deaths), .groups = "drop"),
                   probe |> group_by(sex, age) |> summarise(value = sum(avg), .groups = "drop"), std_dib)
base_r <- adj_rate(probe |> group_by(sex, age) |> summarise(value = sum(recov), .groups = "drop"),
                   probe |> group_by(sex, age) |> summarise(value = sum(avg), .groups = "drop"), std_dib)
split <- function(col, total) { v <- t5[[col]][match(c("M", "F", "T"), t5$sex)]; total * v[1:2] / v[3] }
target_d25 <- split("death_adj", 26.3)
target_r25 <- split("rec_adj", 18.7)
target_r_ult <- split("rec_adj", 11.1)
dfac <- target_d25 / base_d[c("M", "F")]
rfac25 <- target_r25 / base_r[c("M", "F")]
rfac_ult <- target_r_ult / base_r[c("M", "F")]
cat("Death factor (2025 level) M/F:", round(dfac, 3), " | recovery factor 2025 M/F:", round(rfac25, 3),
    "-> ultimate", round(rfac_ult, 3), "\n")
# Death factor relative to 2025 improvement: impr_arr is relative to 2025, so
# dfac applies as is from 2026 (one year of improvement already in impr 2026).

vc5 <- di$vc5 |> select(year, dw)

# ---- Run ---------------------------------------------------------------------------------
adj_rates <- function(age_flows) {
  age_flows |> group_by(year) |> group_modify(function(g, k) {
    den <- g |> group_by(sex, age) |> summarise(value = sum(avg), .groups = "drop")
    dd <- adj_rate(g |> group_by(sex, age) |> summarise(value = sum(deaths), .groups = "drop"), den, std_dib)
    rr <- adj_rate(g |> group_by(sex, age) |> summarise(value = sum(recov), .groups = "drop"), den, std_dib)
    tibble(death_adj = unname(dd["total"]), recovery_adj = unname(rr["total"]))
  }) |> ungroup()
}
# Death factor in year t: dfac x exp(-g (t - 2025)) on top of general-population
# improvement (g = extra improvement, DP-02).
run_projection <- function(dfac, rfac25, rfac_ult, g, ipost = 1, years = 2026:2100) {
  rfac_t <- function(t) if (t >= 2035) rfac_ult else rfac25 + (rfac_ult - rfac25) * (t - 2025) / 10
  state <- state0
  flows <- list(); age_out <- list(); stock_out <- list(); conv_out <- list(); ifac <- c()
  state_out <- list(); ent_out <- list(); convd_out <- list()
  for (t in years) {
    rf <- rfac_t(t); df <- dfac * exp(-g * (t - 2025))
    if (t <= 2035) {
      target <- 1000 * vc5$dw[vc5$year == t]
      f <- function(x) step(state, t, x, df, rf)$flows$stock - target
      x <- uniroot(f, c(0, 3), tol = 1e-6)$root
    } else x <- ipost
    res <- step(state, t, x, df, rf)
    state <- res$state; ifac[as.character(t)] <- x
    flows[[length(flows) + 1]] <- res$flows
    age_out[[length(age_out) + 1]] <- res$age_flows |> mutate(year = t)
    conv_out[[length(conv_out) + 1]] <- res$conv_age |> mutate(year = t)
    ib <- ibnr_year(t)[cbind(match(state$sex, sexes), pmin(state$d, 120) + 1)]
    state_out[[length(state_out) + 1]] <- state |> mutate(cp = n * ib, year = t)
    ent_out[[length(ent_out) + 1]] <- res$ent_detail |> mutate(year = t)
    convd_out[[length(convd_out) + 1]] <- res$conv_detail |> mutate(year = t)
    stock_out[[length(stock_out) + 1]] <- state |> mutate(cp = n * ib) |> group_by(sex, a) |>
      summarise(n_dinadd = sum(n[d >= 4]), cp = sum(cp), n = sum(n), .groups = "drop") |> mutate(year = t)
  }
  af <- bind_rows(age_out)
  list(flows = bind_rows(flows), stock_age = bind_rows(stock_out), age_flows = af, conv_age = bind_rows(conv_out),
       adj = adj_rates(af), ifac = ifac, state = state,
       state_by_year = bind_rows(state_out), entitlements_by_age = bind_rows(ent_out), conversions_detail = bind_rows(convd_out))
}

# Calibrate to the memo's published outcomes (DP-02, DP-03), by fixed-point
# iteration: death termination 26.3 in 2026 and 12.5 in 2100; recovery 18.7 in
# 2026 and an average of 11.1 over 2036-2100. Sex splits stay as set above.
# The same loop sets one constant incidence factor for 2036-2100 (DP-08) so the
# average gap of current pay to TR V.C5 over 2036-2100 is zero.
g <- 0; ipost <- 1
for (it in 1:10) {
  r <- run_projection(dfac, rfac25, rfac_ult, g, ipost)
  a <- r$adj
  gap <- r$flows |> filter(year >= 2036) |> inner_join(vc5, by = "year") |>
    summarise(g = mean(stock / (1000 * dw) - 1)) |> pull(g)
  m <- c(d26 = a$death_adj[a$year == 2026], d2100 = a$death_adj[a$year == 2100],
         r26 = a$recovery_adj[a$year == 2026], rult = mean(a$recovery_adj[a$year >= 2036]))
  cat(sprintf("Calibration %d: death 2026 %.2f, 2100 %.2f | recovery 2026 %.2f, 2036-2100 %.2f | g %.4f | post-2035 incidence %.4f, mean gap %+.2f%%\n",
              it, m["d26"], m["d2100"], m["r26"], m["rult"], g, ipost, 100 * gap))
  if (max(abs(m / c(26.3, 12.5, 18.7, 11.1) - 1)) < 0.003 && abs(gap) < 0.001) break
  dfac <- dfac * 26.3 / m["d26"]
  g <- g + log(m["d2100"] / 12.5) / (2100 - 2026)
  rfac25 <- rfac25 * 18.7 / m["r26"]
  rfac_ult <- rfac_ult * 11.1 / m["rult"]
  ipost <- ipost / (1 + gap)^1.3            # stock responds less than one-for-one within the window
}
flows <- r$flows; stock_age <- r$stock_age; age_flows <- r$age_flows; ifac <- r$ifac; state <- r$state
conv_age <- r$conv_age
state_by_year <- r$state_by_year; entitlements_by_age <- r$entitlements_by_age; conversions_detail <- r$conversions_detail
cat("Final factors: death M/F", round(dfac, 3), "extra improvement", round(100 * g, 2), "%/yr | recovery 2025",
    round(rfac25, 3), "ultimate", round(rfac_ult, 3), "| incidence 2036-2100", round(ipost, 4), "\n")

# ---- Checks ---------------------------------------------------------------------------------
cat("\nIncidence factor 2026-2035 (1 = ultimate):", paste(round(ifac[as.character(2026:2035)], 3), collapse = " "),
    "| 2036-2100:", round(ipost, 4), "\n")

adj_by_year <- r$adj

prev <- stock_age |> rename(age = a) |> group_by(year) |> group_modify(function(g, k) {
  den <- tibble(sex = rep(sexes, each = 121), age = rep(0:120, 2),
                value = c(dins_arr["M", , as.character(k$year)], dins_arr["F", , as.character(k$year)]))
  p <- adj_rate(g |> transmute(sex, age, value = n), den, std_dins)
  tibble(prev_adj = p["total"], prev_adj_M = p["M"], prev_adj_F = p["F"])
}) |> ungroup()

checks <- flows |> left_join(vc5, by = "year") |> mutate(tr_dw = 1000 * dw, gap_pct = 100 * (stock / tr_dw - 1)) |>
  left_join(adj_by_year, by = "year") |> left_join(prev, by = "year") |> select(-dw)
show <- checks |> filter(year %in% c(2026, 2030, 2035, 2040, 2050, 2060, 2075, 2090, 2100)) |>
  transmute(year, stock_k = round(stock / 1000), tr_k = round(tr_dw / 1000), gap_pct = round(gap_pct, 1),
            entitled_k = round(entitled / 1000), entitlements_k = round(entitlements / 1000), deaths_k = round(deaths / 1000),
            recoveries_k = round(recoveries / 1000), conversions_k = round(conversions / 1000),
            death_adj = round(death_adj, 1), recovery_adj = round(recovery_adj, 1), prev_adj = round(prev_adj, 1))
cat("\nDisabled workers (thousands) vs TR V.C5, flows, and age-sex-adjusted rates per 1,000:\n")
print(show, n = Inf, width = 200)
cat("\nMemo targets: death 26.3 (2025) -> 12.5 (2100); recovery 18.7 (2025) -> 11.1 (2036-2100 average);",
    "prevalence 30.7 (2025) -> 40.7 (2100; men 40.3, women 41.2)\n")
cat("Ours: recovery average 2036-2100", round(mean(checks$recovery_adj[checks$year >= 2036]), 1),
    "| prevalence 2100 men", round(checks$prev_adj_M[checks$year == 2100], 1),
    "women", round(checks$prev_adj_F[checks$year == 2100], 1), "\n")
cat("Entitled vs in current pay, 2026:", round(flows$entitled[1]), "/", round(flows$stock[1]), "\n")
cat("2026 flows (entitled basis) vs 2025 actuals (6.F2, current pay): deaths", round(flows$deaths[1]), "(217,802) | recoveries",
    round(flows$recoveries[1]), "(84,602) | conversions", round(flows$conversions[1]), "(457,350)\n")

dir.create("outputs", showWarnings = FALSE)
write.csv(checks, "outputs/di_projection_checks.csv", row.names = FALSE)
saveRDS(list(flows = flows, stock_age = stock_age, age_flows = age_flows, conv_age = conv_age, incidence_factor = ifac,
             death_factor = dfac, death_extra_improvement = g, incidence_factor_post2035 = ipost,
             recovery_factor = list(y2025 = rfac25, ultimate = rfac_ult),
             checks = checks, state_2100 = state,
             state_by_year = state_by_year, entitlements_by_age = entitlements_by_age, conversions_detail = conversions_detail),
        "data/di_projection.rds")
cat("Saved data/di_projection.rds\n")
