# 05_net_immigration.R
#
# Derives net immigration by single age, sex and year from the Trustees'
# population and death probabilities: whoever is in the population at the end
# of a year and wasn't there (alive, one year younger) at the end of the
# previous year arrived during the year, net of those who left.
#
#   net(t, a) = P(t, a) - P(t-1, a-1) x S(t, a)
#
# where P is the December 31 population and S is the probability of surviving
# year t for someone who was a-1 at the end of t-1. They spend roughly half the
# year at each age, so S = 1 - (q(t, a-1) + q(t, a)) / 2.
#
# Births are excluded by construction. Everyone aged 0 on December 31 of year t
# was born in year t or arrived as an infant; the population file can't tell
# them apart, so age 0 is dropped entirely. The derivation starts at age 1,
# whose previous-year counterpart (age 0 on December 31 of t-1) already
# existed. The cost is that immigrants who arrive in their birth year are
# missed until the following year's age-1 count, which absorbs them.
# Age 100 is "100 and older" (an open group), so it is dropped too.
#
# This is total net immigration (lawful permanent residents plus temporary or
# unlawfully present, net of emigration). Splitting it by legal status waits
# for OCACT's 2010 study of the unauthorized population.
#
# Input:  data/population_dec.rds, data/death_probs.rds (scripts 01, 02)
# Check:  TR Table V.A2, total net change in the population from immigration
# Output: data/net_immigration.rds  (year, age 1-99, sex, net)

library(dplyr)
library(tidyr)
source("R/read_tr.R")

pop <- readRDS("data/population_dec.rds") |>
  group_by(year, sex, age) |> summarise(pop = sum(pop), .groups = "drop")
qx <- readRDS("data/death_probs.rds") |> select(year, sex, age, qx)

# ---- Step 1: residual by cohort ---------------------------------------------
prev <- pop |>
  transmute(year = year + 1L, sex, age = age + 1L, pop_prev = pop)   # one year on

net_immigration <- pop |>
  filter(age >= 1, age <= 99) |>          # no age 0 (births), no open 100+ group
  inner_join(prev, by = c("year", "sex", "age")) |>
  left_join(qx |> rename(q_now = qx), by = c("year", "sex", "age")) |>
  left_join(qx |> mutate(age = age + 1L) |> rename(q_prev = qx), by = c("year", "sex", "age")) |>
  mutate(survival = 1 - (q_prev + q_now) / 2,
         net = pop - pop_prev * survival) |>
  select(year, age, sex, net)

stopifnot(!any(net_immigration$age == 0),
          !anyNA(net_immigration$net),
          min(net_immigration$year) == 1941, max(net_immigration$year) == 2100)

# ---- Step 2: compare totals with TR Table V.A2 -------------------------------
# V.A2's last column is the total net change in the population from
# immigration (LPR plus temporary or unlawfully present, in thousands).
# Projected years should agree closely; historical years can differ because
# history also absorbs census revisions and coverage changes.

va2 <- read_tr_single_year("V.A2", c("lpr_in", "lpr_out", "lpr_aos", "lpr_net",
                                      "oth_in", "oth_out", "oth_aos", "oth_net", "total_net"))
cmp <- net_immigration |>
  group_by(year) |> summarise(derived = sum(net) / 1000, .groups = "drop") |>
  inner_join(va2 |> select(year, section, lpr_net, oth_net, total_net), by = "year") |>
  mutate(gap = derived - total_net, pct_gap = 100 * gap / total_net)

cat("Net immigration (thousands): derived from population vs TR V.A2\n")
print(cmp |> filter(year %in% c(1990, 2000, 2010, 2019, 2026:2030, 2040, 2050, 2075, 2100)) |>
        mutate(across(where(is.double), ~ round(.x, 1))), n = Inf)

proj <- cmp |> filter(section == "Intermediate")
cat("\nProjection years: largest gap", round(max(abs(proj$gap)), 1), "thousand;",
    "largest % gap where V.A2 exceeds 500k:",
    round(max(abs(proj$pct_gap[abs(proj$total_net) > 500])), 2), "%\n")
stopifnot(max(abs(proj$gap)) < 30)

cat("\nAge pattern, 2050 (thousands):\n")
print(net_immigration |> filter(year == 2050) |>
        mutate(ages = cut(age, c(0, 9, 19, 29, 39, 49, 64, 99))) |>
        group_by(ages, sex) |> summarise(net = round(sum(net) / 1000), .groups = "drop") |>
        pivot_wider(names_from = sex, values_from = net))

saveRDS(net_immigration, "data/net_immigration.rds")
cat("\nSaved data/net_immigration.rds:", nrow(net_immigration), "rows\n")

# ==============================================================================
# Part 2: immigration by legal status
#
# The insured-status simulation needs two things by age, sex and year:
#   (a) lawful permanent resident (LPR) entrants: people who join the
#       work-authorized population with no US earnings history
#   (b) the stock of temporary or unlawfully present immigrants, whom OCACT
#       excludes from the main simulation and insures at a much lower rate
#
# Totals come from the Trustees (TR Table V.A2; 2026 Demographic Assumptions
# memo, section 3.5 and Table 3.5). Nothing published gives their ages, so both
# use one age-sex pattern for arrivals: the pattern of total net immigration
# derived above, averaged over 2030-2050 (when it is positive and stable).
# ==============================================================================

# ---- Step 3: the arrival age pattern ------------------------------------------
arrival_pattern <- net_immigration |>
  filter(year %in% 2030:2050) |>
  group_by(age, sex) |> summarise(net = mean(net), .groups = "drop") |>
  mutate(net = pmax(net, 0), share = net / sum(net)) |>
  select(age, sex, share)
stopifnot(abs(sum(arrival_pattern$share) - 1) < 1e-9)

# ---- Step 4: LPR entrants -----------------------------------------------------
# V.A2's net LPR change = new LPR arrivals - legal emigrants + adjustments of
# status. All three move people into (or out of) the work-authorized group.
# People adjusting status had lived here as temporary or unlawfully present
# immigrants, so they may have some covered earnings; OCACT's simulation treats
# all LPR entrants alike (prior earnings nullified), and so do we.
lpr_entrants <- va2 |>
  select(year, lpr_net) |>
  filter(!is.na(lpr_net)) |>
  expand_grid(arrival_pattern) |>
  transmute(year, age, sex, entrants = 1000 * lpr_net * share)

# ---- Step 5: the temporary or unlawfully present stock, by age ----------------
# Total stock, end of year (millions), from the Trustees:
#   1963: 0 (OCACT's series starts in 1963)
#   1990: 2.6  (1990 Census, excluding those later legalized under IRCA, who
#               became LPRs)
#   1996: 5.0  (October 1996)
#   1999: 9.9  (DHS, beginning of 2000; the 2000 Census showed the 1990s had
#               been underestimated, hence the jump)
#   2005: 11.7, 2008: 13.5, 2013: 12.5, 2020: 13.4, 2024: 16.9, 2025: 17.3
# Between anchors the total is interpolated linearly. From 2026 on it follows
# V.A2's flows exactly: arrivals less departures and adjustments of status,
# less deaths. The memo's projected totals for 2029 (15.6 million) and 2100
# (28.7 million) are checks.
#
# By age: each year the stock ages one year and loses deaths; arrivals come in
# with the arrival pattern; exits (departures and adjustments of status) are
# spread over ages in proportion to stock x exp(-beta x (age - 30)). The memo
# says OCACT assumes "higher rates of emigration for recent entrants", who are
# mostly young; beta > 0 tilts exits toward younger ages. With V.A2 fixing the
# flows, beta governs how many in the stock die, so it is solved to hit the
# memo's 2100 total. Before 2026, exits are assumed to be 3% of the stock a
# year and arrivals are whatever hits the anchored total.

anchors <- tibble(year = c(1963, 1990, 1996, 1999, 2005, 2008, 2013, 2020, 2024, 2025),
                  stock = c(0, 2.6, 5.0, 9.9, 11.7, 13.5, 12.5, 13.4, 16.9, 17.3) * 1e6)
hist_total <- approx(anchors$year, anchors$stock, xout = 1963:2025)$y
names(hist_total) <- 1963:2025
tu_flows <- va2 |> filter(year >= 2026) |>
  transmute(year, arrivals = 1000 * oth_in, exits = 1000 * (oth_out + oth_aos))

# Work in plain matrices: rows = ages 0..100, columns = M, F.
ages_all <- 0:100
share_mat <- matrix(0, 101, 2)
share_mat[cbind(arrival_pattern$age + 1, as.integer(arrival_pattern$sex))] <- arrival_pattern$share
q_arr <- array(NA_real_, c(101, 2, length(1900:2100)))       # qx at ages 0..100
qq <- qx |> filter(age <= 100)
q_arr[cbind(qq$age + 1, as.integer(qq$sex), qq$year - 1899)] <- qq$qx

project_stock <- function(beta, keep = FALSE) {
  tilt <- exp(-beta * (ages_all - 30))
  stock <- matrix(0, 101, 2)
  out <- if (keep) vector("list", length(1964:2100)) else NULL
  for (yr in 1964:2100) {
    q <- q_arr[, , yr - 1899]
    # survive the year: half the year at last year's age, half at this year's
    q_prev <- rbind(q[1, ], q[-101, ])
    s <- 1 - (q_prev + q) / 2
    aged <- rbind(0, stock[-101, ])                     # age everyone one year
    aged[101, ] <- aged[101, ] + stock[101, ]           # 100+ stays open-ended
    aged <- aged * s
    if (yr <= 2025) {
      exits <- 0.03 * aged
      arrivals <- hist_total[as.character(yr)] - sum(aged - exits)
      if (arrivals < 0) { exits <- exits - arrivals * aged / sum(aged); arrivals <- 0 }
    } else {
      f <- tu_flows[tu_flows$year == yr, ]
      w <- aged * tilt
      exits <- f$exits * w / sum(w)
      exits <- pmin(exits, aged)                        # never more than present
      arrivals <- f$arrivals
    }
    stock <- aged - exits + arrivals * share_mat
    if (keep) out[[yr - 1963]] <- stock
  }
  if (keep) out else sum(stock)
}

# Above about 0.1 the tilt empties the youngest ages and caps bind, so the
# search stays in the range where the 2100 total falls steadily as beta rises.
beta <- uniroot(function(b) project_stock(b) - 28.7e6, c(0, 0.05))$root
cat("\nExit age tilt solved to hit 28.7 million in 2100: beta =", round(beta, 4),
    "(exit weight at age 20 vs 50:", round(exp(30 * beta), 2), "x)\n")
stocks <- project_stock(beta, keep = TRUE)
tu_stock <- bind_rows(lapply(seq_along(stocks), function(i) {
  m <- stocks[[i]]
  tibble(year = 1963L + i, age = rep(ages_all, 2),
         sex = factor(rep(c("M", "F"), each = 101), levels = c("M", "F")),
         stock = c(m[, 1], m[, 2]))
}))

tu_total <- tu_stock |> group_by(year) |> summarise(stock = sum(stock) / 1e6)
cat("\nTemporary or unlawfully present stock (millions), ours vs Trustees:\n")
checks <- tibble(year = c(2000, 2013, 2025, 2029, 2100), trustees = c(9.9, 12.5, 17.3, 15.6, 28.7))
print(checks |> left_join(tu_total, by = "year") |> mutate(gap = round(stock - trustees, 2), stock = round(stock, 2)))

# Must stay below the total population at every age.
share_check <- tu_stock |>
  inner_join(pop, by = c("year", "age", "sex")) |>
  mutate(share = stock / pop)
cat("Largest share of any age-sex group that is temporary or unlawfully present:",
    round(max(share_check$share, na.rm = TRUE), 3), "\n")
stopifnot(all(share_check$share < 0.6, na.rm = TRUE))
cat("Share of population, ages 25-54, selected years:\n")
print(share_check |> filter(age %in% 25:54, year %in% c(2000, 2025, 2050, 2100)) |>
        group_by(year) |> summarise(share = round(sum(stock) / sum(pop), 3)))

saveRDS(list(arrival_pattern = arrival_pattern, lpr_entrants = lpr_entrants,
             tu_stock = tu_stock),
        "data/immigration_status.rds")
cat("\nSaved data/immigration_status.rds\n")
