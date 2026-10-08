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
