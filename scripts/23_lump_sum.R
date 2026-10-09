# 23_lump_sum.R
#
# Lump-sum death payments, 2024-2100 (methodology equation 3.3.13; DECISIONS.md LS-01, LS-02).
#
#   EXPOSURE(age group, sex, year) = deaths x P(fully insured) x P(survived by a spouse or child)
#   LUMSUM = EXPOSURE x BASE,  BASE = actual / estimated deceased workers with a payment in 2024
#                                     (TRYR - 2, as OCACT)
#
#   deaths              population at the start of the year x death probability (scripts/01, 02)
#   fully insured       scripts/07
#   spouse or child     married share at that age and sex + (1 - married share) x P(a child under
#                       18), the child probability from the expected number of children under 18 per
#                       person of that age and sex (birth-age kernels of scripts/15)
# Payments per deceased worker (more than one when several children are paid) are held at 2024's
# ratio; each payment is $255, fixed in law.
#
# Input:  data/population_dec.rds, data/death_probs.rds, data/insured_rates_calibrated.rds;
#         Supplement 2026 Table 6.D9
# Output: data/lump_sum.rds

suppressMessages({library(dplyr); library(tidyr)})
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))
years <- 2024:2100
pop <- readRDS("data/population_dec.rds") |> mutate(sex = as.character(sex), age = pmin(as.integer(age), 100L))
q <- readRDS("data/death_probs.rds") |> mutate(sex = as.character(sex), age = as.integer(age)) |> filter(age <= 100)
fi <- readRDS("data/insured_rates_calibrated.rds") |> mutate(sex = as.character(sex), age = as.integer(age)) |>
  group_by(year, sex, age) |> summarise(fully = mean(fully), .groups = "drop")

# Population at the start of year t = December of t - 1
p0 <- pop |> group_by(year = year + 1L, sex, age) |> summarise(mar = sum(pop[marital == "married"]), pop = sum(pop), .groups = "drop") |>
  filter(year %in% years)
kids <- pop |> filter(age <= 17) |> group_by(year = year + 1L) |> summarise(n = list(tapply(pop, age, sum)), .groups = "drop")
kern <- function(a, m, s) { w <- dnorm(a, m, s); w / sum(w) }
A <- 15:55; fb <- list(F = kern(A, 29, 5.5), M = kern(A, 31.5, 6.5))
child_exp <- p0 |> filter(age >= 15) |> rowwise() |> mutate(e = {
  k <- kids$n[[match(year, kids$year)]]; cs <- 0:17; ab <- age - cs; ok <- ab >= 15 & ab <= 55
  if (any(ok) && pop > 0) sum(k[cs[ok] + 1] * fb[[sex]][ab[ok] - 14]) / pop else 0
}) |> ungroup() |> select(year, sex, age, e)

ex <- p0 |> filter(age >= 20) |>
  inner_join(q |> select(year, sex, age, qx), by = c("year", "sex", "age")) |>
  inner_join(fi, by = c("year", "sex", "age")) |>
  left_join(child_exp, by = c("year", "sex", "age")) |>
  mutate(e = coalesce(e, 0), p_mar = ifelse(pop > 0, mar / pop, 0),
         p_sc = p_mar + (1 - p_mar) * (1 - exp(-e)),
         exposure = pop * qx * fully * p_sc)

d9 <- as.matrix(readxl::read_excel("data-raw/supplement/2026/6d.xlsx", sheet = "6.D9", col_names = FALSE, col_types = "text", .name_repair = "minimal"))
act <- tibble(year = suppressWarnings(as.integer(d9[, 1])), workers = num(d9[, 3]), payments = num(d9[, 4])) |> filter(!is.na(year), !is.na(workers))
est <- ex |> group_by(year) |> summarise(est = sum(exposure), deaths = sum(pop * qx), .groups = "drop")
base <- act$workers[act$year == 2024] / est$est[est$year == 2024]
per_worker <- act$payments[act$year == 2024] / act$workers[act$year == 2024]
out <- est |> mutate(lumsum = est * base, payments = lumsum * per_worker, amount = 255 * payments)
cat("BASE (actual / estimated, 2024):", round(base, 3), "  payments per deceased worker:", round(per_worker, 3), "\n")
cat("2025 out of sample: model", round(out$lumsum[out$year == 2025]), "deceased workers vs", act$workers[act$year == 2025], "actual (6.D9)\n")
print(out |> filter(year %in% c(2024, 2025, 2030, 2040, 2050, 2075, 2100)) |>
        transmute(year, deaths_k = round(deaths / 1e3), deceased_workers_k = round(lumsum / 1e3),
                  payments_k = round(payments / 1e3), amount_millions = round(amount / 1e6)))
saveRDS(list(lump_sum = out, exposure = ex, base = base, per_worker = per_worker), "data/lump_sum.rds")
cat("Saved data/lump_sum.rds\n")
