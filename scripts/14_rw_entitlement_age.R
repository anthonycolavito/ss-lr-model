# 14_rw_entitlement_age.R
#
# Retired workers by age at entitlement (methodology 3.3.c, end of equation
# 3.3.2), 2025-2100, by attained age and sex: needed for benefit levels
# (reduction for early claiming, delayed retirement credits).
#
# OCACT splits each attained age's retired workers by the incidence of
# entitlement at each age in that cohort (increase in prevalence), starting
# from a 100% MBR count by attained age x entitlement age for December 2025.
# That count isn't published; we build each cohort's entitlement-age weights
# (DECISIONS.md EA-01 to EA-04):
#   entitled through 2025   Supplement 6.B5.1: retired-worker entitlements by
#                           year and age at entitlement, 1998-2025 (disability
#                           conversions excluded). A cohort born b entitled at
#                           age a in year b + a. 67-69 is split with our
#                           prevalence increments; 70+ is age 70. Years before
#                           1998 take 1998's age mix.
#   entitled from 2026      OCACT's incidence: max(0, p(b, a) - p(b, a-1)) x
#                           exposure, from scripts/13.
# Retired workers at attained age N in year t (non-converted) are then split
# by their cohort's weights for entitlement ages <= min(N, 70). Converted DI
# beneficiaries are a separate class (entitled at NRA, 100% of PIA).
#
# Input:  data/retired_workers.rds (13), Supplement 6.B5.1
# Output: data/rw_entitlement_age.rds

library(dplyr)
library(tidyr)
source("R/read_oasi.R")

rw <- readRDS("data/retired_workers.rds")
sexes <- c("M", "F")

# Prevalence by year, age, sex: history (2007-2025) and projection (2026-2100)
prev <- bind_rows(rw$hist |> transmute(year, sex, age, p = prev, exposure, rwn),
                  rw$proj |> transmute(year, sex, age, p, exposure, rwn)) |>
  mutate(b = year - age)

# ---- Entitlements by cohort and age at entitlement ----------------------------------------------
# Through 2025: 6.B5.1 counts (thousands x percent), conversions excluded.
t6 <- read_supp_6b51()
hist_ent <- t6 |> pivot_longer(c(a62, a63, a64, a65, a66, a67_69, a70p), names_to = "cls", values_to = "pct") |>
  mutate(n = 1000 * total * pct / 100)
# 67-69 split by our prevalence increments at 67, 68, 69 that year (equal thirds where unavailable)
inc <- prev |> arrange(sex, b, age) |> group_by(sex, b) |>
  mutate(inc = pmax(0, p - lag(p, default = 0)) * exposure) |> ungroup()
w6769 <- inc |> filter(age %in% 67:69, year %in% 2007:2025) |> group_by(year, sex) |>
  mutate(w = ifelse(sum(inc) > 0, inc / sum(inc), 1 / 3)) |> ungroup() |> select(year, sex, age, w)
ent_hist <- bind_rows(
  hist_ent |> filter(cls %in% c("a62", "a63", "a64", "a65", "a66", "a70p")) |>
    transmute(year, sex, ae = c(a62 = 62L, a63 = 63L, a64 = 64L, a65 = 65L, a66 = 66L, a70p = 70L)[cls], n),
  hist_ent |> filter(cls == "a67_69") |> select(year, sex, n) |>
    expand_grid(ae = 67:69) |>
    left_join(w6769 |> rename(ae = age), by = c("year", "sex", "ae")) |>
    mutate(n = n * coalesce(w, 1 / 3)) |> select(year, sex, ae, n)
)
# Before 1998: 1998's age mix (scaled to an arbitrary level - only shares within a cohort matter,
# and a cohort mixing pre-1998 and later years uses counts, see EA-02)
mix98 <- ent_hist |> filter(year == 1998) |> select(sex, ae, n98 = n)

# From 2026: incidence from the projection
ent_proj <- inc |> filter(year >= 2026, age >= 62, age <= 70) |> transmute(year, sex, ae = age, n = inc)

ent <- bind_rows(ent_hist, ent_proj) |> mutate(b = year - ae)
cohort_w <- function(bb, sx) {
  w <- ent |> filter(b == bb, sex == sx) |> select(ae, n)
  missing <- setdiff(62:70, w$ae)
  if (length(missing)) {   # entitlement years before 1998
    m <- mix98 |> filter(sex == sx, ae %in% missing) |> transmute(ae, n = n98)
    w <- bind_rows(w, m)
  }
  w |> mutate(w = n / sum(n))
}
cohorts <- expand_grid(b = 1915:2038, sex = sexes)
W <- bind_rows(lapply(seq_len(nrow(cohorts)), function(i) {
  cohort_w(cohorts$b[i], cohorts$sex[i]) |> mutate(b = cohorts$b[i], sex = cohorts$sex[i])
}))

# ---- Split retired workers at each attained age ---------------------------------------------------
split_rw <- prev |> filter(year >= 2025) |> select(year, sex, age, b, rwn) |>
  inner_join(W |> select(b, sex, ae, w), by = c("b", "sex"), relationship = "many-to-many") |>
  filter(ae <= pmin(age, 70)) |>
  group_by(year, sex, age) |> mutate(share = w / sum(w)) |> ungroup() |>
  transmute(year, sex, age, ae, number = rwn * share)
conv <- rw$conv_stock |> filter(year >= 2025, age >= 62) |> transmute(year, sex, age, ae = NA_integer_, number = conv_stock)
out <- bind_rows(split_rw |> mutate(class = "retired"), conv |> mutate(class = "converted"))

# ---- Checks ---------------------------------------------------------------------------------------
cat("Retired workers by age at entitlement, December 2025 (millions), with converted DI:\n")
print(out |> filter(year == 2025) |> mutate(ae = ifelse(class == "converted", "conv", as.character(ae))) |>
        group_by(sex, ae) |> summarise(n = round(sum(number) / 1e6, 2), .groups = "drop") |>
        pivot_wider(names_from = sex, values_from = n))
chk <- out |> filter(year == 2025) |> summarise(t = sum(number))
cat("Total, December 2025:", round(chk$t), "(Supplement 5.A1.1: 53,624,664)\n")

new26 <- ent_proj |> filter(year == 2026) |> group_by(sex) |> mutate(pct = 100 * n / sum(n)) |> ungroup()
act25 <- t6 |> filter(year == 2025) |> mutate(tot = a62 + a63 + a64 + a65 + a66 + a67_69 + a70p) |>
  transmute(sex, `62` = a62 / tot, `63` = a63 / tot, `64` = a64 / tot, `65` = a65 / tot, `66` = a66 / tot,
            `67-69` = a67_69 / tot, `70` = a70p / tot) |> mutate(across(-sex, ~ round(100 * .x, 1)))
cat("\nNew entitlements by age, percent (conversions excluded): 2025 actual (6.B5.1) vs model 2026:\n")
print(act25 |> mutate(src = "2025 actual"))
print(new26 |> mutate(ae = ifelse(ae %in% 67:69, "67-69", as.character(ae))) |> group_by(sex, ae) |>
        summarise(pct = round(sum(pct), 1), .groups = "drop") |> pivot_wider(names_from = ae, values_from = pct) |>
        mutate(src = "model 2026"))
cat("New entitlements 2026, model (thousands):", round(sum(new26$n) / 1000),
    "| 2025 actual excl. conversions:", round(sum(t6$total[t6$year == 2025] * (1 - t6$conv[t6$year == 2025] / 100))), "\n")

saveRDS(list(rw_ae = out, weights = W, entitlements = ent), "data/rw_entitlement_age.rds")
cat("Saved data/rw_entitlement_age.rds\n")
