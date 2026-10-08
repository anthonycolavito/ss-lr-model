# 03_program_parameters.R
#
# Collects the Social Security program rules the model needs, from ranypia's
# current_law() policy (2026 TR intermediate), and checks them against the
# Trustees Report. Two tables:
#
#   params_by_year    one row per calendar year, 1937-2105:
#                     AWI, COLA, taxable maximum, old-law base, QC amount,
#                     PIA and family-maximum bend points
#   params_by_cohort  one row per birth year, 1875-2043 (turning 62 in
#                     1937-2105): NRA, delayed retirement credit, and the
#                     share of PIA paid when claiming at 62, 65, 66, 67, 70
#
# ranypia is the single source of program rules for both this model and the
# benefit calculator, so a reform (a new NRA schedule, new PIA factors) is
# written once, as a ranypia policy change.
#
# Input:  ranypia::current_law()
# Check:  data-raw/tr2026/TRTables_TR2026.xlsx, Tables V.C1, V.C2, V.C3
# Output: data/params_by_year.rds, data/params_by_cohort.rds

library(dplyr)
library(tidyr)
library(ranypia)
source("R/read_tr.R")

policy <- current_law(alt = 2)
policy_years <- 1937:2105                 # ranypia's series run 1937-2105
stopifnot(length(policy$awi) == length(policy_years))

# ---- Step 1: parameters by calendar year -----------------------------------
# Bend points are fixed by the year a worker becomes eligible (turns 62), and
# are published for every year, so we list them by calendar year here.

# The 1977 amendments' PIA formula starts with workers eligible in 1979, so
# earlier years get NA.
bp_years <- policy_years[policy_years >= 1979]
bp <- mfb_bp <- matrix(NA_real_, length(policy_years), 3)
bp[policy_years >= 1979, 1:2]     <- bend_points(bp_years, policy)
mfb_bp[policy_years >= 1979, 1:3] <- family_max_bend_points(bp_years, policy)

params_by_year <- tibble(
  year      = policy_years,
  awi       = policy$awi,          # average wage index
  cola      = policy$cola,         # COLA effective December of the year, %
  taxmax    = policy$taxmax,       # contribution and benefit base
  base77    = policy$base77,       # old-law base (pre-1977 amendments)
  qc_amount = policy$qc_amount,    # earnings for one quarter of coverage
  pia_bp1   = bp[, 1], pia_bp2 = bp[, 2],
  mfb_bp1   = mfb_bp[, 1], mfb_bp2 = mfb_bp[, 2], mfb_bp3 = mfb_bp[, 3]
)

# ---- Step 2: parameters by birth cohort ------------------------------------
# NRA (in months) and the monthly delayed credit depend on the year a worker
# turns 62. From those and the reduction rules, compute the share of PIA paid
# when claiming at selected ages, the same way Table V.C3 reports it.

pct_of_pia <- function(claim_age_months, nra_months, drc_monthly) {
  early <- pmax(nra_months - claim_age_months, 0)
  late  <- pmin(pmax(claim_age_months - nra_months, 0), 70 * 12 - nra_months)
  100 * (early_reduction_factor(early, policy) + late * drc_monthly) * (early == 0) +
    100 * early_reduction_factor(early, policy) * (early > 0)
}

params_by_cohort <- tibble(
  elig_year   = policy_years,
  birth_year  = policy_years - 62,
  nra_months  = policy$nra,
  drc_annual  = 100 * 12 * policy$drc_rate_by_elig   # % per year of delay
) |>
  mutate(pct_pia_62 = pct_of_pia(62 * 12, nra_months, drc_annual / 1200),
         pct_pia_65 = pct_of_pia(65 * 12, nra_months, drc_annual / 1200),
         pct_pia_66 = pct_of_pia(66 * 12, nra_months, drc_annual / 1200),
         pct_pia_67 = pct_of_pia(67 * 12, nra_months, drc_annual / 1200),
         pct_pia_70 = pct_of_pia(70 * 12, nra_months, drc_annual / 1200))

# ---- Step 3: check against Table V.C1 (COLA, AWI, taxable maximum) ---------
# V.C1 covers 1975-2035. AWI and the taxable maximum should match exactly;
# the COLA to the published 0.1 percent.

vc1 <- read_tr_block("V_C", "V.C1", year_col = 2,
                     cols = c(cola = 3, awi = 4, taxmax = 6))

chk1 <- inner_join(params_by_year, vc1, by = "year", suffix = c("", "_tr")) |>
  mutate(gap_awi = awi - awi_tr, gap_cola = cola - cola_tr, gap_taxmax = taxmax - taxmax_tr)

# Known difference: the 1999 COLA was announced as 2.4% and later corrected
# to 2.5% by legislation after a CPI error (V.C1 footnote f). ranypia carries
# 2.4; the TR shows 2.5. This would understate PIAs of workers eligible before
# 1999 by about 0.1%. Flagged for a fix in ranypia; tolerated here.
known_cola <- chk1$year == 1999

cat("V.C1, years", min(chk1$year), "-", max(chk1$year), "\n")
cat("  largest gap: AWI $", round(max(abs(chk1$gap_awi)), 2),
    "| COLA", round(max(abs(chk1$gap_cola[!known_cola])), 2), "pts (excluding 1999)",
    "| taxable max $", max(abs(chk1$gap_taxmax)), "\n")
cat("  1999 COLA: ranypia", chk1$cola[known_cola], "vs TR", chk1$cola_tr[known_cola], "(known)\n")
stopifnot(max(abs(chk1$gap_awi)) < 0.01,
          max(abs(chk1$gap_cola[!known_cola])) < 0.05,
          max(abs(chk1$gap_taxmax)) == 0)

# ---- Step 4: check against Table V.C2 (bend points, QC, old-law base) ------
# V.C2 covers 1979-2035 (1978 has no bend points). All are whole dollars and
# should match exactly.

vc2 <- read_tr_block("V_C", "V.C2", year_col = 2,
                     cols = c(pia_bp1 = 3, pia_bp2 = 4,
                              mfb_bp1 = 6, mfb_bp2 = 7, mfb_bp3 = 8,
                              qc_amount = 9, base77 = 10)) |>
  filter(year >= 1979)

chk2 <- inner_join(params_by_year, vc2, by = "year", suffix = c("", "_tr"))
gaps2 <- sapply(c("pia_bp1", "pia_bp2", "mfb_bp1", "mfb_bp2", "mfb_bp3", "qc_amount", "base77"),
                function(v) max(abs(chk2[[v]] - chk2[[paste0(v, "_tr")]])))
cat("V.C2, years", min(chk2$year), "-", max(chk2$year), "| largest gap by parameter ($):\n")
print(gaps2)
stopifnot(all(gaps2 == 0))

# ---- Step 5: check against Table V.C3 (NRA, DRC, % of PIA by claim age) ----
# V.C3 lists birth years 1924-1942 and 1955-1959 individually and groups
# 1943-54 and "1960 & later". We expand the groups and compare.

vc3_raw <- readxl::read_excel("data-raw/tr2026/TRTables_TR2026.xlsx", sheet = "V_C",
                              col_names = FALSE, col_types = "text", .name_repair = "minimal")
first <- grep("^Table V\\.C3", vc3_raw[[1]])
vc3_raw <- vc3_raw[first:(first + 40), 1:9]
names(vc3_raw) <- c("birth", "elig", "nra", "drc_annual", "pct_pia_62",
                    "pct_pia_65", "pct_pia_66", "pct_pia_67", "pct_pia_70")
vc3 <- vc3_raw |>
  filter(grepl("^[0-9]{4}", birth)) |>
  mutate(from = as.integer(substr(birth, 1, 4)),
         # "1943-54" -> 1954; "1960 & later" -> 1980 (enough to cover any
         # cohort reaching 62 by 2042); single years -> themselves
         to = ifelse(grepl("-", birth), 1900L + suppressWarnings(as.integer(sub(".*-", "", birth))),
                     ifelse(grepl("later", birth), 1980L, from)),
         nra_months = as.integer(substr(nra, 1, 2)) * 12 +
           ifelse(grepl("mo", nra), as.integer(sub(".*, ([0-9]+) mo", "\\1", nra)), 0L)) |>
  mutate(across(c(drc_annual, starts_with("pct_pia")), as.numeric)) |>
  rowwise() |> mutate(birth_year = list(from:to)) |> ungroup() |>
  unnest(birth_year) |>
  select(birth_year, nra_months, drc_annual, starts_with("pct_pia"))

chk3 <- inner_join(params_by_cohort, vc3, by = "birth_year", suffix = c("", "_tr"))
gaps3 <- sapply(c("nra_months", "drc_annual", "pct_pia_62", "pct_pia_65",
                  "pct_pia_66", "pct_pia_67", "pct_pia_70"),
                function(v) max(abs(chk3[[v]] - chk3[[paste0(v, "_tr")]])))
cat("V.C3, birth years", min(chk3$birth_year), "-", max(chk3$birth_year),
    "| largest gap (months, % per year, % of PIA):\n")
print(round(gaps3, 4))
stopifnot(all(gaps3 < 1e-6))

# ---- Save ------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
saveRDS(params_by_year, "data/params_by_year.rds")
saveRDS(params_by_cohort, "data/params_by_cohort.rds")
cat("\nSaved data/params_by_year.rds (", nrow(params_by_year), "years) and",
    "data/params_by_cohort.rds (", nrow(params_by_cohort), "cohorts)\n")
