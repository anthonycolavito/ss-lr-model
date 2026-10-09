# Raw data sources

Every file in `data-raw/` is listed here with where it came from and when. Files are never edited after download; to update one, replace it and add a new row.

| File | Source | Retrieved | Notes |
| --- | --- | --- | --- |
| `tr2026/SingleYearTRTables_TR2026.xlsx` | [2026 TR single-year tables](https://www.ssa.gov/oact/TR/2026/lrIndex.html) | 2026-10-08 | 20 sheets: IV.B1–B5, V.A1–A5, V.B1–B2, V.C4, V.C5, V.C7, VI.G1–G5 |
| `tr2026/TR2026_Econ_LR_Assumption_Values.xlsx` | [2026 TR](https://www.ssa.gov/oact/TR/2026/TR2026_Econ_LR_Assumption_Values.xlsx) | 2026-10-08 | Compound growth rates over intervals (productivity, CPI, GDP deflator, hours, unemployment, real interest); cross-check only |
| `population/SSPopDec_Alt2_TR2026.csv` | OCACT historical and projected population, 2026 TR intermediate | 2026-10-08 | Dec 31, 1940–2100, ages 0–100 (100 = 100+); columns Total, then M/F × Tot, Sin, Mar, Wid, Div |
| `population/SSPopJul_Alt2_TR2026.csv` | OCACT historical and projected population, 2026 TR intermediate | 2026-10-08 | July 1, 1941–2100; same layout as the December file. Matches TR Table V.A3 to rounding |
| `mortality/DeathProbsE_M_Alt2_TR2026.csv` | OCACT death probabilities, 2026 TR intermediate | 2026-10-08 | Males, 2024–2100, ages 0–119; one title line above the header |
| `mortality/DeathProbsE_F_Alt2_TR2026.csv` | OCACT death probabilities, 2026 TR intermediate | 2026-10-08 | Females, same layout |
| `mortality/DeathProbsE_{M,F}_Hist_TR2026.csv` | OCACT historical death probabilities, 2026 TR | 2026-10-08 | 1900–2023, ages 0–119; joins the projected files at 2024 with no gap |
| `mortality/life_tables/CohLifeTables_{M,F}_Alt2_TR2026.csv` | OCACT cohort life tables, 2026 TR intermediate | 2026-10-08 | Birth cohorts 1900–2100: q, l, d, L, T, e by age, plus actuarial functions at 2.3% interest (D, M, N, annuity factors a and 12a). Header on line 5; data from line 7 |
| `mortality/life_tables/PerLifeTables_{M,F}_Hist_TR2026.csv` | OCACT historical period life tables, 2026 TR | 2026-10-08 | Calendar years 1900–2023, same columns as the cohort tables |
| `tr2026/TRTables_TR2026.xlsx` | [2026 TR tables](https://www.ssa.gov/oact/TR/2026/TRTables_TR2026.xlsx) | 2026-10-08 | Every TR table, 17 sheets. Adds V.C1 (COLA, AWI, taxable max, 1975–2035), V.C2 (wage-indexed parameters), V.C3 (NRA and DRC schedule), III.A5 (2025 benefits by beneficiary type), VI.H (scheduled benefits) |
| `supplement/supplement25_all.xlsx` | [Annual Statistical Supplement, 2025](https://www.ssa.gov/policy/docs/statcomps/supplement/2025/index.html) | 2026-10-08 | All 175 tables, one sheet each, named by table number. Beneficiary data are December 2024 |
| `supplement/2026/*.xlsx` | [Annual Statistical Supplement, 2026](https://www.ssa.gov/policy/docs/statcomps/supplement/2026/index.html) (partial release) | 2026-10-08 | One workbook per section: 5.A–5.H, 5.J, 5.L, 5.M, 6.A–6.F. Beneficiary data are December 2025. Sections 2.A, 4.B, 4.C and a few 5.A tables not yet released; use the 2025 edition for those |
| `oact_wages/avg_and_median_wages_2023.xlsx` | [OCACT wage statistics: central tendency](https://www.ssa.gov/OACT/COLA/central.html) | 2026-10-08 | Average and median net compensation (W-2 wages) and their ratio, 1991–2023. 2010 and 2019 definition changes flagged with "b" |
| `oact_wages/wage_earner_distribution_{2000,2007,2019,2023}.xlsx` | [OCACT wage statistics: distribution by net compensation](https://www.ssa.gov/cgi-bin/netcomp.cgi?year=2023) | 2026-10-08 | Wage earners in 59 brackets from under $5,000 to $50 million+, with counts and aggregate amounts. 2000, 2007, 2019 are business-cycle peaks. Each year's implied average matches the central-tendency file to the cent |

## Still to add

- `supplement/2026/`: sections 2.A, 4.B and 4.C once released
- `as121/`: tables extracted from Actuarial Study No. 121

## Program parameters

Historical and projected program rules (AWI, COLAs, taxable maximum, bend points, QC amounts, NRA, reduction and delayed-credit factors) come from the ranypia package's `current_law()` policy object, built from the 2026 TR. They are not duplicated in `params/`.

## Definitions

**Net compensation** (OCACT wage statistics): compensation subject to federal income tax as reported on Forms W-2, plus contributions to deferred compensation plans, minus deferred-compensation distributions already included in taxable compensation. It is the basis of the AWI. It covers all W-2 wage earners, including those in jobs not covered by Social Security, and excludes self-employment income. For 2023: $11.10 trillion across 173,670,935 wage earners, an average of $63,932.64.

- `docs/reference/AN2026-6_death_disability_probabilities.pdf` — Actuarial Note 2026.6 (July 2026), Disability and Death Probability Tables for Insured Workers Who Attain Age 20 in 2026. Tables C–D: survival and disability status by single age, 20–67, 2026 TR intermediate. Supplied by user, 2026-10-08.
- `data-raw/oact_insured/di_ins_hist.xlsx` — OCACT, estimated number of workers insured in the event of disability, by sex and age group (under 20, 5-year groups to 65–69), 1970–2026, thousands. Matches Supplement 4.C2 through about 2020; revised for 2021–2025 and adds 2026. Supplied by user, 2026-10-08.
- `data-raw/supplement/5d1_vintages/` — Supplement Table 5.D1 (disabled workers by year of entitlement and sex) from the 2012 (PDF, December 2011), 2013, 2014, 2015 and 2016 (xlsx, December 2012–2015) editions. Used for IBNR factors. Supplied by user, 2026-10-08.
