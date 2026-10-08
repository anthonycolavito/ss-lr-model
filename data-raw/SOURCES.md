# Raw data sources

Every file in `data-raw/` is listed here with where it came from and when. Files are never edited after download; to update one, replace it and add a new row.

| File | Source | Retrieved | Notes |
| --- | --- | --- | --- |
| `tr2026/SingleYearTRTables_TR2026.xlsx` | [2026 TR single-year tables](https://www.ssa.gov/oact/TR/2026/lrIndex.html) | 2026-10-08 | 20 sheets: IV.B1–B5, V.A1–A5, V.B1–B2, V.C4, V.C5, V.C7, VI.G1–G5 |
| `tr2026/TR2026_Econ_LR_Assumption_Values.xlsx` | [2026 TR](https://www.ssa.gov/oact/TR/2026/TR2026_Econ_LR_Assumption_Values.xlsx) | 2026-10-08 | Compound growth rates over intervals (productivity, CPI, GDP deflator, hours, unemployment, real interest); cross-check only |
| `population/SSPopDec_Alt2_TR2026.csv` | OCACT historical and projected population, 2026 TR intermediate | 2026-10-08 | Dec 31, 1940–2100, ages 0–100 (100 = 100+); columns Total, then M/F × Tot, Sin, Mar, Wid, Div |
| `mortality/DeathProbsE_M_Alt2_TR2026.csv` | OCACT death probabilities, 2026 TR intermediate | 2026-10-08 | Males, 2024–2100, ages 0–119; one title line above the header |
| `mortality/DeathProbsE_F_Alt2_TR2026.csv` | OCACT death probabilities, 2026 TR intermediate | 2026-10-08 | Females, same layout |
| `tr2026/TRTables_TR2026.xlsx` | [2026 TR tables](https://www.ssa.gov/oact/TR/2026/TRTables_TR2026.xlsx) | 2026-10-08 | Every TR table, 17 sheets. Adds V.C1 (COLA, AWI, taxable max, 1975–2035), V.C2 (wage-indexed parameters), V.C3 (NRA and DRC schedule), III.A5 (2025 benefits by beneficiary type), VI.H (scheduled benefits) |
| `supplement/supplement25_all.xlsx` | [Annual Statistical Supplement, 2025](https://www.ssa.gov/policy/docs/statcomps/supplement/2025/index.html) | 2026-10-08 | All 175 tables, one sheet each, named by table number. Beneficiary data are December 2024 |
| `supplement/2026/*.xlsx` | [Annual Statistical Supplement, 2026](https://www.ssa.gov/policy/docs/statcomps/supplement/2026/index.html) (partial release) | 2026-10-08 | One workbook per section: 5.A–5.H, 5.J, 5.L, 5.M, 6.A–6.F. Beneficiary data are December 2025. Sections 2.A, 4.B, 4.C and a few 5.A tables not yet released; use the 2025 edition for those |
| `oact_wages/avg_and_median_wages_2023.xlsx` | [OCACT wage statistics: central tendency](https://www.ssa.gov/OACT/COLA/central.html) | 2026-10-08 | Average and median net compensation (W-2 wages) and their ratio, 1991–2023. 2010 and 2019 definition changes flagged with "b" |

## Still to add

- `oact_wages/`: net compensation distribution by bracket ([netcomp](https://www.ssa.gov/cgi-bin/netcomp.cgi?year=2023)), latest year plus one earlier year
- `mortality/`: historical death probabilities before 2024, if a phase needs them
- `supplement/2026/`: sections 2.A, 4.B and 4.C once released
- `as121/`: tables extracted from Actuarial Study No. 121

## Program parameters

Historical and projected program rules (AWI, COLAs, taxable maximum, bend points, QC amounts, NRA, reduction and delayed-credit factors) come from the ranypia package's `current_law()` policy object, built from the 2026 TR. They are not duplicated in `params/`.
