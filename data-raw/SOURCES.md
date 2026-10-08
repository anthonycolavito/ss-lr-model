# Raw data sources

Every file in `data-raw/` is listed here with where it came from and when. Files are never edited after download; to update one, replace it and add a new row.

| File | Source | Retrieved | Notes |
| --- | --- | --- | --- |
| `tr2026/SingleYearTRTables_TR2026.xlsx` | [2026 TR single-year tables](https://www.ssa.gov/oact/TR/2026/lrIndex.html) | 2026-10-08 | 20 sheets: IV.B1–B5, V.A1–A5, V.B1–B2, V.C4, V.C5, V.C7, VI.G1–G5 |
| `tr2026/TR2026_Econ_LR_Assumption_Values.xlsx` | [2026 TR](https://www.ssa.gov/oact/TR/2026/TR2026_Econ_LR_Assumption_Values.xlsx) | 2026-10-08 | Compound growth rates over intervals (productivity, CPI, GDP deflator, hours, unemployment, real interest); cross-check only |
| `population/SSPopDec_Alt2_TR2026.csv` | OCACT historical and projected population, 2026 TR intermediate | 2026-10-08 | Dec 31, 1940–2100, ages 0–100 (100 = 100+); columns Total, then M/F × Tot, Sin, Mar, Wid, Div |
| `mortality/DeathProbsE_M_Alt2_TR2026.csv` | OCACT death probabilities, 2026 TR intermediate | 2026-10-08 | Males, 2024–2100, ages 0–119; one title line above the header |
| `mortality/DeathProbsE_F_Alt2_TR2026.csv` | OCACT death probabilities, 2026 TR intermediate | 2026-10-08 | Females, same layout |

## Still to add

- `mortality/`: historical death probabilities before 2024, if a phase needs them
- `supplement/`: Annual Statistical Supplement tables 4.B, 4.C, 5.A/5.B, 6.B, 6.C
- `as121/`: tables extracted from Actuarial Study No. 121
