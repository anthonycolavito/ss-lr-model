# ss-lr-model

A long-range Social Security (OASDI) projection model in R, built to follow the Office of the Chief Actuary's methodology for the 2026 Trustees Report and to score reform proposals against its own baseline.

## Approach

- **Follow OCACT's structure.** The method comes from *Long-Range OASDI Projection Methodology* (2026 TR) and, for the first ten years, Actuarial Study No. 121. Both are in `docs/reference/`.
- **Take published projections as inputs where OCACT publishes them.** This covers population by age, sex and marital status, death probabilities, and the economic assumptions.
- **Build the beneficiary and benefit-level machinery ourselves**, and project every year from 2026 on.
- **Calibrate to the Trustees Report.** We compare against beneficiary counts by type, cost, and income and cost rates.
- **Score reforms against our own baseline.** If our baseline lands far from the TR, we fall back to applying our reform-to-baseline ratio to the TR baseline.

## Layout

| Folder | What goes there |
| --- | --- |
| `data-raw/` | Source files exactly as downloaded. Never edited by hand. Each source is logged in `data-raw/SOURCES.md`. |
| `data/` | Clean tables produced from `data-raw/` by scripts. |
| `params/` | Program rules as small CSVs: bend points, QC amounts, NRA schedule, reduction factors. |
| `R/` | Reusable functions. |
| `scripts/` | The pipeline, numbered in run order (`01_...R`, `02_...R`). |
| `outputs/` | Projection results and comparisons against the TR. |
| `tests/` | Checks that results still match published numbers. |
| `docs/reference/` | Methodology documents. |
| `docs/DECISIONS.md` | Every modeling choice, with the reason, the alternatives, and its status. Read this before changing a method. |

The one rule: anything in `data/` or `outputs/` can be rebuilt from `data-raw/` and `params/` by running `scripts/` in order.

## Status

Phases 0–3 are done: inputs, insured status, disabled workers and their dependents, and every OASI beneficiary category, 2025–2100. Phase 4 (new-award benefit levels) is built through award PIAs and MBAs (scripts 17–20); careers are moved to future cohorts' earnings levels and covered-worker rates (scripts 20–21); OCACT's dispersion adjustment is still open (PE-05). Scripts 17–19 and 21–22 need the BEPUF 2020 files in `data-raw/bepuf/` (not in git; see `data-raw/SOURCES.md`).

Rebuild everything with `Rscript scripts/run_all.R` (about 25 minutes on 2 cores; logs in `outputs/logs/`; `Rscript scripts/run_all.R 11` resumes from step 11). The scripts, in order:

| Script | Builds | Checked against |
| --- | --- | --- |
| `01_import_population.R` | Population by year, age, sex, marital status (Dec 31 and Jul 1) | TR V.A3, within rounding |
| `02_import_mortality.R` | Death probabilities 1900–2100 | TR V.A4 life expectancy, within 0.06 years |
| `03_program_parameters.R` | Program rules by year and birth cohort, from ranypia | TR V.C1–V.C3, exact except the 1999 COLA |
| `04_insured_inputs.R` | Covered-worker rates (Study 127 age paths after 2023), median earnings, QC amounts, earnings distribution, by age, sex, year | Total covered workers = TR IV.B4; men-women change in age-adjusted rates = TR |
| `05_net_immigration.R` | Net immigration by age 1–99, sex, year (births excluded); LPR entrants; temporary or unlawfully present population by age | Totals within 1–2% of TR V.A2; population hits the Trustees' 2025 and 2100 totals, 2029 within 0.1 million |
| `06_insured_simulation.R` | Fully and disability insured rates by age, sex, year, 1970–2100, OCACT's SLCT/SRCH method with immigrants (uses all cores; about 30 minutes on 2) | Supplement 4.C2 history; TR fully insured at age 62 in 2025 and 2100 |
| `07_insured_calibrate.R` | Scales 06's rates to Supplement 4.C2 (2013–2022) and the TR's 2025 and 2100 figures; adds back disabled workers on the rolls 4+ years (DINADD); reports every factor. Needs 08–10: on a fresh build 07 first runs without DINADD, then 09 → 10 → 07 → 09 → 10 (`run_all.R` does this) | TR fully insured at 62 and disability insured at 50, 2025 and 2100, exactly |
| `08_di_inputs.R` | Disabled-worker inputs: Study 130 death and recovery tables and 2001–24 history, Actuarial Note 2026.6, Supplement 2026 stock and awards, TR V.C5 | Study 130 worked example; note's Table A probabilities; December 2025 stock vs V.C5 |
| `09_di_start_stock.R` | Disabled workers at December 2025 by sex, entitlement age and duration: prior from past awards and Study 130 survival, raked to Supplement 5.A1.2 and 5.D1; IBNR factors from two 5.D1 vintages | Both margins matched exactly; survival model reproduces mature cohorts within 1–2% |
| `10_di_projection.R` | Disabled workers 2026–2100 by sex, entitlement age, duration and age; entitled and current pay (IBNR); incidence, deaths, recoveries, conversions | TR V.C5 2026–2035 exactly, 2036–2100 within ±2%; memo death (26.3 → 12.5) and recovery (18.7 → 11.1) rates; V.C5 gross prevalence |
| `11_di_auxiliaries.R` | Dependents of disabled workers by category (minor, student, disabled adult child; young, aged, divorced spouse), 2001–2100: TR V.C5 totals split by OCACT-structured linkages | December 2025 Supplement counts by category; V.C5 totals |
| `12_aged_widows.R` | Aged widow(er)s (equation 3.3.1) by age, sex and marital status, insured and uninsured, plus disabled widow(er)s; levels from TR V.C4 | Supplement 5.A1.6/5.A1.7 history 2012–2025; model alone vs V.C4 (F-15) |
| `13_retired_workers.R` | Retired workers 2007–2100 by age and sex (equation 3.3.2): prevalence from nineteen Supplement editions, age-62 regression, MBA/PIA-based 63–69 with the age-66 NRA adjustment, converted DI added back; widow(er)s from 12 | TR V.C4 retired workers: 2026–2035 matched, 2036–2099 within −1.0% to +2.4%; entitlements at 70 vs 6.B5.1 |
| `14_rw_entitlement_age.R` | Retired workers by attained age × age at entitlement (and converted DI), 2025–2100 | December 2025 total = Supplement 5.A1.1; 2026 entitlements vs 2025 actuals (6.B5.1) |
| `15_oasi_auxiliaries.R` | Dependents of retired and deceased workers by category, 2025–2100: TR V.C4 totals split by OCACT-structured linkages | December 2025 Supplement counts by category; model alone vs V.C4 (F-16) |
| `16_compare_tr.R` | Every projection against the 2026 TR (intermediate, low-cost, high-cost) and Supplement 4.C2, tagged input / fitted / set equal / tested: `outputs/tr_comparison.csv` | Shown on the comparison dashboard |
| `17_bepuf_aime.R` | AIMEs of 587,883 recent new worker beneficiaries from BEPUF 2020 earnings (ranypia); needs the BEPUF files in `data-raw/bepuf/` (not in git) | 2025 award distributions by PIA (Supplement 6.B4, 6.C1) |
| `18_paps.R` | PAPs by sex and age at entitlement (retired 62–70, disabled 24–66) from BEPUF careers raked to 2025 awards; conversions separated | 2025 award distributions (fitted); average award benefits by age, 6.A4 (not fitted; F-22) |
| `19_paps_shuttle.R` | Retired-worker PAPs by year 2025–2100, sex and age 62–70, with OCACT's shuttling; 2025 base raked to 6.B4/6.A4 | 2025 average award benefits by age (fitted), PIA distribution within 1–2 points |
| `20_ate_by_age.R` | Average taxable earnings by age and sex relative to the AWI, 1951–2100 (Supplement 4.B13 2012–2023, 4.B2/4.B6 before, held after) | 4.B13 totals |
| `21_paps_projection.R` | PAPs moved to each year's cohort: earnings levels by age and sex and covered-worker rates (OCACT 4.2.1) | — (F-23) |
| `22_award_levels.R` | Average award PIA and MBA by year, sex and age at entitlement, retired and disabled workers | 2025 averages vs 6.A4 |
| `23_lump_sum.R` | Lump-sum death payments, 2024–2100 (equation 3.3.13) | Supplement 6.D9 (2024 fitted, 2025 +7.6%) |
| `24_post_entitlement.R` | Post-entitlement factors by sex and duration (retired, disabled, conversions), initial to ultimate 2026–2045 | Supplement 5.B4 and 5.D1 by edition, OCACT's window 2014–15 to 2023–24 |
| `25_di_benefits.R` | Disabled-worker benefits in current pay by cohort, December 2025–2100; conversion benefits | Supplement 5.D1 and 5.A1.2 (December 2025, exact); DI cost-rate trend vs IV.B1 +5% by 2035 (F-27) |
| `26_rw_benefits.R` | Retired-worker benefits and PIAs in current pay (age × age at entitlement, conversions), December 2025–2100, net of the dual-entitlement excess | Supplement 5.A1.1 and 5.A3a by age (December 2025, exact with the excess); average PIA vs 5.B7 within 0.9% (check); 5.B4 by entitlement year within −4.4% to +3.6% (check) |
| `27_aux_benefits.R` | Dependents' and survivors' average benefits (linkage × account holder's average PIA or DI benefit), 30 categories | Supplement 5.A1, 5.A1.3, 5.A1.5–5.A1.7 (December 2025, by construction) |
| `28_dual_entitlement.R` | Dually entitled counts and excess amounts (OCACT's regressions) | Supplement 5.G2, 5.G3, 5.A14, 5.A15 (2025, by construction) |
| `29_annual_benefits.R` | Annual scheduled benefits by trust fund, 2026–2100 | TR IV.A1/IV.A2 to 2035 and IV.B1 cost rates after: OASDI benefit rate within ±1.6% to 2090, +2.5% in 2100 (F-31) |

## Setup

R 4.x with `dplyr`, `tidyr`, `readr`, `readxl`, `Rcpp` (with a C++ compiler, for `src/insured_select.cpp`) and `ranypia` (`remotes::install_github("anthonycolavito/ranypia")`), plus `pdftotext` from poppler-utils (reads the PDF tables).

## Planning

The full checklist, the gaps-and-substitutes table and the parameters lifted from the methodology document live in the build guide (a Claude doc): https://claude.ai/code/artifact/c2f3da56-f2c1-4309-b785-0651b799e80a. `docs/DECISIONS.md` is the record of every choice; the guide tracks the plan.
