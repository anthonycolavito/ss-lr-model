# ss-lr-model: notes for a new session

A long-range OASDI projection model in R that follows OCACT's methodology for the
2026 Trustees Report, matches its published targets, and will score reforms
(NRA increase, PIA factor changes, later revenue options) against its own
baseline. Read `README.md` (layout, status, run order) and `docs/DECISIONS.md`
(every choice and finding) before changing anything.

## How Anthony wants to work

- Follow OCACT's methodology as closely as possible and match its targets. The
  methodology document and the studies are in `docs/reference/`.
  "Methodology" means *Long-Range OASDI Projection Methodology, 2026 TR*.
- Walk through the build step by step; explain choices plainly, and scrutinize
  them. When a choice is genuinely his (or costly to undo), lay out the options
  with a recommendation and let him decide. Make data-source judgments yourself.
- Before calibrating anything, check the TR, the 2026 assumptions memos and the
  actuarial studies/notes for published values to calibrate against.
- Take published projections as inputs (population, mortality, economics). When
  the model alone misses a published beneficiary total, use the published level
  and let the model split it and give the reform response (DECISIONS.md P-02,
  DA-06, AW-06, OA-05); always report the model-alone gap as a finding.
- Log every choice in `docs/DECISIONS.md` (ID, choice, reason, alternatives,
  status) and every surprise as a finding (F-xx). Keep the README status table
  and the build guide in step.
- Long runs: use all cores (`parallel::detectCores()`), run detached
  (`setsid nohup Rscript ... &`) so an interrupt doesn't kill them, and post a
  short progress note every 5 minutes. Don't redo calculations that haven't
  changed, but leave the core scripts able to rerun everything.
- Commit and push to `main` as each step is done. Commits are authored with
  `git -c user.name="Anthony Colavito" -c user.email="colavitoanthony@gmail.com" commit ...`.

## Build guide

The plan lives in a Claude doc (the "build guide"):
https://claude.ai/code/artifact/c2f3da56-f2c1-4309-b785-0651b799e80a
It has the phase checklists (tick items as they're done, with a one-line note on
the script and result), a gaps-and-substitutes table, the parameters lifted from
the methodology document, the annualizing formula, and the calibration targets.

## Getting started in a fresh container

`data/` and `outputs/*.rds` aren't committed (P-07). Rebuild first:

```
setsid nohup Rscript scripts/run_all.R > run_all.log 2>&1 &
```

About 25 minutes on 2 cores (06 is 21 of them); per-step logs go to
`outputs/logs/`. Verified from a fresh clone on October 9, 2026: every step ran,
and every published target was hit exactly (insured at 62 and 50, V.C5 disabled
workers 2026–2035, V.C4 retired workers 2026–2035, all V.C4/V.C5 levels). Setup needs `ranypia` (`remotes::install_github("anthonycolavito/ranypia")`),
`Rcpp` with a compiler, `dplyr`, `tidyr`, `readr`, `readxl`, and `pdftotext`.
The insured simulation is random (L'Ecuyer streams over all cores), so a rebuild
on a different core count differs slightly from the figures in DECISIONS.md;
everything calibrated to a published target still hits it exactly. A rebuild moved the model-only shares of OASI dependents by
up to 1.5%.

## Where things stand (October 8, 2026)

Phases 0–3 are done (scripts 01–15). Every beneficiary category is projected
2025–2100 and matches TR V.C4 and V.C5: disabled workers 2026–2035 exactly and
within ±2% after; retired workers out of sample within −1.0% to +2.4%
(short-range factor on ages 65+, RW-11);
widow(er)s and all dependents at V.C4/V.C5 levels, split by OCACT-structured
linkages (model-alone gaps in F-13, F-15, F-16).

Outputs Phase 4 builds on:

| File | What's in it |
| --- | --- |
| `data/rw_entitlement_age.rds` | `rw_ae`: retired workers by year, sex, attained age, age at entitlement (62–70) and class (`retired`, or `converted` DI at NRA); `entitlements`: new entitlements by year, sex, age |
| `data/retired_workers.rds` | prevalence, exposure, converted DI stock, short-range factors |
| `data/di_projection.rds` | DI flows by year; stock by sex and age (`n`, current pay `cp`, on rolls 4+ years `n_d4`); deaths and recoveries by age; conversions by age; the full sex × entitlement age × duration × age state in 2100 only |
| `data/di_stock_2025.rds` | DI stock December 2025 by sex, entitlement age, entitlement year, duration, attained age |
| `data/aged_widows.rds`, `data/di_auxiliaries.rds`, `data/oasi_auxiliaries.rds` | widow(er)s and dependents by category, with model shares and published levels |
| `data/params_by_year.rds`, `data/params_by_cohort.rds` | AWI, COLA, taxable max, bend points, QC; NRA, DRC and reduction by cohort (from ranypia) |

## Comparison with OCACT

`scripts/16_compare_tr.R` sets every projection against the TR and Supplement
4.C2 (`outputs/tr_comparison.csv`); the dashboard "Model vs Trustees 2026"
(https://claude.ai/artifact/LGUWYB7UszUQkyxoYDR36K) shows it. Rerun 16 and
refresh that dashboard's two data files after changes. Open findings from it:
F-20 (insured rates above OCACT's 2023–2025 estimates at 25–54; a fix was
tested and rejected, C-08) and the
model-alone dependent gaps (F-13, F-15, F-16).

## Phase 4, new-award benefit levels (methodology 4.2): status

Built (scripts 17-22; DECISIONS.md PB-, PP-, PS-, PE-, AL-):

- Earnings histories: BEPUF 2020 (synthetic), worker beneficiaries entitled
  2016-2020 (Anthony's choice, PB-01). The two files are too big for git; a new
  container needs them re-sent into `data-raw/bepuf/` (`data-raw/SOURCES.md`;
  `data-raw/bepuf/extract_bepuf_earnings.R` rebuilds the earnings extract from
  SSA's zip). `run_all.R` skips 17-19 and 21-22 without them. Never use BEPUF's AIME/PIA
  columns (withdrawn by SSA; Anthony found the error).
- AIMEs with ranypia (17); conversions separated from FRA claims and DI PAPs (18);
  retired PAPs with OCACT's shuttling, the 2025 base calibrated to Supplement
  6.B4 and 6.A4 (19); award PIA and MBA by year, sex and age (20).
- ranypia has no earnings test or totalization; checked that neither matters
  for award levels (PB-05). Revisit the earnings test for NRA reforms.

- Careers moved to each year's cohort (20-21): average taxable earnings by age
  and sex from Supplement 4.B13 (2012-2023) and covered-worker rates (PE-01 to
  PE-04); award PIA/MBA by year (22). Men's award PIAs relative to the AWI fall
  about 7%, women's rise 1-2% (F-23).

- Lump-sum death payments (23).

Open in Phase 4: OCACT's dispersion adjustment and two smaller rules (PE-05).

Still open:

- F-16: the spouse claiming-age response is weakly tested; revisit for NRA
  reforms once benefit levels exist.
- D-04: ranypia's 1999 COLA is 2.4% (TR 2.5%); flagged for a ranypia fix.

Phase 5 started: post-entitlement factors (24; DECISIONS.md PF-01 to PF-04,
F-25, F-26). Retired from Supplement 5.B4 editions 2014-2026, DI from 5.D1 editions
2012-2026, both over OCACT's window 2014-15 to 2023-24. DI benefits in current
pay (25; DB-01 to DB-06): carried by cohort, started from 5.D1 and 5.A1.2, no
separate workers' compensation offset (DB-04). Open: F-27 (DI cost trend +5%
vs the TR by 2035; check once dependents and annualizing are in). Retired-worker
benefits (26; RB-01 to RB-06): OCACT's age x entitlement-age matrix, started from
5.A3a and 5.A1.1 by age and reduction status, reduced shares fitted (RB-06, F-28);
5.B4 as a check. EA-03/AL-03 handled (RB-02, RB-03).
The Supplement's retired and award averages include the dual-entitlement excess
(R/dual_excess.R, DX-01 to DX-03): it is taken out of the worker matrices, the
award targets (19) and the women's post-entitlement factors (24), and projected
with OCACT's regressions (28; DX-04 to DX-06). Dependents and survivors (27;
AX-01 to AX-03), annual benefits by fund (29; AB-01 to AB-03): OASDI benefits /
payroll within ±1.3% of the TR through 2080, +3.4% in 2100 (F-31). Open: DI
after 2060 (+5% to +8%), retroactive payments as a 2024-fitted loading (AB-02).

After Phase 4: Phase 5 (benefits in current pay: starting PIA/MBA matrices,
roll-forward with COLAs and post-entitlement factors, workers' compensation
offset, auxiliary averages, dual entitlement, annualizing), Phase 6 (trust fund
operations and summary measures, checked against IV.B1), Phase 7 (reforms).
