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

## Next: Phase 4, new-award benefit levels (methodology 4.2)

Build guide checklist:

1. Define the PAP interface: 30 AIME sub-intervals in 1979 dollars (four of $45
   below the first bend point of $180; fourteen between $180 and $1,085, nine of
   $45 and five of $100; twelve above, ten of $200 and two of $1,000). The
   retired-worker passage of the methodology says eighteen intervals between the
   bend points, which overshoots $1,085; use the DI passage's layout for both.
2. Generate PAPs (the share of new awards in each AIME interval) by age at
   entitlement, sex and year, from ranypia run on an earnings-history source.
3. Average award PIA = sum of PIA factor × interval length × PAP, indexed by AWI
   and COLAs (4.3.c).
4. Award MBA = PIA × reduction or delayed retirement credit for age at
   entitlement.

Open decision to settle first, with Anthony: **the earnings-history source for
the PAPs** (the one gap that drives reform accuracy, since PIA factor reforms
reweight the PAP bins). He has been weighing the earnings portion of SSA's
Benefits and Earnings Public-Use File 2020 (BEPUF, synthetic) for his separate
microsimulation work; the same choice should serve both. Present the options
(BEPUF; synthetic histories from Supplement 4.B earnings by age and sex; others)
with what each can and can't match, then let him decide. Calibration check for
the levels: Supplement 6.B average PIA and MBA of new awards by age and sex,
and later the TR's implied average benefits (cost ÷ beneficiaries).

Carry into Phase 4 and 5:

- EA-03: scripts/14's ages at entitlement are December ages, about half a year
  later than the exact ages in Supplement 6.B5.1. Compute reductions and credits
  at December age − ½ year.
- DI new-award PIAs: −0.93% adjudication-level adjustment (TF Ops p. 53).
- scripts/10 doesn't yet save new DI entitlements by age at entitlement and sex
  for each year, or the full entitlement age × duration state by year; Phase 4
  (DI award PIAs) and Phase 5 (DI benefits by duration, workers' compensation
  offset) need them. Add them to its saved output.
- Lump-sum death payments ($255) were moved here from Phase 3: count of deaths
  of insured workers with an eligible survivor × $255.
- F-16: the spouse claiming-age response is weakly tested; revisit for NRA
  reforms once benefit levels exist.
- D-04: ranypia's 1999 COLA is 2.4% (TR 2.5%); flagged for a ranypia fix.

After Phase 4: Phase 5 (benefits in current pay: starting PIA/MBA matrices,
roll-forward with COLAs and post-entitlement factors, workers' compensation
offset, auxiliary averages, dual entitlement, annualizing), Phase 6 (trust fund
operations and summary measures, checked against IV.B1), Phase 7 (reforms).
