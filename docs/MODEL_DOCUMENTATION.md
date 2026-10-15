# ss-lr-model: Model Documentation

Anthony Colavito · figures from the model run of October 9, 2026 (DECISIONS.md F-35)

ss-lr-model is a long-range projection model of Social Security's Old-Age, Survivors and Disability Insurance (OASDI) program, written in R. It follows the Office of the Chief Actuary's (OCACT) methodology for the 2026 Trustees Report, step for step where OCACT publishes enough to do so. It projects insured workers, every type of beneficiary, benefit levels, and trust fund income, cost and reserves from 2026 to 2100. Its purpose is to score reform proposals, starting with increases in the normal retirement age and changes to the PIA formula, against a baseline that reproduces the Trustees'.

With nothing fitted to cost or income, the model's 75-year actuarial balance is −4.45% of taxable payroll against the Trustees' −4.42%. The OASI and combined trust funds are depleted in 2032 and 2034, as in the report. The companion document, [Model vs Trustees 2026: Comparison of Outputs](COMPARISON_TR2026.md), has the full comparison.

## Design principles

1. **OCACT's method, OCACT's assumptions.** The structure, equations and parameters come from *Long-Range OASDI Projection Methodology, 2026 TR* and the 2026 assumption memos and actuarial studies. Where OCACT relies on data that isn't public (the Master Beneficiary Record, a 10% sample of new awards, side models), the model uses the closest public source and records the substitution.
2. **Published projections are inputs.** Population, mortality, immigration, the economic assumptions (average wage index, COLAs, taxable payroll, interest) and covered workers come straight from the Trustees.
3. **Costs and income are built from the ground up.** Benefits, payroll tax, taxation of benefits, interest and the summary measures are computed from the model's own counts and benefit levels. Nothing is fitted to the Trustees' cost or income.
4. **Published beneficiary totals where the method can't reproduce them.** For dependents and survivors, OCACT's linkage method run on public data drifts far from the Trustees' totals. The model takes the published totals each year and uses the method only to split them and to give the reform response. Disabled- and retired-worker counts match the Trustees through 2035 and are the model's own after.
5. **Every choice is logged.** `docs/DECISIONS.md` records each choice with an ID, the reason, the alternatives and its status, and every surprise as a numbered finding.

## How the model fits together

The model runs in six stages, each feeding the next:

| Stage | Scripts | Produces |
| --- | --- | --- |
| 1. Inputs | 01–05 | Population, death probabilities, program rules, covered-worker rates, immigration |
| 2. Insured status | 06–07 | Fully and disability insured rates by age, sex and cohort |
| 3. Beneficiaries | 08–16 | Counts of every beneficiary type, and the TR comparison |
| 4. Award levels | 17–23 | Award PIAs and benefits by year, sex and age; lump sums |
| 5. Benefits in current pay | 24–29 | Average and total benefits by type and fund |
| 6. Trust fund operations | 30 | Income, cost, reserves and summary measures |

Each stage is a set of numbered R scripts that read the previous stages' saved tables (`data/*.rds`) and write their own. Disabled-worker rolls and the disability-insured population depend on each other, so a fresh build runs stages 2 and 3 twice; `scripts/run_all.R` handles the order.

## Stage by stage

### 1. Inputs (scripts 01–05)

Population by single age, sex and marital status and death probabilities come from OCACT's files for the 2026 report. Program rules come from the ranypia package, checked against TR Tables V.C1–V.C3: AWI, COLAs, taxable maximum, bend points, quarter-of-coverage amounts, NRA and delayed retirement credits by cohort. Covered-worker rates by age and sex use Supplement 4.B tables to 2023 and Actuarial Study 127's age paths after, scaled to IV.B4's total covered workers. Net immigration by age is split into lawful permanent residents and the temporary or unlawfully present, consistent with V.A2.

### 2. Insured status (scripts 06–07)

As OCACT does, the model simulates careers year by year for every cohort from 1970 to 2100. It draws who has covered earnings from the covered-worker rates, using OCACT's SLCT/SRCH rule that favors people with recent work, and draws earnings from the median and a fixed distribution. It credits quarters of coverage and applies the fully and disability insured tests, including immigrants arriving at each age. The simulated rates are then scaled:

- by age group, to Supplement 4.C2 for 2013–2022;
- in level, to the Trustees' fully insured rate at 62 and disability insured rate at 50 in 2025 and 2100.

Disabled workers on the rolls long enough are added back to the disability insured, as in the methodology.

### 3. Beneficiaries (scripts 08–16)

**Disabled workers** are projected by sex, age at entitlement, duration and age. Each year has new entitlements (incidence rates from Actuarial Note 2026.6 applied to the insured not yet entitled), deaths and recoveries (Study 130 rates fitted to the assumptions memo), and conversion to retired-worker status at each cohort's normal retirement age. The disabled-worker history at ages 65–66 and past conversions are split by cohort NRA. They start from a December 2025 stock built from Supplement tables. Incidence is fitted to V.C5 through 2035; one constant factor applies after.

**Retired workers** follow OCACT's prevalence approach. The age-62 claim rate comes from the printed regression. Ages 63–69 follow the ratio of benefit to PIA at each claiming age, with the age-66 adjustment for the NRA transition, plus converted disabled workers. A factor fitted to V.C4 through 2035 is held after. Retired workers are then split by age at entitlement (62–70) from published entitlements through 2025 and the model's own incidence after.

**Aged widow(er)s** come from the deceased-spouse equation by marital status and insured status. **Dependents and survivors** use OCACT's linkage equations: children by age of parent, and spouses by age difference. All of these are scaled to the V.C4/V.C5 totals each year.

### 4. Award levels (scripts 17–23)

OCACT's awards module uses a 10% sample of new awards with full earnings histories. The public substitute is SSA's Benefits and Earnings Public-Use File (BEPUF 2020): the model takes 587,883 retired and disabled workers entitled in 2016–2020 and computes their AIMEs with ranypia (35 computation years for retired workers). The records are reweighted to reproduce 2025's award distributions by PIA and age (Supplement 6.A4, 6.B4, 6.C1, which are at December 2025 rates), and their AIMEs are summarized into OCACT's 30-interval PAPs (potential AIME percentages).

For each future year:

- careers are moved to that cohort's earnings levels by age and sex (Supplement 4.B13) and to its covered-worker rates, adjusted as in OCACT's appendix for the change in the fully insured rate at 62;
- retired-worker claiming ages are "shuttled" to the projected age-at-entitlement mix, as in OCACT's appendix.

Award PIAs follow from the PAPs and the bend points, and award benefits from the reduction or delayed credits for each claiming age, averaged over the months of that year of age. The spike of claims at the NRA sits at the NRA's age: 66 in 2025–2026, 67 from 2027. Lump-sum death payments are projected separately.

### 5. Benefits in current pay (scripts 24–29)

**Post-entitlement factors.** These measure benefit growth beyond the COLA (recomputations, and the longer lives of higher earners). They come from consecutive Supplement editions over OCACT's 2014–15 to 2023–24 window. Initial values grade to ultimate values over 20 years. Composition the model tracks itself, and the growth of the dual-entitlement excess, are taken out.

**Disabled workers** are carried by cohort from a December 2025 start fitted to Supplement 5.D1 and 5.A1.2. **Retired workers** use OCACT's matrix of age × age at entitlement, started from Supplement 5.A1.1 and 5.A3a. Each year both are rolled forward with the COLA and the post-entitlement factors, and new awards enter from stage 4.

**Dependents and survivors** get a fixed ratio to the account holder's average PIA, from December 2025 averages. **Dual entitlement**, the excess paid to retired workers also entitled as spouses or widow(er)s, follows OCACT's regressions and targets. December levels are annualized with OCACT's formula, and a loading for retroactive payments is fitted to 2024.

### 6. Trust fund operations (script 30)

- **Payroll tax:** OCACT's collection-lag formula applied to the Trustees' taxable payroll.
- **Taxation of benefits:** the ratio of tax to benefits, the Trustees' through 2035 and OCACT's formula after, applied to the model's benefits.
- **Administrative costs and railroad interchange:** the Trustees' through 2035, then OCACT's growth formula.
- **Interest:** each fund's yield on its average assets: the yield implied by Table IV.A for 2026–2035, the Trustees' long-range effective yield (VI.G1) after.

Annual income and cost rates, trust fund ratios, depletion years, summarized rates, the actuarial balance and the unfunded obligation follow the methodology's formulas.

## Data sources

| Source | Used for |
| --- | --- |
| [2026 Trustees Report](https://www.ssa.gov/oact/TR/2026/) tables and single-year tables | Population, mortality and economic inputs; beneficiary totals (V.C4, V.C5); short-range administrative costs, railroad interchange and taxation ratios (IV.A); checks (IV.B) |
| OCACT population and death-probability files, 2026 TR | Population by age, sex and marital status; death probabilities |
| *Long-Range OASDI Projection Methodology, 2026 TR* | Structure, equations and published parameters throughout |
| 2026 economic and disability assumption memos; Actuarial Studies 121, 127 and 130; Actuarial Note 2026.6 | Employment paths, disabled-worker incidence, death and recovery rates |
| [Annual Statistical Supplement](https://www.ssa.gov/policy/docs/statcomps/supplement/2026/), 2026 and 2012–2025 editions | Insured history (4.C2), earnings by age (4.B), beneficiaries and benefits in December (5.A–5.G), awards (6.A–6.D) |
| [BEPUF 2020](https://www.ssa.gov/policy/docs/microdata/bepuf-2020/index.html) (SSA public-use file) | Earnings histories of 2016–2020 new beneficiaries (its AIME and PIA columns are not used) |
| [ranypia](https://github.com/anthonycolavito/ranypia) (R package) | AIME, PIA and program rules by year and cohort |
| [DI Annual Statistical Report](https://www.ssa.gov/policy/docs/statcomps/di_asr/2024/) | Workers' compensation offset counts (check) |

Every raw file is listed with its source and download date in `data-raw/SOURCES.md`.

## Calibration and validation

**Fitted to published values**

- Insured rates, at four TR figures for 2025 and 2100, plus 4.C2 by age group for 2013–2022.
- Disabled- and retired-worker counts through 2035.
- Dependent and survivor totals, every year.
- December 2025 benefit levels by age, entitlement year and duration.
- The 2025 award distributions.
- The retroactive-payment loading, fitted to 2024.

**Projected and checked**, with nothing fitted to them:

| Measure | Model | Trustees |
| --- | --- | --- |
| 75-year actuarial balance, OASDI | −4.45% | −4.42% |
| 75-year summarized cost rate | 18.13% | 18.10% |
| Depletion: OASI / OASDI | 2032 / 2034 | 2032 / 2034 |
| Annual cost rate gap, through 2080 | within 0.3 points | — |
| Retired workers, 2050 / 2100 | −0.5% / +2.9% | V.C4 |
| Disabled workers, 2050 / 2100 | −1.1% / +1.9% | V.C5 |
| Average PIA of retired workers, December 2025 | $2,442 men, $1,783 women | $2,448, $1,792 (Supplement 5.B7) |

The 75-year match is partly offsetting: OASI cost runs low around midcentury, and OASI and DI cost run high after 2080. [Model vs Trustees 2026: Comparison of Outputs](COMPARISON_TR2026.md) covers every measure.

## Departures from OCACT and known limitations

Where OCACT uses data or side models that are not public, the model substitutes:

| OCACT uses | The model uses |
| --- | --- |
| 100% Master Beneficiary Record for starting matrices | Supplement tables by age, sex, entitlement year and reduction status |
| 10% sample of 2022 new awards with earnings | BEPUF 2020 synthetic earnings of 2016–2020 new beneficiaries, reweighted to 2025 awards |
| Short-range model for 2026–2035 | Factors fitted to the Trustees' 2026–2035 counts; the Trustees' short-range administrative costs, railroad interchange and taxation ratios |
| Linkage factors and residuals for dependents | Published totals each year, split by the linkage method |
| Workers' compensation offset factors | Implicit in benefit averages (offset is in the published amounts) |
| Fund-specific bond yields; benefit payment timing | Yields by fund implied by IV.A for 2026–2035, one published yield after; mid-year payment |
| Dispersion adjustment to future earnings | Not done (no public data) |

**Known limitations**

- **Counts after 2035.** Disabled- and retired-worker counts are the model's own and drift up to 2.9% from the Trustees'. Scaling them to the published totals was considered and not adopted (P-11, F-37).
- **Claiming within a year of age.** Claiming is modeled in whole years of age, with claim factors averaged over the months of each year. That matters for reforms that move the NRA, since claims bunch at the NRA month.
- **Program rules.** Totalization isn't in ranypia. The retirement earnings test was added to ranypia on October 12, 2026 (`earnings_test()`, off by default) but the model doesn't use it yet. Neither affects current-law award levels (PB-05); the earnings test will matter for NRA reforms.
- **Smaller open items.** The comprehensive review's bugs (DECISIONS.md F-34) are fixed and rebuilt (F-35). Still open: shuttling uses economy-wide rather than sample covered-worker rates; the memo's 2025 DI death and recovery rates are fitted as 2026; select DI rates run half a year late; covered-worker rates use the July population; the 2027 dip in income isn't reproduced; DI award eligibility uses one year, where OCACT weights the current and prior year; widow(er)s past 70 are all taken as uninsured (AW-04). DI benefits run 4.6–4.8% above the Trustees' late in the period, mostly from DI counts (DP-08).

## Running the model

The repository is `github.com/anthonycolavito/ss-lr-model`. Generated tables (`data/`, `outputs/*.rds`) are not committed, so a new copy is rebuilt from the raw files:

```
Rscript scripts/run_all.R        # everything, in order; logs in outputs/logs/
Rscript scripts/run_all.R 17     # resume from a step
```

- **Requirements:** R with `ranypia` (from GitHub), `Rcpp` and a compiler, `dplyr`, `tidyr`, `data.table`, `matrixStats`, `readr`, `readxl`, and `pdftotext`.
- **BEPUF files.** The two BEPUF 2020 files are too large for git and go in `data-raw/bepuf/`. Without them, stages 4–6 are skipped.
- **Run time.** About an hour on 2 cores: the insured-status simulation (script 06, about 38 minutes, using every core) and moving careers to future cohorts (21, about 20 minutes) take most of it.
- **Reproducibility.** The insured simulation is random, so a rebuild on a different core count differs slightly from logged figures; every fitted target is still hit exactly.

Key outputs: `data/trust_fund.rds` (operations and summary measures), `data/annual_benefits.rds`, beneficiary and benefit tables by type, and the comparison files `outputs/tr_comparison.csv`, `benefits_vs_tr.csv` and `trust_fund_vs_tr.csv`.

## Status and next steps

Stages 1–6 are built, and the comprehensive review's fixes (F-34) are in and rebuilt end to end through run_all.R (F-35, October 9, 2026). Next:

1. Reforms (Phase 7): an NRA increase and PIA-factor changes, scored against the model's baseline. ranypia now has the retirement earnings test; bringing it into the model and revisiting the spouse claiming response (F-16) come first for NRA reforms.
2. A taxable payroll module calibrated to VI.G1, before the first revenue option.

P-11 was settled on October 9, 2026: disabled-worker counts after 2035 keep DP-08's constant incidence factor, and worker counts are not scaled to V.C4/V.C5.

The build plan and phase checklists are in the project's build guide. Every modeling choice and finding is in `docs/DECISIONS.md`.
