# Decision log

Every modeling choice that isn't dictated by the OCACT methodology or by law, with the reason and the alternatives considered. Newest decisions are added at the end of each section. "Methodology" means *Long-Range OASDI Projection Methodology, 2026 TR* (`docs/reference/2026_LR_Model_Documentation.pdf`).

Status: **Adopted** (in use), **Provisional** (in use, to revisit), **Superseded** (replaced; kept for the record), **Pending** (decided, not yet built).

## Project-level

| ID | Decision | Reason | Alternatives considered | Status |
| --- | --- | --- | --- | --- |
| P-01 | Follow OCACT's long-range structure; take published demographic and economic projections as inputs | Avoid rebuilding what OCACT publishes; stay comparable to the Trustees | Build demography and economics ourselves | Adopted |
| P-02 | Project every year from 2026 with our own model, including the first decade | Reforms must move every year; OCACT's short-range model isn't public | Borrow the TR's 2026–2035 path | Adopted |
| P-03 | Score reforms against our own baseline; fall back to applying our ratio to the TR baseline if our baseline is far off | Consistent internal comparisons | Score as differences applied to the TR | Adopted |
| P-04 | Project retired workers as a stock (prevalence rates), as OCACT does | Needs less data; gives the NRA claiming response | Flow model built up from awards | Adopted |
| P-05 | Simulate insured status (OCACT's INSURED model) rather than trend historical rates | Follows the methodology; responds to work patterns | Cohort-trend insured rates | Adopted |
| P-06 | Program rules (AWI, COLAs, taxable max, bend points, QC amounts, NRA, reduction and delayed credits) come from ranypia's `current_law()` | One source of rules for the projection model and the benefit calculator; reforms are written once | Copy rules into `params/` | Adopted |
| P-07 | R, in the `ss-lr-model` GitHub repo; raw files in `data-raw/` are never edited; everything in `data/` and `outputs/` is rebuilt by scripts | Traceability | — | Adopted |
| P-08 | Follow the actuaries' auxiliary beneficiary categories wherever possible | Faithfulness to OCACT | Collapse small categories | Adopted |
| P-09 | Model horizon ends in 2100 (OCACT runs to 2105) | Published population ends in 2100 | Extrapolate population | Adopted |

## Phase 0: data and parameters

| ID | Decision | Reason | Alternatives considered | Status |
| --- | --- | --- | --- | --- |
| D-01 | Population: OCACT's Dec 31 and Jul 1 files (2026 TR intermediate), by single age 0–100+, sex, marital status | Published; Jul 1 matches TR V.A3 to rounding | Average two Decembers to estimate July (used before the July file arrived; within 0.02%) | Adopted |
| D-02 | Death probabilities: OCACT historical (1900–2023) joined to projected (2024–2100) | One continuous series; life expectancy matches TR V.A4 within 0.06 years | — | Adopted |
| D-03 | Single-year TR tables: keep only historical and intermediate blocks (`R/read_tr.R`) | The workbook stacks all three alternatives | — | Adopted |
| D-04 | The 1999 COLA differs: ranypia has 2.4% (as announced), the TR 2.5% (after the legislated correction). Tolerated in the check; flagged for a fix in ranypia | Benefits have been paid on 2.5% since the correction; understates pre-1999 PIAs by about 0.1% | Patch ranypia from here (not done: separate package) | Provisional |
| D-05 | Beneficiary starting values from the 2026 Annual Statistical Supplement (December 2025); 2025 edition for sections not yet released (2.A, 4.B, 4.C) | Matches our December 2025 starting point | Roll 2024 data forward | Adopted |

## Phase 1: insured status

### Inputs (`scripts/04_insured_inputs.R`)

| ID | Decision | Reason | Alternatives considered | Status |
| --- | --- | --- | --- | --- |
| I-01 | Covered-worker rate by age group = Supplement 4.B5 covered workers ÷ July 1 population; "72 or older" applied to ages 72–84; rate 0 at age 13; 1937–40 use 1941 population | 4.B5 is the same concept as TR IV.B4 (within 0.3%) | — | Adopted |
| I-02 | Years between 4.B5's selected years (before 1995) filled by linear interpolation of rates | Only selected years published | — | Adopted |
| I-03 | Ages 14–19 shaped by Study 127 employment-to-population ratios (16–17, 18–19), rescaled to the 4.B5 under-20 total; ages 14–15 assumed 0.15 and 0.40 × the 16–17 ratio; pre-1981 use 1981's shape | Data-based shape; the total stays exact. 14–15 aren't in the labor survey | My first assumed ramp (6% at 14 to 75% at 19) | Adopted (replaced the assumed ramp) |
| I-04 | Ages 20–84 smoothed within groups: piecewise-linear curve through group midpoints, joined to the 18–19 level at 18.5, midpoints adjusted until each group's total is exact; cap 0.99; 1937–39 exempt from the exactness check | Flat group rates overstate work at 20–21 and distort when people first become insured; OCACT uses single-age rates we don't have | Flat within groups (used before); monotone spline on cumulative workers (rejected: rates balloon at 77–84 where population falls fast) | Adopted |
| I-05 | Projected rates (2024–2100) follow Study 127's employment-ratio path for each age group and sex (held after 2096), scaled each year so total covered workers match TR IV.B4 | Ties the age shape to the Trustees' labor projections; rates at older ages rise with longevity and the NRA | Hold 2023 rates (used before: pushed age-adjusted rates up ~1.5 points) | Adopted |
| I-06 | Women get a linear trend (+0.9% by 2100) so the men–women gap in the change of age-adjusted covered rates matches the TR (men −0.8, women +1.2 points, 2024–2100) | A single scale factor can't move the sexes in opposite directions | Separate trends for both sexes (not identified: the IV.B4 total fixes the level) | Adopted |
| I-07 | Known difference: our age-adjusted covered rates rise ~1.5 points more than the TR's over 2024–2100 (men 68.4→68.8 vs TR 68.7→67.9) | Likely the "72+" mapping; IV.B4 fixes the total so no clean lever. The insured calibration targets the TR's 2100 insured rates, absorbing much of it | — | Provisional |
| I-08 | Median earnings: ratio to the AWI by group (4.B6), interpolated between published years, held at 2023 after; by single age, interpolated linearly between group midpoints | Medians can't be summed, so no total-preserving smoothing; QC-to-median ratio stays fixed in projection, as both grow with the AWI | Flat within groups (used before) | Adopted |
| I-09 | QC amounts before 1978: $250 scaled back by the AWI two years earlier relative to 1976 | Methodology 4.2.b input 19 does the same; annual simulation can't reproduce quarterly crediting | OCACT's ANNUAL comparability factor (not published) | Adopted |
| I-10 | Earnings relative to the median (FRAC): from OCACT's 2023 net compensation distribution; power-function extension below $5,000 fitted to the first two brackets; same shape for both sexes, all ages and years | OCACT's FRAC uses Supplement 4.B7/4.B9 (coarser); net compensation has finer low brackets | 4.B7/4.B9 | Adopted |

### Immigration (`scripts/05_net_immigration.R`)

| ID | Decision | Reason | Alternatives considered | Status |
| --- | --- | --- | --- | --- |
| M-01 | Net immigration by age, sex, year = population residual: P(t,a) − P(t−1,a−1) × survival | Consistent with the TR population; totals within 1–2% of V.A2 in projection years | Age distribution from outside sources | Adopted |
| M-02 | Age 0 dropped so births never enter; age 100+ dropped (open group) | Age 0 at Dec 31 mixes births and infant arrivals | — | Adopted |
| M-03 | One arrival age-sex pattern for all immigrants: the residual averaged over 2030–2050 | No published age detail by legal status | Separate patterns by status (no data) | Provisional |
| M-04 | LPR entrants = V.A2 net LPR change (new arrivals − legal emigrants + adjustments of status) × arrival pattern; adjustments of status treated as entrants with no prior earnings | OCACT treats LPR entrants alike | Credit adjusters with some prior covered work | Provisional |
| M-05 | Temporary or unlawfully present stock: totals anchored to the Trustees' figures (0 in 1963; 2.6M 1990 excl. IRCA; 5.0M 1996; 9.9M start of 2000; 11.7M 2005; 13.5M 2008; 12.5M 2013; 13.4M 2020; 16.9M 2024; 17.3M 2025), interpolated between; from 2026 V.A2 flows (arrivals − departures − adjustments of status) less deaths | Published totals and flows (TR text; Demographic Assumptions memo §3.5, Tables 3.1, 3.5) | — | Adopted |
| M-06 | Before 2026, exits assumed 3% of the stock a year (arrivals fill to the anchored total) | No published flows by age before 2026 | — | Provisional |
| M-07 | Exits spread by age ∝ stock × exp(−β(age − 30)), β solved so 2100 = 28.7M (β = 0.017: exit weight 1.7× at 20 vs 50); 2029 then lands at 15.5M vs TR 15.6M | Memo: higher emigration for recent (young) entrants; with V.A2 fixing flows, β governs deaths | Uniform exits by age (overshot 2100 by 2.9M) | Adopted |
| M-08 | DACA recipients not modeled separately (OCACT includes them in the simulation as if LPRs) | Small (~0.5M); no age data | — | Provisional |

### Simulation (`R/insured_sim.R`, `src/insured_select.cpp`, `scripts/06_insured_simulation.R`)

| ID | Decision | Reason | Alternatives considered | Status |
| --- | --- | --- | --- | --- |
| S-01 | OCACT's selection of non-covered workers (SLCT/SRCH search), 30,000 records per cohort and sex, written in C++ | Methodology 3.1.c; clustered gaps matter for the disability-insured recency test | Latent attachment + yearly noise (v1; better history fit but scattered gaps) | Adopted (v1 Superseded) |
| S-02 | Covered workers among records draw QCs independently each year from FRAC | Methodology 3.1.c | Persistent earnings rank (v1) | Adopted |
| S-03 | New LPR entrants: a random share of records each year, prior QCs wiped, 0–4 QCs that year with equal chance, never chosen as non-covered that year | Methodology 3.1.c | — | Adopted |
| S-04 | Temporary or unlawfully present: outside the simulation; insured share of everyone = sim × (L + 0.75 k U)/(L + U), α = 0.75 from the methodology, k (their covered-worker rate relative to others) calibrated by sex | Methodology 3.1.c formula; k (CW_OTHER) isn't published | — | Adopted |
| S-05 | Women's SLCT/SRCH graded linearly toward men's as women's covered rate goes from 90% to 100% of men's | Methodology 3.1.c, footnote 2 | — | Adopted |
| S-06 | Ages 85+ hold each cohort's age-84 simulated share | Methodology (FSIM_LEG held beyond 84) | — | Adopted |
| S-07 | One search parameter per Supplement 4.C2 age group and sex, not per single age | OCACT's values (by single age) aren't published; our history is in 5-year groups, so single-age values would be unidentified; SLCT and SRCH substitute, so only one can be fitted per target | Single-age values fitted to interpolated targets; one value for all ages 25+ (v3: fit 3–4 points off with an age tilt) | Adopted |
| S-08 | The fitted parameter is SRCH (log scale 1–30,000), SLCT fixed at 4; ages 13–17 at SLCT 1, SRCH 3 | With SRCH fixed, SLCT stops mattering once no record within SRCH qualifies; SRCH spans from random to strictly longest gaps | Fit SLCT (tried: ran out of range) | Provisional |
| S-09 | SLCT may be fractional (a pick uses floor or floor+1 at random) | Smooth calibration | Integers only | Adopted |
| S-10 | Calibration: for each k in {0, ⅓, ⅔, 1}, fit SRCH group by group, youngest first, to zero average gap with 4.C2 fully insured (1990–2025); choose k by the TR's fully insured share at 62 (men 92.6% 2025, 88.4% 2100; women 88.5%, 87.7%). Men first, then women | Each age's status depends only on earlier work; every k then matches history, so the projection picks k. After 40 QCs status is permanent, so remaining gaps at older ages come from earlier in those cohorts' lives and are reported, not forced | Joint grid over all parameters (too costly) | Not working yet: 20–24 can't get low enough and 25–29 needs near-random selection, so most groups hit the search bounds (see F-01) |
| S-11 | Not yet modeled: disabled-worker add-back to disability insured (DINADD), pre-1978 ANNUAL factor, 10% earnings retention for DACA | DINADD needs Phase 2 counts; others unpublished/minor | — | Pending (DINADD in Phase 2) |

## Versions of the insured simulation

| Version | Description | Fully insured vs 4.C2, 1990–2025 (RMSE, points) | Age 62 vs TR, 2100 (points) |
| --- | --- | --- | --- |
| v1 | Latent attachment, assumed teen ramp, no immigrants | men 1.3, women 2.2 | men +2.9, women +3.7 |
| v2 | v1 with Study 127 teen shape | men 1.3, women 2.2 | men +1.8, women +2.5 |
| v3 | OCACT SLCT/SRCH, one value for ages 25+, immigrants | men 3.0, women 4.1 (too low at 25–34, too high at 65–74) | men +3.4, women +1.0 (k at its floor, 0) |
| v4 | OCACT, SRCH by age group and sex, smoothed single-age inputs, F-03 fix; k men ⅔, women 0 | Fully insured RMSE 1990–2025, ages 20–74: men 1.8, women 3.1 points (v3: 3.0, 4.1, but v3 had the F-03 bug). Most groups still at SRCH bounds | Age 62 vs TR: men −0.9 (2025), −0.1 (2100); women −4.6, −3.0. Disability insured at 50: 76.8% / 77.2% (TR 75.9 / 77.4), before DINADD |

## Findings

| ID | Finding | Evidence | Implication |
| --- | --- | --- | --- |
| F-01 | The simulation can't reproduce the jump in fully insured rates from 20–24 (76% men) to 25–29 (89%) in Supplement 4.C2 with any one search setting | Men, k = ⅔, 1990–2025 average gap: SRCH 1 gives +14.0 / +5.8 points (20–24 / 25–29); SRCH 30 gives +2.9 / −1.2; SRCH 30,000 gives +1.0 / −6.6. Smoothing covered rates within groups (I-04) changed this little | The selection parameters aren't the binding problem |
| F-02 | Early-career earnings looked like the cause of the 20–24 overshoot, but the data don't support wider dispersion for young workers | Scaling medians under 25 by 0.6 moved 20–24 to +1.1 at SRCH 30. But 2023 taxable mean ÷ median (4.B13 ÷ 4.B5 against 4.B6) is 1.17 for men in their 20s, the same as ages 30–59; fitting a log-scale stretch of the all-ages distribution, with the taxable maximum applied, gives 0.77 for the 20s (less spread than average) and 1.0–1.5 for ages 30–64 | Not adopted. The overshoot was mostly the bug in F-03 |
| F-03 | Bug: insured status was computed after the whole lifetime was simulated, so a record re-drawn as a new immigrant at a later age had its earlier status wiped too | One cohort (1980, men): fully insured at 20 was 0.810 when stopped at 24 and 0.738 when run to 84. Full runs understated insured rates at younger ages (v1–v3 all affected); calibration stopped at each group's top age, so it saw fewer wipes than the full run, which drove SRCH between its bounds | Fixed: status is now recorded each year as the history stands then (R/insured_sim.R). Identical to the old calculation when there are no immigrants (checked) |
