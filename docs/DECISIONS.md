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
| S-05 | Women's SLCT/SRCH graded toward men's as women's covered rate goes from 90% to 100% of men's; SRCH blended on a log scale | Methodology 3.1.c, footnote 2. Log scale because SRCH is calibrated on one and spans 1–30,000 (F-04) | Linear blend (used through v4) | Adopted |
| S-06 | Ages 85+ hold each cohort's age-84 simulated share | Methodology (FSIM_LEG held beyond 84) | — | Adopted |
| S-07 | One search parameter per Supplement 4.C2 age group and sex, not per single age | OCACT's values (by single age) aren't published; our history is in 5-year groups, so single-age values would be unidentified; SLCT and SRCH substitute, so only one can be fitted per target | Single-age values fitted to interpolated targets; one value for all ages 25+ (v3: fit 3–4 points off with an age tilt) | Adopted |
| S-08 | The fitted parameter is SRCH (log scale 1–30,000), SLCT fixed at 4; ages 13–17 at SLCT 1, SRCH 3 | With SRCH fixed, SLCT stops mattering once no record within SRCH qualifies; SRCH spans from random to strictly longest gaps | Fit SLCT (tried: ran out of range) | Provisional |
| S-09 | SLCT may be fractional (a pick uses floor or floor+1 at random) | Smooth calibration | Integers only | Adopted |
| S-10 | Calibration: for each k in {0, ⅓, ⅔, 1}, fit SRCH group by group, youngest first, to zero average gap with 4.C2 fully insured (1990–2025); choose k by the TR's fully insured share at 62 (men 92.6% 2025, 88.4% 2100; women 88.5%, 87.7%). Men first, then women | Each age's status depends only on earlier work; every k then matches history, so the projection picks k. After 40 QCs status is permanent, so remaining gaps at older ages come from earlier in those cohorts' lives and are reported, not forced | Joint grid over all parameters (too costly) | Not working yet: 20–24 can't get low enough and 25–29 needs near-random selection, so most groups hit the search bounds (see F-01) |
| S-11 | Not yet modeled: disabled-worker add-back to disability insured (DINADD), pre-1978 ANNUAL factor, 10% earnings retention for DACA | DINADD needs Phase 2 counts; others unpublished/minor | — | Pending (DINADD in Phase 2) |

## Calibration layer (scripts/07)

| ID | Choice | Reason | Alternatives considered | Status |
| --- | --- | --- | --- | --- |
| C-01 | Scale simulated insured rates to published figures, both sexes and both statuses | Keeps simulation gaps (F-05, F-06, missing DINADD) out of later phases; same rule for everyone so differences by sex reflect policy, not uneven treatment | Women only (inconsistent); no layer (carries 3-point errors forward) | Adopted |
| C-02 | Base factor = 4.C2 rate / simulated rate by status, sex and 4.C2 age group, pooled over 2013–2022; disability compared below 65 only | The latest ten years built on actual earnings data. 4.C2's 2023–2025 values are estimates (earnings data lag ~2 years) and differ from OCACT's newer series (di_ins_hist.xlsx) by up to 1.4% in total, 3.6% at 20–24; through 2022 the two agree within 0.1%, so the choice of source doesn't matter there | 2016–2025 (used briefly: mixes in estimates of uncertain vintage); switch to di_ins_hist.xlsx for disability insured (newer, but provenance and vintage not documented, and no fully insured counterpart) | Adopted |
| C-03 | Single-age factors linear between group midpoints (u20 at 16, 75+ at 80), flat beyond | No steps at group edges, as for the inputs (I-04) | Step by group | Adopted |
| C-04 | After 2022 a second factor per status and sex grades linearly from 1 (2022) to the value that hits the TR's 2025 figure, then linearly to the value that hits the TR's 2100 figure: fully insured at 62 by sex; disability insured at 50, one factor for both sexes. Caps: fully ≤ 99.5%, disability ≤ fully | The TR's 2025 figures are its own estimates for a year without complete data, the same kind of number as 4.C2's 2023–2025, and the TR is what we calibrate to. Second factors: 2025 0.97–1.02, 2100 0.98–1.05 | Leave 2025 as a check (women at 62 then −2.1, disability at 50 +2.7); age-specific grading (no data to set it) | Adopted |
| C-06 | OCACT's disability-insured history by age group (data-raw/oact_insured/di_ins_hist.xlsx, 1970–2026) kept as a cross-check, not a target | See C-02 | — | Adopted |
| C-05 | Disability factors currently absorb the missing disabled-worker add-back (DINADD); recompute after Phase 2 adds it | DINADD needs disabled-worker counts | — | Pending |

## Phase 2: disabled workers (inputs, scripts/08)

| ID | Choice | Reason | Alternatives considered | Status |
| --- | --- | --- | --- | --- |
| DI-01 | Base probabilities of death and recovery = Actuarial Study 130 select-and-ultimate tables (2016–20 experience; Tables 7A–7C, 14A–14B) | The same tables OCACT uses as its base (methodology 3.2.b, items 26–27) | — | Adopted |
| DI-02 | Starting stock = Supplement 2026 5.A1.2 (December 2025, single ages 20–66, under-20 as age 19) and 5.D1 (by year of entitlement) | Latest published MBR counts; matches TR V.C5 (7,126 thousand) | Study 130 Table 6 (December 2024, age groups) | Adopted |
| DI-03 | Incidence by single age and sex from Actuarial Note 2026.6 (cohort born 2006), on the note's own age convention: exposure at the start of the year aged a (December 31 age) uses the note's row a + 1 (the year that cohort attains a + 1); new entrants are split evenly between entitlement ages a and a + 1. Ultimate levels not rescaled | The note is computed from the 2026 TR's own rates and, read at entitlement age, reproduces the methodology's age-sex-adjusted 4.8 exactly (F-07). Rescaling each group to the rounded table would add steps at group edges for ±2% | Rescale to the methodology's group rates; memo table (award basis, the wrong basis); our own 2025 awards ÷ exposure | Adopted |
| DI-04 | History for calibration: Study 130 Tables 3–6 (2001–24) and TR V.C5 (1975–2100) | Published, consistent with each other (men + women = total in every year; 2024 stock matches V.C5) | — | Adopted |

## Phase 2: starting stock (scripts/09)

| ID | Choice | Reason | Alternatives considered | Status |
| --- | --- | --- | --- | --- |
| DS-01 | Disabled workers at December 2025 by sex × entitlement age × duration, rebuilt as a prior table and raked to Supplement 2026 5.A1.2 (single age) and 5.D1 (year of entitlement) | OCACT reads this from the MBR; only the two margins are published. Raking matches both exactly | One duration pattern for all ages (misstates early-duration deaths for young entrants) | Adopted |
| DS-02 | Prior entrants: awards by year, sex and age group at award (6.C2, annual 1980–2025; 1966–79 from the nearest published year), spread to single ages by note incidence × disability-insured exposure; award year and age treated as entitlement year and age | Only source by year and age; the prior only shapes the age × duration cross, the margins fix the totals | Study 130 Table 3 (2001–24 only) | Adopted |
| DS-03 | Prior survival: Study 130 base death and recovery (2016–20), half-year exposure in the year of entitlement; conversion at NRA removes those past 66; attained age = entitlement age + duration, split evenly with one year older | The study's own tables and the note's half-year convention | Year-specific historical rates (not published by age and duration) | Adopted |
| DS-04 | Under-20 stock (674 people) placed at age 19; entitlement ages 16–19 use half the note's age-20 incidence (16–17) or the full rate (18–19) | Tiny; 5.A1.2 has no single-age detail under 20 | — | Adopted |

## Phase 2: projection (scripts/10)

| ID | Choice | Reason | Alternatives considered | Status |
| --- | --- | --- | --- | --- |
| DP-01 | Incidence: note rates on the note's age convention (DI-03), ultimate from 2036; for 2026–2035 one factor per year (all ages, both sexes) fitted so year-end current pay matches TR V.C5 | OCACT reconciles the first ten years with its short-range model (IPROJG); V.C5 is the only published result of that | Fixed path from 2025 rates to ultimate | Provisional |
| DP-02 | Deaths: Study 130 base × factor by sex × general-population improvement by single age and sex relative to 2025 × exp(−g(t−2025)); factor and g fitted so the age-sex-adjusted death termination rate is 26.3 in 2026 and 12.5 in 2100 (memo). Sex split per Study 130 Table 5 (2024). Result: factor 1.05 men, 1.07 women; g = 0.18%/yr | Memo section 3 publishes the outcome; general-population improvement alone gives 13.7 in 2100 (our select/duration mix and rebuilt standard population differ from OCACT's) | Improvement alone (misses the published 2100 rate by 10%) | Adopted |
| DP-03 | Recoveries: Study 130 base × factor by sex grading linearly from 2025 to 2035, constant after; fitted so the adjusted rate is 18.7 in 2026 and averages 11.1 over 2036–2100 (memo). None at 66+. Result: 1.85/1.99 in 2025 → 1.05/1.13 | Memo section 4 | Methodology's two-stage path (10-year average by 2035, ultimate by 2045) | Adopted |
| DP-04 | Conversions at NRA by birth year: at year-end those aged a born in b remain with probability min(1, max(0, NRA_b − a)) | Uniform birth months | — | Adopted |
| DP-05 | Exposure = disability insured (scripts/07) × population at the start of the year − currently entitled at that age | Methodology 3.2.c | — | Adopted |
| DP-06 | Age-sex-adjusted rates use rebuilt standard populations: disabled workers December 1999 (5.D4 bands) for death and recovery; disability insured December 1999 (4.C2) for prevalence | Approximates OCACT's standards; the 1999 incidence weights reproduce the published 4.8/4.6 within 0.03 (F-07) | — | Adopted |
| DP-07 | IBNR: project the currently entitled; current pay = entitled × IBNR(duration), grading linearly from the 2024→2025 factors (backlog) to the mean of the 2011→2015 factors by 2029 | Methodology 3.2.c; OCACT's factors come from 2005–2014 entitlements; the TR expects pending awards to be realized by about 2029. Four 2011–2015 pairs agree closely (d0: men .48–.54, women .42–.48) | Today's factors held fixed (current pay ~10% low long-run); no IBNR | Adopted |

## Versions of the insured simulation

| Version | Description | Fully insured vs 4.C2, 1990–2025 (RMSE, points) | Age 62 vs TR, 2100 (points) |
| --- | --- | --- | --- |
| v1 | Latent attachment, assumed teen ramp, no immigrants | men 1.3, women 2.2 | men +2.9, women +3.7 |
| v2 | v1 with Study 127 teen shape | men 1.3, women 2.2 | men +1.8, women +2.5 |
| v3 | OCACT SLCT/SRCH, one value for ages 25+, immigrants | men 3.0, women 4.1 (too low at 25–34, too high at 65–74) | men +3.4, women +1.0 (k at its floor, 0) |
| v4 | OCACT, SRCH by age group and sex, smoothed single-age inputs, F-03 fix; k men ⅔, women 0 | Fully insured RMSE 1990–2025, ages 20–74: men 1.8, women 3.1 points (v3: 3.0, 4.1, but v3 had the F-03 bug). Most groups still at SRCH bounds | Age 62 vs TR: men −0.9 (2025), −0.1 (2100); women −4.6, −3.0. Disability insured at 50: 76.8% / 77.2% (TR 75.9 / 77.4), before DINADD |
| v5 | v4 plus log-scale grading of women's SRCH (S-05), women refitted, k men ⅔, women ⅓. Men carried over from v4 (grading affects women only) | Fully insured RMSE 1990–2025, ages 20–74: men 1.8, women 2.6 points | Age 62 vs TR: men −0.9 (2025), −0.1 (2100); women −0.9, −2.9. Disability insured at 50: 76.5% / 76.8% (TR 75.9 / 77.4), before DINADD. Calibrated by 07: 2100 targets exact; women's 2100 factor 1.04, all others within 0.94–1.10 except under-20 (0.63–0.72) |

## Findings

| ID | Finding | Evidence | Implication |
| --- | --- | --- | --- |
| F-01 | The simulation can't reproduce the jump in fully insured rates from 20–24 (76% men) to 25–29 (89%) in Supplement 4.C2 with any one search setting | Men, k = ⅔, 1990–2025 average gap: SRCH 1 gives +14.0 / +5.8 points (20–24 / 25–29); SRCH 30 gives +2.9 / −1.2; SRCH 30,000 gives +1.0 / −6.6. Smoothing covered rates within groups (I-04) changed this little | The selection parameters aren't the binding problem |
| F-02 | Early-career earnings looked like the cause of the 20–24 overshoot, but the data don't support wider dispersion for young workers | Scaling medians under 25 by 0.6 moved 20–24 to +1.1 at SRCH 30. But 2023 taxable mean ÷ median (4.B13 ÷ 4.B5 against 4.B6) is 1.17 for men in their 20s, the same as ages 30–59; fitting a log-scale stretch of the all-ages distribution, with the taxable maximum applied, gives 0.77 for the 20s (less spread than average) and 1.0–1.5 for ages 30–64 | Not adopted. The overshoot was mostly the bug in F-03 |
| F-03 | Bug: insured status was computed after the whole lifetime was simulated, so a record re-drawn as a new immigrant at a later age had its earlier status wiped too | One cohort (1980, men): fully insured at 20 was 0.810 when stopped at 24 and 0.738 when run to 84. Full runs understated insured rates at younger ages (v1–v3 all affected); calibration stopped at each group's top age, so it saw fewer wipes than the full run, which drove SRCH between its bounds | Fixed: status is now recorded each year as the history stands then (R/insured_sim.R). Identical to the old calculation when there are no immigrants (checked) |
| F-04 | Women's gap at 62 came mostly from grading SRCH linearly toward men's | With women's own SRCH 1 and men's 30,000, a 10% weight gave ~3,000. Women-only tests (5,000 records): log-scale blend + refitted women, k = ⅓: age 62 −0.8 (2025), −3.1 (2100); history RMSE 2.6 (was −4.6, −3.0, 3.1). 90% cutoff variants (80%, 95%) did worse | Adopted log blend (S-05) |
| F-05 | The remaining ~3-point shortfall for women at 62 in 2100 isn't from the inputs we tested | Women's covered rates +5% by 2100: −1.2. Work rate of the temporary or unlawfully present (k) 0–1: −2.7 to −3.9. Their sex split moved to 54% men (ours 47%): −2.5. With men's settings, simulated work-authorized women are slightly more often insured than men (92.6% vs 91.6%, 2037 cohort) | Unexplained; left to a calibration layer |
| F-06 | Women are too high in early history (1970s–80s, +4 to +5 points) in every variant | Unaffected by grading, k or trend | Candidate cause: before 1978 a QC required $50 earned in the calendar quarter; part-year work (more common for women then) earned fewer QCs than annual earnings imply. OCACT's ANNUAL factor (S-11) handles this; not modeled |
| F-07 | The note's incidence differs from the methodology table by age convention, not by rate definition. Both are entitlement basis on the same exposure (insured, not currently entitled, at the start of the year). The note's row x is the year the cohort attains age x; the methodology's groups are by age at entitlement (age last birthday), which averages x − ½ over that year. The memo's table is a third series, on an award basis | Age-sex-adjusted on OCACT's standard population (disability insured not in current pay, December 1999; weights reproduce the published 4.8 and 4.6 to within 0.03): methodology 4.77, memo 4.57, note as printed 4.65, note at entitlement age (mean of rows a and a+1) 4.77. Group gaps fall from 3–5% to within ±2–3% | Resolved: DI-03 updated |
| F-08 | On the December 2025 stock, Study 130's 2016–20 base rates give 196k deaths and 45k recoveries a year; 2025 actuals were 218k deaths (6.F2) and ~85–90k recoveries (6.F2 disability ceased 84.6k; Study 130 Table 5, 2024: 90.4k) | scripts/09 output | Sets the starting projection factors (DPROJG ≈ 1.11, RPROJG ≈ 2) as OCACT does: start from the latest actual rate, then grade per the memo |
| F-09 | IBNR factors from the two latest 5.D1 vintages (December 2024 → 2025) reflect a processing backlog: share of the entitled in current pay by duration 0–4 = men .32 .69 .91 .98 .995, women .28 .62 .86 .96 .99 (survival model checks to 1–2% at durations 5–15). Held fixed, current pay runs ~10% below V.C5 from 2050 and 2026 needs 1.2M entitlements. Without IBNR (entitled = current pay), 2036–2100 is within −2.6% to +0.8% of V.C5, but the adjusted death rate rises 2026–2030 as recent cohorts fill in | scripts/09, 10 | OCACT's factors come from 2005–2014 entitlements (normal processing) and the TR expects the backlog to clear by about 2029. Proposed: estimate normal-era IBNR from older 5.D1 vintages and grade from today's factors to those |
| F-10 | With incidence at the note's ultimate rates and deaths and recoveries on the memo's published paths, current pay after 2035 runs 2–8% below TR V.C5 (2040 −2.2%, 2060 −7.7%, 2100 −4.5%). Our entitled stock lands within ~1% of V.C5, and prevalence measured on current pay (40.2 in 2100) is close to the memo's 40.7 | scripts/10 | Likely sources: our disability-insured base (rebuilt from 4.C2 and two TR anchors) and the long-run IBNR level. Open: how to close it (DP-08) |
