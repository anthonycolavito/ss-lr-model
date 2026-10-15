# Model vs Trustees 2026: Comparison of Outputs

Anthony Colavito · figures from the model run of October 9, 2026 (DECISIONS.md F-35)

The model reproduces the 2026 Trustees Report's long-range finances closely without being fitted to them: the 75-year OASDI actuarial balance is −4.45% of taxable payroll against the Trustees' −4.42%, and the OASI and combined funds are depleted in 2032 and 2034, as in the report. Annual cost rates stay within 0.3 percentage points of the Trustees' in every year to 2080 and within 0.65 points to 2100. Beneficiary counts match the published totals through 2035 and stay within 3% after. Part of the 75-year match comes from offsetting gaps: OASI benefits run 1–2% below the Trustees' around midcentury and 1–3% above after 2080, and DI benefits run 3–5% above after 2080.

Every comparison below is labeled by how the model gets there: **input** (taken from the Trustees), **fitted** (a model factor set so the result matches), **set equal** (published total used, model splits it), or **projected** (the model's own result, nothing fitted). Unless noted, figures are the intermediate assumptions; the model run is of October 9, 2026.

## Summary measures

All summary measures are **projected**: payroll tax, taxation of benefits, interest and cost come from the model with OCACT's formulas, and only the starting reserves, taxable payroll and interest rates are inputs.

| Fund | Period | Income rate, model | Income rate, TR | Cost rate, model | Cost rate, TR | Actuarial balance, model | Actuarial balance, TR |
| --- | --- | --- | --- | --- | --- | --- | --- |
| OASDI | 2026–2050 | 14.06 | 14.06 | 16.64 | 16.75 | −2.58 | −2.69 |
| OASDI | 2026–2075 | 13.74 | 13.74 | 17.35 | 17.45 | −3.62 | −3.70 |
| OASDI | 2026–2100 | 13.68 | 13.68 | 18.13 | 18.10 | −4.45 | −4.42 |
| OASI | 2026–2100 | 11.82 | 11.82 | 16.38 | 16.37 | −4.56 | −4.55 |
| DI | 2026–2100 | 1.86 | 1.86 | 1.75 | 1.73 | +0.10 | +0.13 |

Rates are percent of taxable payroll and include beginning reserves (income) and the ending target fund (cost); TR values from Table IV.B6.

| Present value, 2026–2100 ($ billions) | Model | TR (IV.B8) | Gap |
| --- | --- | --- | --- |
| Payroll tax contributions | 85,576 | 85,559 | +0.0% |
| Taxation of benefits | 6,271 | 6,308 | −0.6% |
| Cost | 123,871 | 123,731 | +0.1% |
| Taxable payroll | 690,332 | 690,396 | −0.0% |
| Open-group unfunded obligation | 29,462 | 29,303 | +0.5% |

| Reserve depletion | Model | TR (IV.B5) |
| --- | --- | --- |
| OASI | 2032 | 2032 |
| DI | Not depleted by 2100 | Not depleted by 2100 |
| OASDI (hypothetical combined) | 2034 | 2034 |

The 25- and 50-year balances are about 0.1 point less negative than the Trustees', which reflects the model's lower OASI cost around 2040–2060 (next section).

## Annual income and cost rates

OASDI income and cost rates, percent of taxable payroll (model from `outputs/trust_fund_vs_tr.csv`, Trustees from IV.B1):

| Year | Income rate, model | Income rate, TR | Cost rate, model | Cost rate, TR | Cost gap |
| --- | --- | --- | --- | --- | --- |
| 2026 | 12.94 | 12.91 | 15.33 | 15.37 | −0.04 |
| 2027 | 13.00 | 12.85 | 15.56 | 15.62 | −0.06 |
| 2030 | 13.08 | 13.07 | 15.74 | 15.74 | +0.00 |
| 2035 | 13.17 | 13.16 | 15.83 | 15.77 | +0.06 |
| 2040 | 13.19 | 13.20 | 16.08 | 16.20 | −0.12 |
| 2045 | 13.20 | 13.22 | 16.29 | 16.57 | −0.28 |
| 2050 | 13.23 | 13.25 | 16.70 | 16.93 | −0.23 |
| 2055 | 13.26 | 13.28 | 17.31 | 17.48 | −0.17 |
| 2060 | 13.31 | 13.32 | 18.00 | 18.15 | −0.15 |
| 2065 | 13.35 | 13.36 | 18.65 | 18.74 | −0.09 |
| 2070 | 13.39 | 13.40 | 19.33 | 19.32 | +0.01 |
| 2075 | 13.43 | 13.44 | 20.01 | 19.90 | +0.11 |
| 2080 | 13.46 | 13.46 | 20.50 | 20.29 | +0.21 |
| 2085 | 13.48 | 13.47 | 20.77 | 20.45 | +0.32 |
| 2090 | 13.48 | 13.47 | 20.78 | 20.35 | +0.43 |
| 2095 | 13.48 | 13.46 | 20.69 | 20.15 | +0.54 |
| 2100 | 13.48 | 13.45 | 20.66 | 20.02 | +0.64 |

The income rate is the Trustees' within 0.03 points in every year except 2027, when the Trustees show a one-year dip in payroll tax receipts that the model's collection-lag formula does not reproduce (+0.15 points). The model's cost rate runs up to 0.3 points below the Trustees' from 2040 to 2070 and up to 0.65 points above from 2085 to 2100.

| Fund | 2030 | 2050 | 2075 | 2100 |
| --- | --- | --- | --- | --- |
| OASI cost rate, model / TR | 14.32 / 14.30 | 14.92 / 15.16 | 18.11 / 18.05 | 18.72 / 18.17 |
| DI cost rate, model / TR | 1.42 / 1.44 | 1.78 / 1.77 | 1.90 / 1.85 | 1.94 / 1.86 |
| OASI income rate, model / TR | 11.27 / 11.26 | 11.40 / 11.42 | 11.60 / 11.61 | 11.64 / 11.62 |
| DI income rate, model / TR | 1.81 / 1.81 | 1.82 / 1.82 | 1.83 / 1.83 | 1.83 / 1.83 |

## Scheduled benefits by trust fund

Benefits are **projected**: beneficiary counts times the model's average benefits, annualized with OCACT's formula. Only a loading for retroactive payments is fitted, to 2024's actual benefits. The Trustees' figures are IV.A1/IV.A2 for 2026–2035 and, after 2035, the cost rate times taxable payroll less administrative costs (OCACT's formula) and railroad interchange.

| Year | OASI, model ($bn) | OASI, TR | OASI gap | DI, model ($bn) | DI, TR | DI gap |
| --- | --- | --- | --- | --- | --- | --- |
| 2026 | 1,519 | 1,518 | +0.1% | 161 | 166 | −2.8% |
| 2028 | 1,704 | 1,700 | +0.2% | 170 | 181 | −5.7% |
| 2030 | 1,887 | 1,885 | +0.1% | 185 | 187 | −0.9% |
| 2035 | 2,366 | 2,357 | +0.4% | 229 | 227 | +0.7% |
| 2040 | 2,876 | 2,906 | −1.0% | 299 | 295 | +1.1% |
| 2050 | 4,233 | 4,301 | −1.6% | 501 | 498 | +0.5% |
| 2060 | 6,538 | 6,601 | −0.9% | 751 | 749 | +0.2% |
| 2070 | 10,070 | 10,086 | −0.2% | 1,098 | 1,076 | +2.1% |
| 2080 | 15,248 | 15,120 | +0.9% | 1,539 | 1,493 | +3.1% |
| 2090 | 22,080 | 21,680 | +1.8% | 2,199 | 2,098 | +4.8% |
| 2100 | 31,464 | 30,545 | +3.0% | 3,263 | 3,121 | +4.6% |

Where the gaps come from:

- **OASI after 2035** follows the retired-worker count gap (next section): −0.5% in 2050, +2.9% in 2100.
- **DI in 2026–2028** is below the Trustees' short-range model, which projects a surge of awards as the backlog of pending claims clears.
- **DI after 2070** is partly the disabled-worker count gap (+1.8–1.9% in 2090–2100). The rest is an average benefit per worker 2.5–3% above OCACT's, about 1.5 points of it from benefit growth at long durations.

## Beneficiaries

December counts in current pay, thousands; Trustees from V.C4 (OASI) and V.C5 (DI).

| Category | How | 2025 | 2035 | 2050 | 2075 | 2100 |
| --- | --- | --- | --- | --- | --- | --- |
| Retired workers, model | Fitted to 2035; the 2035 factor held after, as OCACT does | 53,625 | 64,804 | 69,551 | 84,941 | 89,624 |
| Retired workers, TR |  | 53,625 | 64,804 | 69,911 | 84,322 | 87,133 |
| Retired workers, gap |  | 0.0% | 0.0% | −0.5% | +0.7% | +2.9% |
| Disabled workers, model | Fitted to 2035; one constant incidence factor after (1.071, DP-08) | 7,126 | 7,475 | 9,225 | 9,821 | 10,272 |
| Disabled workers, TR |  | 7,126 | 7,475 | 9,327 | 9,837 | 10,083 |
| Disabled workers, gap |  | 0.0% | 0.0% | −1.1% | −0.2% | +1.9% |
| OASI beneficiaries, all, gap |  | 0.0% | 0.0% | −0.5% | +0.7% | +2.7% |

The other categories are **set equal**: the model takes the Trustees' totals every year and splits them by age, sex and type, so they match exactly. Their model-alone projections show how far the linkage method would drift without that step:

| Category, model alone vs TR | 2035 | 2050 | 2075 | 2100 |
| --- | --- | --- | --- | --- |
| Spouses of retired workers | +29.2% | +86.4% | +96.6% | +91.8% |
| Children of retired workers | −19.5% | −32.5% | −40.2% | −38.1% |
| Widow(er)s, aged and disabled | 0.0% | +8.3% | +8.2% | +6.9% |
| Widowed mothers and fathers | −5.5% | −27.6% | −20.7% | −20.6% |
| Children of deceased workers | −3.5% | −8.8% | −6.6% | −7.6% |
| Spouses of disabled workers | +16.9% | +46.1% | +60.0% | +54.2% |
| Children of disabled workers | +6.9% | −10.2% | −10.5% | −11.4% |

The largest model-alone gaps are in small groups: spouses of retired workers are 2.1 million of 62 million OASI beneficiaries in 2025.

Disabled-worker rates against the 2026 disability assumptions memo and V.C5:

| Measure | Model | Trustees |
| --- | --- | --- |
| Death rate, age-sex-adjusted, 2026 / 2100 | 26.3 / 12.5 | 26.3 / 12.5 (fitted) |
| Recovery rate, age-sex-adjusted, 2026 / 2100 | 18.7 / 11.1 | 18.7 / 11.1 (fitted) |
| Prevalence, age-sex-adjusted per 1,000 insured, 2035 / 2100 | 32.9 / 40.4 | 34 / 41 |
| Prevalence, gross, 2035 / 2100 | 45.4 / 60.1 | 46 / 59 |

## Covered workers and beneficiaries per 100 workers

Total covered workers are an **input**: the model's covered-worker rates are scaled so the total equals IV.B4 every year (184.7 million in 2025, 190.4 million in 2035, 199.4 million in 2100).

The ratio of beneficiaries to workers then follows the beneficiary counts above.

| Mid-year (June 30) | 2030 | 2035 | 2050 | 2075 | 2100 |
| --- | --- | --- | --- | --- | --- |
| OASI beneficiaries, model / TR (thousands) | 68,646 / 68,700 | 72,988 / 73,008 | 76,982 / 77,368 | 91,767 / 91,175 | 96,174 / 93,720 |
| DI beneficiaries, model / TR (thousands) | 8,225 / 8,229 | 8,496 / 8,493 | 10,675 / 10,768 | 11,391 / 11,413 | 11,885 / 11,696 |
| Beneficiaries per 100 covered workers, model / TR | 40.9 / 41 | 42.8 / 43 | 45.3 / 46 | 52.6 / 52 | 54.2 / 53 |

Mid-year counts are the average of the two Decembers, an approximation to the Trustees' June 30 figures; that is why 2030–2035 differ slightly although the December counts match.

## Insured population

The model simulates careers to get fully and disability insured rates by age and sex, as OCACT does, then scales them in two steps. First, factors by age group are fitted to Supplement 4.C2 for 2013–2022. Second, the levels are fitted to the four figures the Trustees publish for 2025 and 2100.

| Fitted target (TR text) | 2025, model / TR | 2100, model / TR |
| --- | --- | --- |
| Fully insured at 62, men | 92.6% / 92.6% | 88.4% / 88.4% |
| Fully insured at 62, women | 88.5% / 88.5% | 87.7% / 87.7% |
| Disability insured at 50 | 75.9% / 75.9% | 77.4% / 77.4% |

Against Supplement 4.C2 by five-year age group (25–29 to 60–64), root-mean-square gap in percentage points:

| Period | Status | Fully insured, men | Fully insured, women | Disability insured, men | Disability insured, women |
| --- | --- | --- | --- | --- | --- |
| 1990–2012 | Projected (simulation only) | 1.6 | 1.1 | 3.2 | 3.2 |
| 2013–2022 | Fitted | 0.5 | 0.7 | 0.8 | 0.6 |
| 2023–2025 | Projected, against OCACT's own estimates | 1.5 | 1.9 | 1.2 | 1.3 |

In 2023–2025 the model's fully insured rates run 1.3–1.5 points above OCACT's estimates on average, most at 25–54 (finding F-20). OCACT's estimates fall after the 2022–2024 rise in immigration, which the model's covered-worker inputs do not reflect at those ages. A fix was tested and rejected (C-08).

## Benefit levels

The Trustees publish no projected average benefits by type, so benefit levels are checked against December 2025 Supplement data, then judged through the cost comparisons above.

| Check, December 2025 | Model | Published | How |
| --- | --- | --- | --- |
| OASI monthly benefits ($ billions) | 123.29 | 123.30 (5.A4) | Fitted (start matrices) |
| DI monthly benefits ($ billions) | 12.18 | 12.18 (5.A4) | Fitted |
| Retired-worker benefits by age | — | 5.A1.1, 5.A3a | Fitted |
| Dual-entitlement excess ($ billions a month) | 6.41 | 6.41 (5.G3) | Fitted |
| Average PIA, retired men | $2,442 | $2,448 (5.B7) | Projected check, −0.2% |
| Average PIA, retired women | $1,783 | $1,792 (5.B7) | Projected check, −0.5% |
| Retired-worker averages by year of entitlement | −6.7% to +2.9% | 5.B4 | Projected check |

New awards are built from 2016–2020 earnings records (BEPUF 2020) moved to each future cohort's earnings levels and coverage, with the 2025 awards fitted to Supplement 6.A4 and 6.B4. The projected award PIA for men claiming at 67 falls from 0.44 of the average wage index in 2025 to about 0.41 from 2035 on. The Trustees publish no direct counterpart; the cost comparisons are the test.

After award, benefits grow with the COLA and post-entitlement factors estimated from Supplement editions over OCACT's window, 2014–15 to 2023–24: men's retired-worker benefits rise 0.5–1.1% a year beyond the COLA, DI benefits 0.0–0.4% after the second year.

## Inputs taken from the Trustees

These come straight from the Trustees and match by construction; small differences are rounding or recomputation.

| Input | Source | Check |
| --- | --- | --- |
| Population by age, sex and marital status, 1940–2100 | OCACT single-year population files (Alternative 2) | July 1 total and 65+ match V.A3 to within 0.001% |
| Death probabilities | OCACT death probability files | Period life expectancy at 65 within 0.3% of V.A4 (e.g. men 2100: 22.6 vs 22.6) |
| Net immigration | V.A2, distributed by age | Totals within −1.2% to −4.8% of V.A2 (births to immigrants excluded) |
| AWI, COLA, taxable maximum, bend points, NRA and delayed credits | V.C1–V.C3 (through the ranypia package) | Exact (ranypia stores the 1999 COLA as 2.4% and applies the 0.1-point correction itself; D-04) |
| Covered workers | IV.B4 | Exact total |
| Taxable payroll, interest factor, CPI | VI.G1 | Exact |
| Disabled-worker death and recovery rates | 2026 disability assumptions memo | Exact in 2026 and 2100 |
| Taxation-of-benefits parameters, productivity | Methodology and TR text | As published |

## Caveats

- **The 75-year match is partly offsetting.** OASI cost runs low around midcentury and high after 2080, and DI cost high late in the period; the 25- and 50-year balances are about 0.1 point less negative than the Trustees'.
- **What is fitted:** insured levels at four TR points; disabled- and retired-worker counts through 2035, with the retired-worker 2035 factor held after (as OCACT does) and one constant DI incidence factor after 2035 so disabled workers match V.C5 on average (1.071; without it they run 4.5–7.4% low, F-37); dependent and survivor totals every year; December 2025 benefit levels; the 2025 award distributions; the retroactive-payment loading (2024). Nothing is fitted to cost, income, or the actuarial balance.
- **Review fixes applied**: this version reflects the fixes from the model's comprehensive review (DECISIONS.md F-34, results in F-35), among them computation years, December-rate award tables, the DI history at 65–66 and conversions, the age-66 claim split and the insured simulation. They moved the 75-year balance from −4.41% to −4.45% of payroll.
- **Departures from OCACT** where its inputs are unpublished: one interest rate for both funds after 2035 (2026–2035 yields by fund implied by IV.A); railroad interchange after 2035 held at its 2035 share; benefit payment timing at mid-year; dependent linkages from December 2025 averages, held constant.

## Sources

- [2026 OASDI Trustees Report and its tables](https://www.ssa.gov/oact/TR/2026/) (IV.A1–A2, IV.B1, IV.B4–B6, IV.B8, V.A2–A4, V.C1–C5, VI.G1)
- [Annual Statistical Supplement, 2026](https://www.ssa.gov/policy/docs/statcomps/supplement/2026/) and earlier editions (sections 4.B, 4.C, 5.A–5.G, 6.A–6.D)
- Long-Range OASDI Projection Methodology, 2026 TR (OCACT)
- Model documentation: [MODEL_DOCUMENTATION.md](MODEL_DOCUMENTATION.md)
- Model repository: github.com/anthonycolavito/ss-lr-model (scripts 01–30; choices and findings in docs/DECISIONS.md; comparison data in `outputs/tr_comparison.csv`, `benefits_vs_tr.csv`, `trust_fund_vs_tr.csv`)
