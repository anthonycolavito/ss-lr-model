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

The one rule: anything in `data/` or `outputs/` can be rebuilt from `data-raw/` and `params/` by running `scripts/` in order.

## Status

Phase 0: data and parameters.
