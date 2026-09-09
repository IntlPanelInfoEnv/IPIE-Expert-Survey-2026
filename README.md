# Trends in the Information Environment: 2026 Expert Survey Results (SR2026.3)

Replication materials for the IPIE 2026 Expert Survey report.

## Contents

- `Questionnaire.pdf` — the survey instrument as fielded (6 May–8 June 2026).
- `Data and code/Data.xlsx` — survey responses (N = 509 rows; the analytic sample is the 470 respondents with Progress > 90). Timers, display orders, bot-detection fields, open-text answers and contact suggestions were removed to protect respondents' anonymity.
- `Figures/` — every figure in the report (pdf + png), plus Figure A1.

## Code

| Script | What it produces | Needs |
|---|---|---|
| `Script.R` | The statistics reported in the text, both latent class analyses, Figures 2a, 3, 4a, 5, 6, 7a and 8a, and Table 1 | `Data.xlsx` |
| `Script_Map.R` | Figure 1 | `Data.xlsx` |
| `LCA_Typologies.R` | Class selection for the two typologies: BIC, entropy and the bootstrapped likelihood-ratio test of three classes against two | `Data.xlsx` |
| `Figures_Review.R` | Figures 7b, 9 and A1 | `Data.xlsx`, `review_trend.csv` |
| `Figures_Trends.R` | Figures 2b, 4b and 8b | 2024 and 2025 microdata |
| `Review_Analyses.R` | The Wilson intervals and two-proportion tests behind every group comparison the report states, and the outlook series | 2023, 2024 and 2025 microdata |

Set the working directory to `Data and code/`. `Script.R`, `Script_Map.R`, `LCA_Typologies.R` and `Figures_Review.R` run there with no further inputs; figures are written to `Figures/`. Required packages: rio, tidyverse, officer, flextable, poLCA, sf, rnaturalearth, rnaturalearthdata, ggthemes. Run with R 4.3.

### Earlier waves

`Review_Analyses.R` and `Figures_Trends.R` read microdata from earlier waves, which belong to those reports and their own replication packages (SR2023.3, SR2024.2, SR2025.2) and are not redistributed here. Their outputs are included so the results stay inspectable:

- `review_stats.csv` — every group comparison the report states, with Wilson intervals and test statistics.
- `review_trend.csv`, `review_trend_balanced.csv`, `review_trend_us.csv`, `review_trend_economy.csv`, `review_trend_adjusted.csv` — the outlook series and its robustness checks.
- `trend_importance.csv`, `trend_ai_future.csv`, `trend_barriers.csv` — the per-wave estimates behind Figures 2b, 4b and 8b.
- `country_expertise_counts.csv` — the number of experts on each of the 71 countries of expertise.

## Notes

- The report quotes poLCA's estimated class proportions (`$P`), not the share of respondents assigned to each class by modal posterior probability. The two differ (52/37/11 against 55/33/12 for the threat typology), so the distinction matters when comparing output to the text.
- Percentages in the report keep "don't know" in the denominator, so they are shares of all 470 experts. The latent class analyses drop it, which is why they run on 456 and 447 respondents.
- The 2023 outlook figure is recomputed from the 2023 microdata rather than taken from SR2023.3. Following the 2023 team's own filter but without their merge on a country shapefile, which dropped Hong Kong and Singapore, the denominator is 296 rather than the published 289 and the share expecting worsening is 53% rather than 54%.
- Figure A1 gives the class-membership probabilities behind the generative-AI typology. It was cut from the report as a diagnostic and kept here.
- Statistics from earlier waves cited in the report come from SR2023.3, SR2024.2 and SR2025.2.
- The country-specific "expert block" questions are analysed in a separate IPIE report; only the items quoted in SR2026.3 are included here.
