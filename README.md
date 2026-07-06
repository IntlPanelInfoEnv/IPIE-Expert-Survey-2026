# Trends in the Information Environment: 2026 Expert Survey Results (SR2026.2)

Replication materials for the IPIE 2026 Expert Survey report.

## Contents

- `Questionnaire.pdf` — the full survey instrument as fielded (6 May–8 June 2026).
- `Data and code/Data.xlsx` — survey responses (N = 509 rows; the analytic sample is the 470 respondents with Progress > 90). Timers, display orders, bot-detection fields, open-text answers, and contact suggestions were removed to protect respondents' anonymity.
- `Data and code/Script.R` — descriptives, all statistics reported in the text, the latent class analyses, and Figures 2 to 8.
- `Data and code/Script_Map.R` — Figure 1 (map of countries of expertise).
- `Figures/` — all figures as they appear in the report (pdf + png).

## How to reproduce

Set the working directory to `Data and code/` and run `Script.R`, then `Script_Map.R`. Figures are written to `Figures/`, the regions table (Table 1) to `table_regions.docx`, and package versions to `sessionInfo.txt`.

Required R packages: rio, tidyverse, officer, flextable, poLCA (Script.R); rio, tidyverse, sf, rnaturalearth, rnaturalearthdata, ggthemes (Script_Map.R). Run with R 4.3.

## Notes

- Statistics from earlier waves (2023–2025) cited in the report come from the corresponding IPIE reports and their replication materials (SR2023.3, SR2024.2, SR2025.2).
- The country-specific "expert block" questions are analysed in a separate IPIE report; only the five items quoted in SR2026.2 are included here.