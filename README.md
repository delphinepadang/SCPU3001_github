# Licensing Synroc abroad: decision model and figures

Code and data behind the SCPU3001 Group Project Report for ANSTO, *Where should ANSTO license Synroc first?* The report screens eight jurisdictions with a two-stage multi-criteria decision analysis (MCDA), tests the ranking for robustness, and uses a Monte Carlo cost model to check whether Synroc beats cementation for each waste stream.

The interactive version for ANSTO staff is at
<https://delphinepadang.github.io/Licensing_Synroc_ANSTO/docs/index.html#questions>
(source: <https://github.com/delphinepadang/Licensing_Synroc_ANSTO>).

## Quick start

```r
# R 4.2 or later
install.packages(c("ggplot2", "patchwork", "scales", "ragg", "rsvg"))
# open SCPU3001_synroc.Rproj in RStudio, then:
source("run_all.R")
```

This rebuilds every result table in `output/tables/` and Figures 1–8 in `output/figures/` in a few seconds. Random draws use seed 2026, so results repeat exactly.

## What is in the folder

```
run_all.R                 runs everything below, in order
R/00_setup.R              packages, paths, report colours, shared figure theme
R/01_mcda.R               Stage 1 screen, Stage 2 weighted sum, SMAA-2, sensitivity tests
R/02_cost_model.R         Monte Carlo cost model (20,000 scenarios) + check against published results
R/03_figures.R            Figures 3–8
R/04_diagrams.R           Figures 1–2, rendered from the SVG files in diagrams/
data/                     every input, as plain CSV (one row per jurisdiction, risk, case, ...)
diagrams/                 editable SVG sources for Figures 1 and 2
output/tables/            CSV results quoted in the report and Appendix B
output/figures/           PNG figures as used in the report
```

| Report item | Script | Output |
|---|---|---|
| Figure 1 Research design | `04_diagrams.R` | `figure1_research_design.png` |
| Figure 2 Treatment routes | `04_diagrams.R` | `figure2_treatment_routes.png` |
| Figure 3 Cost curves | `03_figures.R` | `figure3_cost_curves.png` |
| Figure 4 Savings by stream | `03_figures.R` | `figure4_savings.png` |
| Figure 5 Decision matrix | `03_figures.R` | `figure5_decision_matrix.png` |
| Figure 6 Robustness tests | `01_mcda.R`, `03_figures.R` | `figure6_robustness.png` |
| Figure 7 Risk matrix | `03_figures.R` (titles from `data/risks.csv` = Appendix A) | `figure7_risk_matrix.png` |
| Figure 8 Roadmap | `03_figures.R` | `figure8_roadmap.png` |
| Table B2, SMAA and sensitivity (Appendix B) | `01_mcda.R` | `smaa_weights.csv`, `score_perturbation.csv`, `threshold_south_africa.csv`, `stress_thresholds.csv` |
| Licence break-even volumes | `02_cost_model.R` | `licence_breakeven.csv` |

## Decision model (`01_mcda.R`)

* **Stage 1, screen.** Each jurisdiction must pass three gates that no strength elsewhere can offset: waste chemistry suits Synroc, the law admits a Synroc wasteform, and annual volume reaches the licence break-even. Jurisdictions that cannot yet be tested are logged as evidence gaps instead of being guessed.
* **Stage 2, rank.** Survivors get 1–5 scores on four strands (technical, economic, law and politics, social licence) and a weighted sum V = Σ wⱼ xⱼ.
* **Robustness.** SMAA-2 draws 10,000 weight vectors from a flat Dirichlet(1,1,1,1); a ranking counts as robust if it holds in at least 75% of draws (threshold fixed before the analysis). The script also perturbs every score by ±0.5 and ±1 point, tries each possible economic score for South Africa, and finds how far each volume estimate can fall before it drops below break-even.

Main results with the published inputs: the Netherlands ranks first in 100% of weightings (it dominates Belgium on every strand); it stays first in 97.1% of draws with ±0.5-point score error and 87.1% with ±1 point.

## Cost model (`02_cost_model.R`): please read

* **Figures 3 and 4 plot the recorded outputs of the economics strand's original model** (`data/cost_model_results.csv`, `data/cost_model_curves.csv`), so they match the report exactly.
* `02_cost_model.R` is a **re-implementation** of that model's documented structure (annualised plant cost with a 0.614 scale exponent, a triangular licence share, lognormal treatment uncertainty and a discounted avoidable back-end charge), using the inputs listed in `data/cost_engine_*.csv`. It is not the original code.
* `output/tables/cost_model_comparison.csv` compares the re-run with the published results case by case. Every case keeps the same sign and the same verdict (for example, the Netherlands licence: median A$2.70m vs 3.11m per m³; Synroc cheaper in 96% vs 97% of scenarios). Medians differ by up to about A$0.8m per m³ because the original input distributions were not all recorded.

## Using it with ANSTO's own data

Public data cannot settle the questions that matter most: measured waste volumes, agency tariffs and plant costs are confidential. That is why the model is built from editable CSVs:

1. Replace values in `data/stage2_scores.csv`, `data/cost_engine_cases.csv` or `data/cost_engine_globals.csv`.
2. Run `source("run_all.R")`.
3. Read the new ranking, robustness shares and break-even volumes in `output/tables/`.

Nothing is sent anywhere; the scripts only read and write files in this folder.

## Authors and AI use

SCPU3001 student team, University of Sydney, 2026, for ANSTO Synroc. Code was written with help from Claude (Anthropic); the team checked every output against the report. See the report's Appendix D for the full AI statement.
