# =============================================================================
# run_all.R  —  rebuild every number and figure in the SCPU3001 Synroc report
# -----------------------------------------------------------------------------
# Open SCPU3001_synroc.Rproj in RStudio (or setwd() to this folder) and run:
#     source("run_all.R")
# Outputs:
#     output/tables/   CSV results quoted in the report and Appendix B
#     output/figures/  Figures 1–8 as PNG (2,500 px wide)
# Runtime: well under a minute on a laptop.
# =============================================================================

t0 <- Sys.time()
source("R/00_setup.R")       # packages, palette, theme, helpers
source("R/01_mcda.R")        # Stage 1 screen, Stage 2 scores, SMAA, sensitivity
source("R/02_cost_model.R")  # Monte Carlo cost model + comparison with published results
source("R/03_figures.R")     # Figures 3–8
source("R/04_diagrams.R")    # Figures 1–2 (rendered from diagrams/*.svg)

message(sprintf("\nDone in %.0f s. Tables: %s  Figures: %s",
                as.numeric(difftime(Sys.time(), t0, units = "secs")),
                dir_tables, dir_figures))
