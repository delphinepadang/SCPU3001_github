# =============================================================================
# 00_setup.R  —  packages, paths, colours and the shared figure theme
# -----------------------------------------------------------------------------
# Sourced by run_all.R. Every other script assumes the objects defined here.
# =============================================================================

required <- c("ggplot2", "patchwork", "scales", "ragg", "rsvg")
missing  <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Install these packages first:\n  install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), "))", call. = FALSE)
}
suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(scales)
})

# ---- Paths (relative to the project root; open the .Rproj or setwd() there) --
dir_data    <- "data"
dir_tables  <- file.path("output", "tables")
dir_figures <- file.path("output", "figures")
dir.create(dir_tables,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_figures, recursive = TRUE, showWarnings = FALSE)

read_data <- function(name) {
  utils::read.csv(file.path(dir_data, name), stringsAsFactors = FALSE,
                  check.names = FALSE, encoding = "UTF-8")
}
write_table <- function(df, name) {
  utils::write.csv(df, file.path(dir_tables, name), row.names = FALSE)
}

# ---- Reproducibility ---------------------------------------------------------
SEED       <- 2026      # used by every random draw in the project
N_WEIGHTS  <- 10000     # SMAA weight draws
THRESHOLD  <- 0.75      # robustness threshold, fixed before the analysis

# ---- Report palette (matches the written report) -----------------------------
pal <- list(
  ink      = "#26363e",  # main text
  ink2     = "#4f5f68",  # secondary text
  grid     = "#e3e9ec",
  border   = "#c9d3d8",
  accent   = "#3f7f99",  # deep teal: headings, highlights
  mint     = "#bfe3d6",  # pass / proceeds
  rose     = "#ece7e4",  # excluded
  pale     = "#eef6f8",
  NL       = "#2f6f8a",
  BE       = "#5aa6c2",
  UK       = "#c08a3e",
  CA       = "#a8695f",
  gate     = "#c0844a",
  scores   = c("1" = "#f4f8f9", "2" = "#c6e4ec", "2.5" = "#a9d3e1", "3" = "#8cc3d6",
               "3.5" = "#73b1c8", "4" = "#5a9fba", "5" = "#3f7f99")
)

# ---- Font: Carlito (metric twin of Calibri, the report's body font) ---------
pick_font <- function() {
  fams <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character())
  for (f in c("Carlito", "Calibri", "Helvetica Neue", "Arial")) if (f %in% fams) return(f)
  "sans"
}
FONT <- pick_font()

theme_report <- function(base = 12) {
  theme_minimal(base_size = base, base_family = FONT) +
    theme(
      text             = element_text(colour = pal$ink),
      axis.text        = element_text(colour = pal$ink2),
      axis.title       = element_text(colour = pal$ink),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = pal$grid, linewidth = 0.4),
      plot.title       = element_text(face = "bold", size = base + 1, hjust = 0),
      plot.title.position = "plot",
      legend.position  = "bottom",
      legend.title     = element_blank(),
      plot.margin      = margin(8, 12, 8, 8)
    )
}

# Save at print resolution; width in inches matches the report column (6.25 in)
save_fig <- function(plot, name, width = 6.25, height = 4, dpi = 400) {
  path <- file.path(dir_figures, name)
  ggsave(path, plot, width = width, height = height, dpi = dpi,
         device = ragg::agg_png, bg = "white")
  message("  wrote ", path)
  invisible(path)
}
