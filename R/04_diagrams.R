# 04_diagrams.R -------------------------------------------------------------
# Figures 1 and 2 are diagrams, not data plots. Their sources are the SVG files
# in diagrams/ (edit them in any text editor or in Inkscape/Illustrator); this
# script renders them to PNG at print resolution so every report figure comes
# out of one run.
#
# Fonts: the SVGs ask for Carlito, then Calibri. If neither is installed the
# renderer falls back to a sans-serif font and line breaks may shift slightly.

diagrams <- c(
  "figure1_research_design.svg"  = "figure1_research_design.png",
  "figure2_treatment_routes.svg" = "figure2_treatment_routes.png"
)

for (src in names(diagrams)) {
  in_path  <- file.path("diagrams", src)
  out_path <- file.path("output", "figures", diagrams[[src]])
  if (!file.exists(in_path)) stop("Missing diagram source: ", in_path)
  # 2,500 px wide matches the width of the R-drawn figures (6.25 in at 400 dpi)
  rsvg::rsvg_png(in_path, out_path, width = 2500)
  message("Wrote ", out_path)
}
