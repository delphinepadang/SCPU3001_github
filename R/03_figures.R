# =============================================================================
# 03_figures.R  —  Figures 3 to 8 of the report
# -----------------------------------------------------------------------------
# Needs the objects from 01_mcda.R (gap, threshold) and the data files.
#   Figure 3  Synroc cost against avoidable back-end cost   (cost model results)
#   Figure 4  Saving and probability Synroc is cheaper      (cost model results)
#   Figure 5  Decision matrix                               (screen and scores)
#   Figure 6  Robustness of the ranking                     (01_mcda.R)
#   Figure 7  Risk matrix                                   (risk register)
#   Figure 8  Implementation roadmap                        (roadmap)
# =============================================================================

base <- 9   # point size at the printed width of 6.25 in

# ---- Figure 3: cost curves --------------------------------------------------
curves <- read_data("cost_model_curves.csv")
avoid  <- read_data("avoidable_costs.csv")
vgrid  <- 10^seq(log10(0.3), log10(300), length.out = 400)
curve_df <- do.call(rbind, lapply(seq_len(nrow(curves)), function(i) {
  cv <- curves[i, ]
  med <- cv$annual_cost_coef * pmax(vgrid, cv$min_volume)^cv$scale_exponent / vgrid
  data.frame(model = cv$model, V = vgrid, med = med,
             lo = med * cv$band_p10, hi = pmin(med * cv$band_p90, 25))
}))
curve_df$model <- factor(curve_df$model, c("plant", "licence"),
                         c("Dedicated plant", "Licence (existing shielded cells)"))
lic_cv <- curves[curves$model == "licence", ]
be <- sapply(avoid$avoidable_cost_median, function(a)
  uniroot(function(V) lic_cv$annual_cost_coef * pmax(V, lic_cv$min_volume)^lic_cv$scale_exponent / V - a,
          c(0.05, 5000))$root)
avoid$be <- be
avoid$col <- unlist(pal[avoid$colour_key])
avoid$be_lab <- ifelse(be < 2, sprintf("%.1f", be), paste0("≈", round(be)))
avoid$lab <- sprintf("%s\navoidable A$%sm per m³", avoid$rules,
                     formatC(avoid$avoidable_cost_median, format = "fg"))

fig3 <- ggplot(curve_df, aes(V)) +
  annotate("rect", xmin = 0.7, xmax = 4.0, ymin = 0.2, ymax = 25, fill = pal$pale) +
  annotate("text", x = sqrt(0.7 * 4), y = 19, label = "One producer’s\nannual output",
           family = FONT, size = base / .pt, colour = pal$accent, fontface = "bold", lineheight = 0.9) +
  geom_ribbon(aes(ymin = lo, ymax = hi, group = model), fill = pal$ink, alpha = 0.12) +
  geom_line(aes(y = med, linetype = model), colour = pal$ink, linewidth = 0.8) +
  geom_hline(data = avoid, aes(yintercept = avoidable_cost_median), colour = avoid$col, linewidth = 0.8) +
  geom_point(data = avoid, aes(x = be, y = avoidable_cost_median), colour = "white",
             fill = avoid$col, shape = 21, size = 2.8, stroke = 0.8) +
  geom_text(data = avoid, aes(x = be * 1.12, y = avoidable_cost_median * 1.12, label = be_lab),
            colour = avoid$col, family = FONT, fontface = "bold", size = base / .pt, hjust = 0) +
  geom_text(data = avoid, aes(x = 340, y = avoidable_cost_median, label = lab), colour = avoid$col,
            family = FONT, size = (base - 0.5) / .pt, hjust = 0, lineheight = 0.95) +
  annotate("segment", x = 2.6, xend = 15, y = 0.27, yend = 0.27, colour = pal$ink2,
           arrow = arrow(ends = "both", length = unit(4, "pt"), type = "closed")) +
  annotate("text", x = sqrt(2.6 * 15), y = 0.32, label = "Whole world’s output per year",
           family = FONT, size = (base - 0.5) / .pt, colour = pal$ink2) +
  scale_x_log10(breaks = c(0.3, 1, 3, 10, 30, 100, 300), labels = c("0.3", "1", "3", "10", "30", "100", "300"),
                expand = c(0, 0)) +
  scale_y_log10(breaks = c(0.2, 0.5, 1, 2, 5, 10, 20), labels = c("0.2", "0.5", "1", "2", "5", "10", "20"), expand = c(0, 0)) +
  scale_linetype_manual(values = c("solid", "22")) +
  coord_cartesian(xlim = c(0.3, 300), ylim = c(0.2, 25), clip = "off") +
  labs(x = "Waste treated per year (m³ of raw Mo-99 liquid, log scale)",
       y = "A$ million per m³ of raw waste\n(present value, 2025; log scale)",
       caption = "Line = median; band = 10th–90th percentile. Synroc pays wherever a line lies above the cost curve;\ndots mark the licence break-even volume (m³ a year).") +
  theme_report(base) +
  theme(plot.margin = margin(8, 118, 6, 6), legend.margin = margin(0, 0, 0, 0),
        plot.caption = element_text(hjust = 0, colour = pal$ink2, face = "italic", size = base - 1),
        plot.caption.position = "plot")
save_fig(fig3, "figure3_cost_curves.png", height = 4.1)

# ---- Figure 4: savings by case ----------------------------------------------
res <- read_data("cost_model_results.csv")
case_col <- c("Netherlands, Mo-99 liquid" = pal$NL, "Belgium, Mo-99 liquid" = pal$BE,
              "UK, resins and sludges" = pal$UK, "Canada, resins and sludges" = pal$CA,
              "Dutch waste, UK rules" = pal$UK, "Dutch waste, Canadian rules" = pal$CA)
order_cases <- names(case_col)
group_lab <- c(target = "TARGET STREAMS", other = "OTHER STREAMS",
               test = "TEST: DUTCH WASTE, OTHER RULES")
# y positions: each case has two rows (plant above licence), groups separated by gaps
ypos <- c(); y <- 0; heads <- c()
for (gname in c("target", "other", "test")) {
  y <- y - 1.1; heads[gname] <- y
  for (cn in unique(res$case[res$group == gname])) { y <- y - 1.2; ypos[cn] <- y }
  y <- y - 0.4
}
res$y <- ypos[res$case] + ifelse(res$model == "plant", 0.22, -0.22)
res$col <- case_col[res$case]
res$plab <- ifelse(res$p_cheaper < 0.01, "<1%", paste0(round(100 * res$p_cheaper), "%"))
res$bold <- ifelse(res$p_cheaper >= THRESHOLD, "bold", "plain")
res$px <- ifelse(res$model == "plant", 9.6, 11.6)

fig4 <- ggplot(res) +
  annotate("rect", xmin = 0, xmax = 8, ymin = min(res$y) - 0.6, ymax = max(heads) + 0.4, fill = "#f2f8f6") +
  annotate("segment", x = seq(-6, 8, 2), xend = seq(-6, 8, 2), y = min(res$y) - 0.6, yend = max(heads) + 0.4,
           colour = pal$grid, linewidth = 0.3) +
  annotate("segment", x = 0, xend = 0, y = min(res$y) - 0.6, yend = max(heads) + 0.4,
           colour = pal$ink, linewidth = 0.6) +
  geom_segment(aes(x = p10_saving, xend = p90_saving, y = y, yend = y), colour = res$col, linewidth = 0.9) +
  geom_point(data = res[res$model == "plant", ], aes(median_saving, y), shape = 21, fill = "white",
             colour = res$col[res$model == "plant"], size = 2.3, stroke = 1) +
  geom_point(data = res[res$model == "licence", ], aes(median_saving, y), shape = 15,
             colour = res$col[res$model == "licence"], size = 2.4) +
  geom_text(aes(x = px, y = y, label = plab, fontface = bold), colour = res$col, family = FONT,
            size = base / .pt) +
  annotate("text", x = c(9.6, 11.6), y = max(heads) + 1.1, label = c("Plant", "Licence"),
           family = FONT, size = base / .pt, colour = pal$ink2) +
  annotate("text", x = 10.6, y = max(heads) + 1.9, label = "Synroc cheaper in", fontface = "bold",
           family = FONT, size = base / .pt, colour = pal$ink) +
  annotate("text", x = -0.3, y = max(heads) + 1.1, label = "← Cement cheaper", hjust = 1,
           family = FONT, size = (base - 0.5) / .pt, colour = pal$ink2) +
  annotate("text", x = 0.3, y = max(heads) + 1.1, label = "Synroc cheaper →", hjust = 0,
           family = FONT, size = (base - 0.5) / .pt, colour = pal$ink2) +
  annotate("text", x = -12.8, y = heads, label = group_lab, hjust = 0, family = FONT, fontface = "bold",
           size = (base - 1) / .pt, colour = pal$accent) +
  annotate("text", x = -6.6, y = ypos, label = names(ypos), hjust = 1, family = FONT,
           size = base / .pt, colour = pal$ink) +
  scale_x_continuous(breaks = seq(-6, 8, 2), labels = function(x) sub("-", "−", x)) +
  coord_cartesian(xlim = c(-6, 8), ylim = c(min(res$y) - 0.6, max(heads) + 2.0), clip = "off") +
  labs(x = "Saving from Synroc over cementation (A$ million per m³ of raw waste, present value)", y = NULL,
       caption = "○ dedicated plant      ■ licence      Marker = median; bar = 10th–90th percentile of 20,000 scenarios.") +
  theme_report(base) +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(6, 92, 6, 150),
        plot.caption = element_text(hjust = 0.5, colour = pal$ink2, size = base - 0.5))
save_fig(fig4, "figure4_savings.png", height = 3.9)

# ---- Figure 5: decision matrix ---------------------------------------------
sm <- read_data("screen_matrix.csv")
sm$row <- factor(sm$label, rev(sm$label))
s1 <- data.frame(row = rep(sm$row, 4),
                 col = factor(rep(c("Chemistry", "Legal\nadmissibility", "Volume vs\nbreak-even", "Outcome"),
                                  each = nrow(sm)),
                              c("Chemistry", "Legal\nadmissibility", "Volume vs\nbreak-even", "Outcome")),
                 txt = c(sm$chemistry, sm$legal_admissibility, sm$volume_vs_breakeven, sm$stage1_outcome))
s1$fill <- ifelse(s1$txt %in% c("Provisional", "Pass", "Proceeds") | grepl(" vs ", s1$txt), "pass",
           ifelse(s1$txt %in% c("Excluded", "No stream") | grepl("^Needs", s1$txt), "fail", "open"))
s1$face <- ifelse(s1$col == "Outcome", "bold", "plain")
s1$lab <- sub(" vs ", "\nvs ", s1$txt)
s1$lab <- ifelse(nchar(s1$lab) > 10 & !grepl("\n", s1$lab), sub(" ", "\n", s1$lab), s1$lab)
p5a <- ggplot(s1, aes(col, row)) +
  geom_tile(aes(fill = fill), colour = pal$border, width = 0.96, height = 0.9, linewidth = 0.3) +
  geom_text(aes(label = lab, fontface = face), family = FONT, size = (base - 1.5) / .pt, colour = pal$ink, lineheight = 0.85) +
  scale_fill_manual(values = c(pass = pal$mint, fail = pal$rose, open = "white"), guide = "none") +
  scale_x_discrete(position = "top") +
  labs(title = "Stage 1 · Screen out deal-breakers", x = NULL, y = NULL) +
  theme_report(base) +
  theme(panel.grid = element_blank(), axis.ticks = element_blank(),
        axis.text.x.top = element_text(size = base - 1.5, lineheight = 0.9, colour = pal$ink2),
        axis.text.y = element_text(size = base - 0.5, colour = pal$ink, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = base + 0.5))
s2 <- data.frame(row = rep(sm$row, 4),
                 col = factor(rep(c("Tech-\nnical", "Eco-\nnomic", "Law and\npolitics", "Social\nlicence"), each = nrow(sm)),
                              c("Tech-\nnical", "Eco-\nnomic", "Law and\npolitics", "Social\nlicence")),
                 val = c(sm$technical, sm$economic, sm$law_politics, sm$social))
s2$val[is.na(s2$val)] <- ""
s2$key <- ifelse(s2$val == "", "none", as.character(s2$val))
excluded <- sm$row[sm$stage1_outcome == "Excluded"]
s2$fill <- ifelse(s2$key == "none", "white", pal$scores[s2$key])
s2$fill[s2$row %in% excluded & s2$key != "none"] <- scales::alpha(s2$fill[s2$row %in% excluded & s2$key != "none"], 0.35)
s2$txtcol <- ifelse(s2$key %in% c("3.5", "4", "5") & !(s2$row %in% excluded), "white", pal$ink)
s2$lab <- ifelse(s2$val == "", "–", s2$val)
p5b <- ggplot(s2, aes(col, row)) +
  geom_tile(fill = s2$fill, colour = pal$border, width = 0.94, height = 0.9, linewidth = 0.3) +
  geom_text(aes(label = lab), colour = s2$txtcol, family = FONT, size = (base - 0.5) / .pt) +
  scale_x_discrete(position = "top") +
  labs(title = "Stage 2 · Scores (1–5)", x = NULL, y = NULL) +
  theme_report(base) +
  theme(panel.grid = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        axis.text.x.top = element_text(size = base - 1.5, lineheight = 0.9, colour = pal$ink2),
        plot.title = element_text(hjust = 0.5, size = base + 0.5))
fig5 <- p5a + p5b + plot_layout(widths = c(4, 3))
save_fig(fig5, "figure5_decision_matrix.png", height = 3.4)

# ---- Figure 6: robustness ----------------------------------------------------
p6a <- ggplot(data.frame(gap = gap), aes(gap)) +
  geom_histogram(binwidth = 0.02, boundary = 0, fill = pal$scores[["3"]], colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, colour = pal$ink, linewidth = 0.7) +
  annotate("text", x = 0.02, y = Inf, vjust = 1.3, hjust = 0, family = FONT, size = (base - 1) / .pt, colour = pal$ink,
           label = sprintf("%s of %s weightings put\nthe Netherlands first",
                           format(sum(gap > 0), big.mark = ","), format(length(gap), big.mark = ","))) +
  scale_x_continuous(limits = c(-0.02, 1.02), breaks = seq(0, 1, 0.25)) +
  labs(title = "(a) Weights", x = "Netherlands’ score minus Belgium’s\n(below 0 would put Belgium first)",
       y = "Number of weightings") +
  theme_report(base)
th <- data.frame(score = threshold$assumed_economic_score, share = threshold$outranks_belgium)
never_nl <- all(threshold$outranks_netherlands == 0)
p6b <- ggplot(th, aes(factor(score), share)) +
  geom_col(width = 0.6, fill = pal$scores[["3"]]) +
  geom_text(aes(label = percent(share, 1)), vjust = -0.4,
            family = FONT, size = (base - 1) / .pt, colour = pal$ink) +
  annotate("text", x = 0.6, y = 0.98, hjust = 0, vjust = 1, family = FONT, size = (base - 1) / .pt,
           colour = pal$ink, lineheight = 0.95,
           label = paste0("Bars: share of weightings in which\nSouth Africa outranks Belgium.",
                          if (never_nl) "\nIt never outranks the Netherlands." else "")) +
  scale_y_continuous(labels = percent, limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  labs(title = "(b) Missing data", x = "Economic score assumed for South Africa", y = "Share of weightings") +
  theme_report(base) + theme(panel.grid.major.x = element_blank())
vol <- data.frame(alt = factor(c("Netherlands\n(estimate, range)", "Belgium\n(1985–96 record)"),
                               c("Belgium\n(1985–96 record)", "Netherlands\n(estimate, range)")),
                  lo = c(0.7, 3.2), hi = c(4.0, 3.2), pt = c(0.7, 3.2), be = c(0.5, 1.0),
                  note = c(sprintf("0.7 m³/yr: survives only a %s fall (%s at 4.0 m³/yr)",
                                   percent(stress$fall_in_volume[1], 1), percent(stress$fall_in_volume[2], 1)),
                           sprintf("3.2 m³/yr: survives a %s fall", percent(stress$fall_in_volume[3], 1))))
p6c <- ggplot(vol, aes(y = alt)) +
  geom_segment(aes(x = lo, xend = hi, yend = alt), colour = pal$scores[["2"]], linewidth = 5) +
  geom_segment(aes(x = be, xend = be, y = as.numeric(alt) - 0.28, yend = as.numeric(alt) + 0.28),
               colour = pal$ink, linewidth = 1) +
  geom_text(aes(x = be + 0.05, y = as.numeric(alt) - 0.38, label = paste("break-even", sprintf("%.1f", be))),
            hjust = 0, family = FONT, size = (base - 1.5) / .pt, colour = pal$ink2) +
  geom_point(aes(x = pt), shape = 21, fill = pal$NL, colour = "white", size = 3, stroke = 0.8) +
  geom_text(aes(x = 5, y = as.numeric(alt) + 0.36, label = note), hjust = 1, family = FONT,
            size = (base - 1.5) / .pt, colour = pal$ink) +
  scale_x_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(title = "(c) Volume", x = "Raw liquid ILW, m³ per year (licensing model)", y = NULL) +
  theme_report(base) + theme(panel.grid.major.y = element_blank())
fig6 <- (p6a | p6b) / p6c + plot_layout(heights = c(1.5, 1))
save_fig(fig6, "figure6_robustness.png", height = 4.9)

# ---- Figure 7: risk matrix ---------------------------------------------------
# Risk titles are read from data/risks.csv, the same wording as Appendix A.
risks <- read_data("risks.csv")
band_of <- function(sev) ifelse(sev >= 6, "High", ifelse(sev >= 3, "Medium", "Low"))
band_fill <- c(Low = "#f1f5f4", Medium = "#a2d3c6", High = pal$accent)
cells <- expand.grid(L = 1:3, I = 1:3)
cells$band <- band_of(cells$L * cells$I)
cells$ids <- mapply(function(l, i) {
  ids <- risks$id[risks$inherent_likelihood == l & risks$inherent_impact == i]
  if (length(ids) == 0) "" else paste(strwrap(paste(ids, collapse = " "), 7), collapse = "\n")
}, cells$L, cells$I)
cells$txtcol <- ifelse(cells$band == "High", "white", pal$ink)
p7a <- ggplot(cells, aes(I, L)) +
  geom_tile(aes(fill = band), colour = "white", linewidth = 1.2) +
  geom_text(aes(label = ids), colour = cells$txtcol, family = FONT, fontface = "bold",
            size = (base - 0.5) / .pt, lineheight = 0.95) +
  scale_fill_manual(values = band_fill, breaks = c("High", "Medium", "Low"), name = "Inherent rating") +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "Medium", "High"), expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:3, labels = c("Low", "Medium", "High"), expand = c(0, 0)) +
  coord_equal() +
  labs(x = "Impact on the recommendation", y = "Likelihood before G3") +
  theme_report(base) +
  theme(panel.grid = element_blank(), axis.ticks = element_blank(), legend.position = "bottom",
        legend.title = element_text(size = base - 1), legend.text = element_text(size = base - 1),
        legend.key.size = unit(8, "pt"))
leg <- data.frame(id = risks$id, lab = risks$risk, k = seq_len(nrow(risks)),
                  band = band_of(risks$inherent_likelihood * risks$inherent_impact))
leg$y <- -leg$k
p7b <- ggplot(leg) +
  geom_tile(aes(0, y, fill = band), width = 0.22, height = 0.7, colour = pal$border, linewidth = 0.2) +
  geom_text(aes(0.25, y, label = id), hjust = 0, fontface = "bold", family = FONT,
            size = (base - 1.5) / .pt, colour = pal$ink) +
  geom_text(aes(0.75, y, label = lab), hjust = 0, family = FONT, size = (base - 1.5) / .pt, colour = pal$ink) +
  scale_fill_manual(values = band_fill, guide = "none") +
  scale_x_continuous(limits = c(-0.15, 8.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-nrow(leg) - 0.6, -0.4), expand = c(0, 0)) +
  theme_void()
fig7 <- p7a + p7b + plot_layout(widths = c(1, 1.75))
save_fig(fig7, "figure7_risk_matrix.png", height = 3.3)

# ---- Figure 8: implementation roadmap ----------------------------------------
road <- read_data("roadmap.csv")
lanes <- c("Government", "Technical", "Regulatory", "Social licence", "Economic", "ANSTO plant", "Commercial")
road$lane <- factor(road$lane, rev(lanes))
lane_fill <- c(Government = "#e1e9f5", Technical = pal$scores[["3"]], Regulatory = "#a9d8c9",
               "Social licence" = "#c9e7d5", Economic = "#c6e4ec", "ANSTO plant" = "#e4e9ec", Commercial = "#5aa6c2")
road$fill <- lane_fill[as.character(road$lane)]
road$fill[road$task == "Sign licences; first operations"] <- pal$accent
road$txt <- ifelse(road$fill %in% c(pal$accent, "#5aa6c2"), "white", pal$ink)
road$y <- as.numeric(road$lane)
chars_per_year <- 30            # wrap each label to the length of its bar
road$wrapped <- mapply(function(s, w) paste(strwrap(s, max(12, floor(w * chars_per_year))), collapse = "\n"),
                       road$task, road$end - road$start)
gates <- road[road$gate_at_end != "", ]
gate_band <- c(2028.6, 2028.7)
fig8 <- ggplot(road) +
  annotate("rect", xmin = gate_band[1], xmax = gate_band[2], ymin = 0.45, ymax = 7.55, fill = "#f6e3d6") +
  annotate("text", x = mean(gate_band), y = 7.85, label = "Licences signed only after G1–G3", fontface = "bold",
           family = FONT, size = (base - 0.5) / .pt, colour = pal$ink) +
  geom_vline(xintercept = 2030, linetype = "22", colour = pal$ink2) +
  annotate("text", x = 2029.96, y = 2.0, label = "PALLAS expected\nto replace HFR\n(2030)", hjust = 1,
           family = FONT, size = (base - 1.5) / .pt, colour = pal$ink2, lineheight = 0.9) +
  geom_rect(aes(xmin = start, xmax = end, ymin = y - 0.38, ymax = y + 0.38), fill = road$fill,
            colour = "white", linewidth = 0.6) +
  geom_text(aes(x = (start + end) / 2, y = y, label = wrapped), colour = road$txt, family = FONT,
            size = (base - 2) / .pt, lineheight = 0.88) +
  geom_point(data = gates, aes(end, y), shape = 23, size = 3, fill = pal$gate, colour = "white", stroke = 0.8) +
  geom_text(data = gates, aes(gate_band[2] + 0.05, y, label = gate_at_end), hjust = 0, family = FONT,
            fontface = "bold", size = (base - 1.5) / .pt, colour = pal$gate) +
  scale_x_continuous(breaks = 2027:2030, limits = c(2026.8, 2030.08), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq_along(levels(road$lane)), labels = levels(road$lane), limits = c(0.45, 8.1),
                     expand = c(0, 0)) +
  labs(x = "Indicative timing (calendar year)", y = NULL) +
  theme_report(base) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank(),
        axis.text.y = element_text(face = "bold", colour = pal$ink))
save_fig(fig8, "figure8_roadmap.png", height = 3.7)
