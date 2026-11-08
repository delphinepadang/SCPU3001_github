# =============================================================================
# 01_mcda.R  —  the two-stage decision tool (report Sections 3.6–3.7, Appendix B)
# -----------------------------------------------------------------------------
# Stage 1: non-compensatory screen (chemistry, legal admissibility, volume).
# Stage 2: weighted sum of 1–5 scores, Equation (1): V(a_i) = sum_j w_j x_ij.
# Robustness: dominance check, SMAA-2 with uniform Dirichlet weights,
#             score-perturbation test, threshold analysis for South Africa,
#             and stress thresholds for the volume screen.
# Outputs: CSV tables in output/tables and the objects used by 03_figures.R.
# =============================================================================

scores <- read_data("stage2_scores.csv")
crit   <- c("technical", "economic", "law_politics", "social")

# ---- Stage 1 summary -----------------------------------------------------------
screen <- read_data("screen_matrix.csv")
write_table(screen[, c("label", "chemistry", "legal_admissibility",
                       "volume_vs_breakeven", "stage1_outcome")],
            "stage1_screen.csv")

survivors <- scores[scores$id %in% c("NL", "BE"), ]
X <- as.matrix(survivors[, crit]); rownames(X) <- survivors$id

# ---- Dominance ---------------------------------------------------------------
gap_vector   <- X["NL", ] - X["BE", ]          # (1, 1, 0.5, 0)
nl_dominates <- all(gap_vector >= 0) && any(gap_vector > 0)

# ---- Random weights: SMAA-2, w ~ Dirichlet(1, 1, 1, 1) ----------------------
rdirichlet1 <- function(n, k) { g <- matrix(rgamma(n * k, shape = 1), ncol = k); g / rowSums(g) }

set.seed(SEED)
W   <- rdirichlet1(N_WEIGHTS, length(crit))
V   <- W %*% t(X)                              # weighted values, one row per draw
gap <- as.vector(W %*% gap_vector)             # V(NL) - V(BE)
first_nl <- mean(V[, "NL"] > V[, "BE"] | (V[, "NL"] == V[, "BE"] & runif(N_WEIGHTS) < 0.5))

closest <- order(gap)[1:100]
smaa <- data.frame(
  measure = c("Netherlands ranked first (share of draws)", "Gap minimum", "Gap median",
              "Gap maximum", "Mean social weight in the 100 smallest gaps",
              "Robustness threshold", "Netherlands dominates Belgium"),
  value   = c(first_nl, min(gap), median(gap), max(gap), mean(W[closest, 4]),
              THRESHOLD, nl_dominates)
)
write_table(smaa, "smaa_weights.csv")

# ---- Score perturbation: every score +/- e (uniform), alongside uniform weights -
perturb <- function(e) {
  set.seed(SEED)
  Wp <- rdirichlet1(N_WEIGHTS, length(crit))
  E1 <- matrix(runif(N_WEIGHTS * 4, -e, e), ncol = 4)
  E2 <- matrix(runif(N_WEIGHTS * 4, -e, e), ncol = 4)
  v1 <- rowSums(Wp * (matrix(X["NL", ], N_WEIGHTS, 4, byrow = TRUE) + E1))
  v2 <- rowSums(Wp * (matrix(X["BE", ], N_WEIGHTS, 4, byrow = TRUE) + E2))
  mean(v1 > v2)
}
perturbation <- data.frame(score_error = c("±0.5", "±1.0"),
                           netherlands_first = c(perturb(0.5), perturb(1.0)))
write_table(perturbation, "score_perturbation.csv")

# ---- Threshold analysis: South Africa's missing economic score (Table B2) ----
za <- unlist(scores[scores$id == "ZA", crit])
threshold <- do.call(rbind, lapply(1:5, function(s) {
  x <- za; x["economic"] <- s
  set.seed(SEED)
  Wt <- rdirichlet1(N_WEIGHTS, length(crit))
  vz <- Wt %*% x
  data.frame(assumed_economic_score = s,
             outranks_belgium        = mean(vz > Wt %*% X["BE", ]),
             outranks_netherlands    = mean(vz > Wt %*% X["NL", ]))
}))
write_table(threshold, "threshold_south_africa.csv")

# ---- Stress thresholds for the volume screen (Table B3) ---------------------
# Licensee cost = fixed F + proportional cost, so break-even volume B = F / a.
stress_row <- function(label, V, B) {
  data.frame(alternative = label,
             fall_in_volume = 1 - B / V,
             cut_in_avoidable_cost = 1 - B / V,
             fixed_cost_overrun = V / B - 1)
}
stress <- rbind(stress_row("Netherlands, 0.7 m3/yr (estimate)",       0.7, 0.5),
                stress_row("Netherlands, 4.0 m3/yr (upper estimate)", 4.0, 0.5),
                stress_row("Belgium, 3.2 m3/yr (1985-96 record)",     3.2, 1.0))
write_table(stress, "stress_thresholds.csv")

# ---- Console summary -----------------------------------------------------------
message(sprintf("  Netherlands first in %.1f%% of %d weightings (dominance: %s)",
                100 * first_nl, N_WEIGHTS, nl_dominates))
message(sprintf("  Gap V(NL) - V(BE): min %.2f, median %.2f, max %.2f",
                min(gap), median(gap), max(gap)))
message(sprintf("  Score perturbation: %.1f%% (±0.5), %.1f%% (±1.0)",
                100 * perturbation$netherlands_first[1], 100 * perturbation$netherlands_first[2]))
message("  South Africa outranks Belgium: ",
        paste0(round(100 * threshold$outranks_belgium), "%", collapse = ", "))
