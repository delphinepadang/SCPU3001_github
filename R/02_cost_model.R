# =============================================================================
# 02_cost_model.R  —  Monte Carlo cost model (report Section 3.3, Appendix B6)
# -----------------------------------------------------------------------------
# PURPOSE
#   A transparent, re-runnable version of the economic model. ANSTO can replace
#   the public inputs in data/cost_engine_globals.csv and data/cost_engine_cases.csv
#   with its own confidential figures (measured volumes, tariffs, plant costs)
#   and re-run this script; nothing leaves the computer.
#
# STATUS (read this)
#   The published Figures 3 and 4 plot the recorded outputs of the economics
#   strand's original model (data/cost_model_results.csv, data/cost_model_curves.csv).
#   This script re-implements that model's documented structure with the input
#   assumptions listed in the CSV files. With the default inputs it reproduces the
#   published results in direction and magnitude; output/tables/cost_model_comparison.csv
#   lists the differences case by case. It is a re-implementation, not the original code.
#
# MODEL, per cubic metre of raw liquid waste, in 2025 A$ million (present value)
#   Treatment (annualised):
#     C_plant(V) = (CRF * capex_ref + opex_ref) * (max(V, min_scale) / capacity_ref)^b
#     cost_plant = C_plant(V) / V * treatment multiplier (lognormal)
#     cost_licence = cost_plant * licence share (triangular, 35–70%)
#   Avoidable back-end cost (what Synroc's smaller package saves):
#     A = (cement_ratio - synroc_volume) * charge * discount factor
#     discount factor = (1 + r)^-delay  (delay = 0 where disposal is paid on hand-over)
#   Saving from Synroc over cementation = A - treatment cost
# =============================================================================

g     <- read_data("cost_engine_globals.csv")
par   <- setNames(as.numeric(g$value), g$parameter)
cases <- read_data("cost_engine_cases.csv")

rtri <- function(n, a, m, b) {                   # triangular draw
  if (a == b) return(rep(a, n))
  u <- runif(n); c <- (m - a) / (b - a)
  ifelse(u < c, a + sqrt(u * (b - a) * (m - a)), b - sqrt((1 - u) * (b - a) * (b - m)))
}
crf <- function(r, n) r / (1 - (1 + r)^-n)       # capital recovery factor

annual_plant_cost <- function(V) {
  (crf(par["discount_rate"], par["plant_life"]) * par["capex_ref"] + par["opex_ref"]) *
    (pmax(V, par["min_scale"]) / par["capacity_ref"])^par["scale_exponent"]
}

run_case <- function(row, n = par["n_draws"]) {
  set.seed(par["seed"])
  V      <- rtri(n, row$volume_min, row$volume_mode, row$volume_max)
  mult   <- exp(rnorm(n, 0, par["treatment_cost_sdlog"]))
  share  <- rtri(n, par["licence_share_min"], par["licence_share_mode"], par["licence_share_max"])
  vsyn   <- runif(n, par["synroc_volume_min"], par["synroc_volume_max"])
  charge <- row$charge_median * exp(rnorm(n, 0, row$charge_sdlog))
  delay  <- runif(n, row$delay_min, row$delay_max)
  A      <- (row$cement_ratio - vsyn) * charge * (1 + par["discount_rate"])^-delay
  plant  <- annual_plant_cost(V) / V * mult
  lic    <- plant * share
  data.frame(case = row$case, group = row$group, avoidable = A,
             saving_plant = A - plant, saving_licence = A - lic)
}

sims <- do.call(rbind, lapply(seq_len(nrow(cases)), function(i) run_case(cases[i, ])))

summ <- function(x) c(median = median(x), p10 = unname(quantile(x, 0.1)),
                      p90 = unname(quantile(x, 0.9)), p_cheaper = mean(x > 0))
engine <- do.call(rbind, lapply(split(sims, sims$case), function(d) {
  rbind(data.frame(case = d$case[1], model = "plant",   t(summ(d$saving_plant))),
        data.frame(case = d$case[1], model = "licence", t(summ(d$saving_licence))))
}))
engine$avoidable_median <- ave(seq_len(nrow(engine)), engine$case, FUN = function(i)
  median(sims$avoidable[sims$case == engine$case[i[1]]]))
write_table(engine, "cost_model_rerun.csv")

# ---- Compare with the published results ------------------------------------
published <- read_data("cost_model_results.csv")
cmp <- merge(published, engine, by = c("case", "model"), suffixes = c("_published", "_rerun"))
cmp <- data.frame(case = cmp$case, model = cmp$model,
                  median_published = cmp$median_saving, median_rerun = round(cmp$median, 2),
                  p_cheaper_published = cmp$p_cheaper_published,
                  p_cheaper_rerun = round(cmp$p_cheaper_rerun, 3))
write_table(cmp, "cost_model_comparison.csv")

# ---- Belgium at its documented volume (Appendix B6) -------------------------
# The published Belgian case used 4 m3 a year; IRE's documented output is 3.2 m3
# a year (IAEA-TECDOC-1051). Re-run the licence case at both volumes.
be_row <- cases[cases$case == "Belgium, Mo-99 liquid", ]
be_doc <- do.call(rbind, lapply(c(4.0, 3.2), function(v) {
  r <- be_row; r$volume_min <- v; r$volume_mode <- v; r$volume_max <- v
  data.frame(volume_m3 = v, t(summ(run_case(r)$saving_licence)))
}))
write_table(be_doc, "belgium_documented_volume.csv")
message(sprintf("  Belgium licence at 3.2 m3/yr: median A$%.2fm, cheaper in %.0f%%",
                be_doc$median[2], 100 * be_doc$p_cheaper[2]))

# ---- Licence break-even volumes from the published curves (Figure 3) --------
curves <- read_data("cost_model_curves.csv")
avoid  <- read_data("avoidable_costs.csv")
lic    <- curves[curves$model == "licence", ]
cost_per_m3 <- function(V, cv) cv$annual_cost_coef * pmax(V, cv$min_volume)^cv$scale_exponent / V
breakeven <- data.frame(rules = avoid$rules, avoidable_cost = avoid$avoidable_cost_median,
  licence_breakeven_m3 = sapply(avoid$avoidable_cost_median, function(a)
    uniroot(function(V) cost_per_m3(V, lic) - a, c(0.05, 5000))$root))
write_table(breakeven, "licence_breakeven.csv")

message("  Licence break-even (m3/yr): ",
        paste(sprintf("%s %.1f", breakeven$rules, breakeven$licence_breakeven_m3), collapse = "; "))
