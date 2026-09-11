#===================================================================
#04_clark_west.R
#
# Purpose: Compute Clark-West (2007) MSPE-adjusted statistic for
#          each of combination forecasts and test whether the combination
#          beats the historical benchmark.
#
# Requires: objects from 02_forecast.R and 03_combine.R:
#           r_actual, f_avg, f_mean, f_median, f_trimmed, f_dmspe10, f_dmspe09
#   ===================================================================

#---------Clark-West test function
#Inputs:
#   actual  - vector of realized equity premiums
#   f_bench - vector of benchmark forecasts (historical average length T)
#   f_alt   - vector of alternative (combination) forecasts length T
#
# Output: a list with the CW statistic and one-sided p-value


clark_west <- function(actual, f_bench, f_alt) {

  e_bench_sq <- (actual - f_bench)^2    # benchmark squared errors
  e_alt_sq <- (actual - f_alt)^2        # combination squared errors
  correction <- (f_bench - f_alt)^2     # handicap correction

  fhat <- e_bench_sq - e_alt_sq + correction

  # regress fhat on a constant - the t-stat on the intercept is out CW stat
  reg <- lm(fhat ~ 1)
  cw_stat <- summary(reg)$coefficients[1, "t value"]

  # one sided p value (we only care if combination beats benchmark)
  p_value <- 1 - pnorm(cw_stat)

  list(cw = cw_stat, p = p_value)
}
# apply to all 5 combination forecasts
combo_list <- list(
  Mean      = f_mean,
  Median    = f_median,
  Trimmed   = f_trimmed,
  DMSPE_1.0 = f_dmspe10,
  DMSPE_0.9 = f_dmspe09
)

cat("Clark-West MSPE-adjusted statistics, 1965:q1 - 2005:Q4\n")
cat("(one-sided test: H0 = benchmark is correct)\n\n")
cat(sprintf("%-12s  %8s %8s %s\n", "Combo", "CW stat", "p-value", "Sig"))
cat(strrep("-", 50), "\n")

for (name in names(combo_list)) {
  result <- clark_west(r_actual, f_avg, combo_list[[name]])

  stars <- ""
  if (result$p < 0.01) stars <- "***"
  else if (result$p < 0.05) stars <- "**"
  else if (result$p < 0.10) stars <- "*"

  cat(sprintf("%-12s  %8.3f %8.4f %s\n", name, result$cw, result$p, stars))
}
