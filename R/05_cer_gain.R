# ============================================================================
# 05_cer_gain.R
#
# Purpose: Compute the CER (certainty equivalent return) utility gain for
#          each combination forecast. Measures how much an investor would
#          pay to use the combination forecast instead of the historical avg.
#
# Requires: objects from 02_forecast.R and 03_combine.R
#           (data, r_actual, f_avg, f_mean, f_median, f_trimmed,
#            f_dmspe10, f_dmspe09, oos_start)
# ==========================================================

gamma <- 3 # risk aversion
n_oos <- length(r_actual) #164

#--- Estimate the variance at each OOS quarter
# At each quarter t, estimate sigma^2 using all equity premiums available
# up to that point (expanding window, same idea as the forecasts)

sigma2 <- numeric(n_oos)

for(j in seq_len(n_oos)) {
  s<- oos_start + j -1                            # row we are forecasting
  past_returns <- data$equity_premium[1:(s-1)]    # all returns before this quarter
  sigma2[j] <- var(past_returns)                  # sample variance
}

# --- Portfolio weight function ------------------------------
# Given a forecast and a variance estimate, compute the capped weight

portfolio_weight <- function(forecast, sigma2, gamma, w_min = 0, w_max=1.5) {
  w <- (1 / gamma) * (forecast / sigma2)
  pmin(pmax(w, w_min), w_max)     # cap between 0 and 1.5
}

#--- Compute portfolio returns for a given forecast

portfolio_return <- function(forecast, sigma2, r_actual, gamma) {
  w <- portfolio_weight(forecast, sigma2, gamma)
  # Portfolio return = w * stock return + (1-w) * risk-free
  # equity premium = stock return - risk-free, so:
  # portfolio excess return = w * equity_premium
  w * r_actual
}

#--- CER function

cer <- function(port_returns, gamma) {
  mu <- mean(port_returns)
  s2 <- var(port_returns)
  mu - (gamma / 2) * s2
}

#--- compute CER gains for each combination

# benchmark portfolio (using historical avg forecast)
port_bench <- portfolio_return(f_avg, sigma2, r_actual, gamma)
cer_bench <- cer(port_bench, gamma)

combo_list <- list(
  Mean      = f_mean,
  Median    = f_median,
  Trimmed   = f_trimmed,
  DMSPE_1.0 = f_dmspe10,
  DMSPE_0.9 = f_dmspe09
)

cat("CER utility gain (annualized, %), gamma =", gamma, "\n")
cat("OOS period: 1965:Q1 - 2005:Q4\n\n")
cat(sprintf("%-12s %8s %8s %8s\n", "Combo", "CER_combo", "CER_bench", "Delta"))
cat(strrep("-", 50), "\n")

for (name in names(combo_list)) {
  port_combo <- portfolio_return(combo_list[[name]], sigma2, r_actual, gamma)
  cer_combo <- cer(port_combo, gamma)

  # delta = difference, annualized (mult. quarterly by 4)
  delta <- (cer_combo - cer_bench) * 4 * 100    #mult. by 100 for percent

  cat(sprintf("%-12s %8.3f %8.3f %8.2f\n",
              name,
              cer_combo * 4 * 100,
              cer_bench * 4 * 100,
              delta))
}

