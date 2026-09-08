# ============================================================================
# 03_combine.R
#
# Purpose: Build combination forecasts from the 15 individual predictor
#          forecasts produced by 02_forecast.R. Compute R^2_OS for each
#          combination scheme vs. the historical average benchmark.
#
# Requires: objects from 02_forecast.R (f_pred, f_avg, r_actual)
# ============================================================================

# --- Simple combination forecasts -------------------------------------------
# Each row of f_pred is one OOS quarter, with 15 predictor forecasts.
# We collapse each row to a single number using different rules.

n_oos <- length(r_actual)   # 164

# 1. Mean: average all 15 forecasts
f_mean <- rowMeans(f_pred)

# 2. Median: middle value of the 15 forecasts
f_median <- apply(f_pred, 1, median)

# 3. Trimmed mean: drop the highest and lowest, average the remaining 13
f_trimmed <- apply(f_pred, 1, function(row) {
  sorted <- sort(row)
  mean(sorted[2:14]) # drop the first and last (lowest and highest)
})

cat("Simple combos computed. First 5 values:\n")
cat("Mean:    ", round(f_mean[1:5], 5), "\n")
cat("Median:  ", round(f_median[1:5], 5), "\n")
cat("Trimmed: ", round(f_trimmed[1:5], 5), "\n")

# --- DMSPE combination forecasts -----------------------------------------

# DMSPE (Dynamic Mean Squared Prediction Error) combination
# We will use the holdout period to compute the DMSPE for each combination.

holdout_start <- which(data$yyyyq == 19551)   # row 33
n_holdout <- oos_start - holdout_start         # 40 quarters

cat("\nHoldout period: row", holdout_start, "to row", oos_start - 1, "\n")
cat("Holdout length:", n_holdout, "quarters\n")

# Generate forecasts for the holdout period (rows 33 to 72)
# Same expanding-window OLS as in 02_forecast.R
f_holdout <- matrix(NA, nrow = n_holdout, ncol = length(predictors))
colnames(f_holdout) <- predictors
r_holdout <- numeric(n_holdout)

for (j in seq_len(n_holdout)) {
  s <- holdout_start + j - 1   # row we're forecasting (33 to 72)
  train <- data[1:(s - 1), ]
  r_holdout[j] <- data$equity_premium[s]

  for (i in seq_along(predictors)) {
    p <- predictors[i]
    y <- train$equity_premium[2:(s - 1)]
    x <- train[[p]][1:(s - 2)]
    fit <- lm(y ~ x)
    x_now <- data[[p]][s - 1]
    f_holdout[j, i] <- coef(fit)[1] + coef(fit)[2] * x_now
  }
}

cat("Holdout forecasts computed.\n")

# --- Compute DMSPE combination forecasts ------------------------------

# Stack all forecasts: holdout (40 rows) then OOS (164 rows) = 204 rows
all_forecasts <- rbind(f_holdout, f_pred)
all_actuals   <- c(r_holdout, r_actual)

# Squared errors for every predictor at every quarter: 204 x 15 matrix
all_errors_sq <- (all_actuals - all_forecasts)^2

# Now compute DMSPE combination forecasts for each OOS quarter
f_dmspe10 <- numeric(n_oos)   # theta = 1.0
f_dmspe09 <- numeric(n_oos)   # theta = 0.9

for (j in seq_len(n_oos)) {
  # At OOS quarter j, we have errors from the holdout (rows 1:40)
  # plus any previous OOS quarters (rows 41 through 40+j-1).
  # Total track record length: 40 + (j - 1) = 39 + j rows.
  track_end <- n_holdout + j - 1   # last row of track record

  # --- theta = 1.0 (no discounting) ---
  # phi_i = sum of all squared errors so far for predictor i
  phi10 <- colSums(all_errors_sq[1:track_end, , drop = FALSE])

  # Weight = (1/phi) / sum(1/phi)
  inv_phi10 <- 1 / phi10
  w10 <- inv_phi10 / sum(inv_phi10)

  # Weighted average of the 15 forecasts for this quarter
  f_dmspe10[j] <- sum(w10 * f_pred[j, ])

  # --- theta = 0.9 (discount older errors) ---
  # Multiply each row's errors by theta^(distance from most recent)
  distances <- (track_end:1) - 1   # 0 for most recent, 1 for one before, etc.
  discount_weights <- 0.9^distances

  # phi_i = sum of discounted squared errors
  phi09 <- colSums(discount_weights * all_errors_sq[1:track_end, , drop = FALSE])

  inv_phi09 <- 1 / phi09
  w09 <- inv_phi09 / sum(inv_phi09)

  f_dmspe09[j] <- sum(w09 * f_pred[j, ])
}

cat("DMSPE combos computed. First 5 values:\n")
cat("DMSPE 1.0:", round(f_dmspe10[1:5], 5), "\n")
cat("DMSPE 0.9:", round(f_dmspe09[1:5], 5), "\n")

# --- R^2_OS for combination forecasts ----------------------------------------
denom <- sum((r_actual - f_avg)^2)

combo_names <- c("Mean", "Median", "Trimmed", "DMSPE_1.0", "DMSPE_0.9")
combo_forecasts <- cbind(f_mean, f_median, f_trimmed, f_dmspe10, f_dmspe09)

r2os_combo <- numeric(length(combo_names))
names(r2os_combo) <- combo_names

for (i in seq_along(combo_names)) {
  numer <- sum((r_actual - combo_forecasts[, i])^2)
  r2os_combo[i] <- 1 - numer / denom
}

cat("\nR^2_OS (%) for combination forecasts, 1965:Q1 - 2005:Q4:\n")
print(round(r2os_combo * 100, 2))