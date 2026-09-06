#===================================================================
# 02_forecast.R
#
# Purpose: Forecast the equity premium using 
#           (a) the historcial average benchmark
#           (b) 15 indivisual predictive regressions
#           (c) Compute R^2_OS for each predictor vs, the benchmark
#
#Input: data/processed/predictor.rds
#Output: (to be determined) - forecast results)
#===================================================================

library(dplyr)

# --- load data
data <- readRDS("data/processed/predictors.rds")

#--- define the sample split for out-of-sample forecasting
#--- paper uses 1947:Q1-2005:Q4 (236 rows, already in 01_load_data.R
#--- out-od-sample period starts 1965:Q1
#--- this means the inital in-sample window is 1947:Q1-1964:Q4 = 72 quarters

oos_start <- which(data$yyyyq == 19651) # row index where OOS geins
cat("OOS starts at row:", oos_start, "\n")
cat("Initital in-sample size:", oos_start - 1, "quarters\n")
cat("Out-of-Sample size:", nrow(data) - oos_start + 1, "quarters\n")

#--- predictor names
predictors <- c("DP", "DY", "EP", "DE", "SVAR", "BM", "NTIS",
                "TBL", "LTY", "LTR", "TMS", "DFY", "DFR",
                "INFL", "IK")

# --- prepare storage
# we need to store forecast for each OOS quarter
# n_oos = number of out-of-sample quarters
n_oos <- nrow(data) - oos_start + 1     # 164 total

# vectors to hold historical average forecast and actual return
f_avg <- numeric(n_oos)       # historcial average forecast
r_actual <- numeric(n_oos)    # realized equity premium

# matrix to hold indiviual predictor forecasts: 164 rows x 15 columns
f_pred <- matrix(NA, nrow = n_oos, ncol = length(predictors))
colnames(f_pred) <- predictors

#--- expanding-window loop
# At each OOS quarter s, we:
#  1. use rows 1:(s-1) as training data
#. 2. row s-1 has the predictor values x_t we plug into the forecast
#. 3. row s has the realized return r_{t+1} we want to forecast
# 
# we use s-1 because the regression is r_{t+1} = alpha + beta * x_t
# and the training pairs are (x_t,r_{t+1}) for t = [1:(s-2)]
# so the forecast uses x_{s-1} to predict r_s

for (j in seq_len(n_oos)) {
  # s = row index of the quarter we are forecasting
  s <- oos_start + j - 1      # s runs from 73 to 236

  # training data: rows 1 thru s-1
  train <- data[1:(s-1), ]

  # historcial average forecast
  # means of all equity premiums observed so far
  f_avg[j] <- mean(train$equity_premium)

  # realized return for this quarter
  r_actual[j] <- data$equity_premium[s]

  # individual predictor forecasts
  for(i in seq_along(predictors)) {
    p <- predictors[i]

    #build training vectors:equity_premium
    #   y = equity premium from period 2 to s-1.  (the r_{t+1}'s)
    #   x = predictor values from period 1 to s-2.  (the x_t's)
    y <- train$equity_premium[2:(s-1)]
    x <- train[[p]][1:(s-2)]

    # OLS regression: r_{t+1} = alpha + beta * x_t
    fit <- lm(y ~ x)

    #forecast: plug in the current (most recent) predictor value
    x_now <- data[[p]][s-1]
    f_pred[j,i] <- coef(fit)[1] + coef(fit)[2] * x_now
  }
}

cat("Loop complete.", n_oos, "quarters forecasted.\n")


# --- Compute R²_OS for each predictor --------------------------------------
# R²_OS = 1 - sum((r - f_pred)^2) / sum((r - f_avg)^2)
#
# Numerator: squared forecast errors from the predictor model
# Denominator: squared forecast errors from the historical average
# Positive = predictor beats the benchmark

denom <- sum((r_actual - f_avg)^2)

r2os <- numeric(length(predictors))
names(r2os) <- predictors

for (i in seq_along(predictors)) {
  numer <- sum((r_actual - f_pred[, predictors[i]])^2)
  r2os[i] <- 1 - numer / denom
}

# display as precentage, sorted best to worst
cat("\nR^2_OS (%), 1965:Q1 - 2005:Q4:\n")
print(sort(round(r2os * 100, 2), decreasing = TRUE))