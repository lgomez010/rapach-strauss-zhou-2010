#-------------------------
# 07_ct_restriction.R
# Campbell-Thompson (2005) restriction: clip negative forescast to 0
# Requires: objects from 02_forecast.R in memory
#----------------------

# f_pred is our 164 x 15 matrix of individual predictor forecasts
# some entries are negative (predicting stockes lose to T-bills)
# pmax() is "parallel max" - it compares each element to 0 and
# keeps whichever is larger. Works element-wise on a whole matrix.

f_pred_ct <- pmax(f_pred, 0)

# check how many forecast are negative before clipping
cat("Negative forecasts in original f_pred:", sum(f_pred < 0), "\n")
cat("Total forecast entries:", length(f_pred), "\n")
cat("Percentage negative:", round(100 * sum(f_pred < 0) / length(f_pred), 1), "%\n")

#----------------
# mean combination using CT-clipped forecasts
#-------------------

# rowMeans() averages across columns for each row
# each row is one quarter, each column is one predictor
# so this gives a 164-vector: the average CT-clipped forecast per quarter

f_mean_ct <- rowMeans(f_pred_ct)

# R-squared OS: measures how much better (positive) or worse (negative)
# our forecast is compared to the historical average benchmark
# formula: 1 - (sum of our squared errors / sum of benchmark squared errors)

r2os_mean_ct <- (1 - sum((r_actual - f_mean_ct)^2) / sum((r_actual - f_avg)^2)) * 100

cat("R-squared OS for Mean,CT:", round(r2os_mean_ct, 2), "%\n")
cat("R-squared OS for Mean (no CT):", round(
  (1 - sum((r_actual - f_mean)^2) / sum((r_actual - f_avg)^2)) * 100, 2), "%\n")

#-------------------
# clark-West test and CER gain for Mean, CT
#------------------

# Clark-West test: is Mean, CT significantly better than the historical average?
# clark_west() was defined in 04_clark_west.R and is needed in memory
# it returns the test statistic; values above ~1.65 / 2.33 / 3.09
# correspond to signifance at 10% / 5% / 1%

cw_mean_ct <- clark_west(r_actual, f_avg, f_mean_ct)
cat("Clark_West stat for Mean,CT:", round(cw_mean_ct$cw, 3), "\n")

# CER gain: how much would a mean-variance inverstor pay (as an annual fee)
# to use Mean, CT forecasts instead of the histrocial average?
# portfolio_weight(), portfolio_return(), and cer() come from 05_cer_gain.R
# gamma = 3 (risk aversion), weights capped at [0, 1.5]

w_ct      <- portfolio_weight(f_mean_ct, sigma2, gamma = 3)
w_bench   <- portfolio_weight(f_avg, sigma2, gamma = 3)
ret_ct    <- portfolio_return(f_mean_ct, sigma2, r_actual, gamma = 3)
ret_bench <- portfolio_return(f_avg, sigma2, r_actual, gamma = 3)
cer_ct    <- cer(ret_ct, gamma = 3)
cer_bench <- cer(ret_bench, gamma = 3)

# we also need the benchmark CER for comparison
delta_ct  <- (cer_ct - cer_bench) * 400 # *4 for quarterly -> annual, *100 for percentage

cat("CER gain (annualized %) for Mean,CT:", round(delta_ct, 2), "%\n")
