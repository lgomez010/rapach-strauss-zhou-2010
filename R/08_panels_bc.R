#R/08_panels_bc.R
#-------------------
#Panels B and C of table 1
#   Panel B: 1976:Q1-2005:Q4 (120 quarters)
#   Panel C: 2000:Q1-2005:Q4 (24 quarters)
#
# Note: the individual expanding-window forecasts (f_pred, f_avg)
# were already computed for the full 1965:Q1-2005:Q4 OOS period.
# Panels B and C evaluate thos same forecasts over shorter tails.
# The only method that needs re-computation is DMSPE, because its
# weights depend on accumulated forecast errors; and that accumulation
# restarts at the new start date.
#
# Requires in memory: f_avg, f_pred, r_actual, sigma2,
#   clark_west(), portfolio_return(), cer()
# (Source scripts 01 thru 05 first if needed)
#----------------------

#-----DMSPE combination function
# Builds DMSPE-weighted combination forecasts from scratch
#
# Arguments:
#   fp    : T x 15 matrix of individual predictor forecasts
#   r     : T-vector of realized equity premiums
#   theta : discount factor (1.0 = equal weight on all past errors,
#           0.9 = recent errors matter more)
# Returns: T-vector of combination forecasts
#--------------------

dmspe_combine <- function(fp, r, theta) {
  T_oos <- length(r)
  N <- ncol(fp)     # 15 predictors
  f_out <- numeric(T_oos)

  # first quarter: no past errors yet, so use equal q=weights (=mean)
  f_out[1] <- mean(fp[1, ])

  for (t in 2:T_oos) {
    # forescast errors for each predictor at times 1,...,t-1
    errors <- fp[1:(t-1), , drop = FALSE] - r[1:(t-1)]  # (t-1) x N

    # Discount vector: the most recent error gets theta^0 = 1,
    # the oldest gets theta^(t-2). If theta = 1, all errors
    # count equally; if theta = 0.9, older errors get small
    disc <- theta^((t-2):0)     # length t-1

    # Discounted sum of squared errors for each predictor
    dsse <- colSums(disc * errors^2) # N-vector

    w <- (1 / dsse) / sum(1 / dsse)

    # Weighted combination forecast
    f_out[t] <- sum(w * fp[t, ])
  }

  return(f_out)
}

#---- panel specifications
panels <- list(
  B = list(start_idx = 45, label = "1976:Q1-2005:Q4"),
  C = list(start_idx = 141, label = "2000:Q1-2005:Q4")
)

#--- loop over panels

for (pnl in names(panels)) {

  s   <- panels[[pnl]]$start_idx
  idx <- s:164        # indices into our 164-vectors

  # subset the OOS vectors
  r_sub   <- r_actual[idx]
  fa_sub  <- f_avg[idx]
  fp_sub  <- f_pred[idx, ]
  sig_sub <- sigma2[idx]
  T_oos   <- length(r_sub)

  cat("\n====================================\n")
  cat(sprintf(" Panel %s: %s  (%d quarters)\n",
              pnl, panels[[pnl]]$label, T_oos))
  cat("\n====================================\n")

  #--- combination forecasts
  # Mean, median, trimmed mean: pure row-by-row formulas
  f_mean_sub  <- rowMeans(fp_sub)
  f_median_sub <- apply(fp_sub, 1, median)
  f_trimmed_sub <- apply(fp_sub, 1, mean, trim = 0.1)

  # DMSPE: re-accumulate errors from the new start

  f_dmspe10_sub <- dmspe_combine(fp_sub, r_sub, theta = 1.0)
  f_dmspe09_sub <- dmspe_combine(fp_sub, r_sub, theta = 0.9)

  #--- evaluate all combination methods
  e2_bench <- (r_sub - fa_sub)^2    # benchmark squared errors

  fp_ct_sub     <- pmax(fp_sub, 0)
  f_mean_ct_sub <- rowMeans(fp_ct_sub)

  combos <- list(
    "Mean"        = f_mean_sub,
    "Median"      = f_median_sub,
    "Trimmed"     = f_trimmed_sub,
    "DMSPE 1.0"   = f_dmspe10_sub,
    "DMSPE 0.9"   = f_dmspe09_sub,
    "Mean, CT"    = f_mean_ct_sub
  )  
  cat("\n  Method           R^2_OS(%)   CW stat   p-val    CER difference (%)\n")
  cat("  ", strrep("-", 58), "\n")

  for (nm in names(combos)) {
    f_combo  <- combos[[nm]]
    e2_combo <- (r_sub - f_combo)^2

    # R^2_OS: how much lower is the combo's MSPE vs the benchmark?
    r2os <- (1 - sum(e2_combo) / sum(e2_bench)) * 100

    # Clark-West significance test
    cw <- clark_west(r_sub, fa_sub, f_combo)

    # CER utility gain (annualized percentage points)
    cer_combo <- cer(portfolio_return(f_combo, sig_sub, r_sub, gamma = 3), gamma = 3)
    cer_bench <- cer(portfolio_return(fa_sub,  sig_sub, r_sub, gamma = 3), gamma = 3)
    delta <- (cer_combo - cer_bench) * 400

    # Significance stars
    stars <- ifelse(cw$p < 0.01, "***",
             ifelse(cw$p < 0.05, "** ",
             ifelse(cw$p < 0.10, "*  ", "   ")))

    cat(sprintf("  %-14s  %7.2f%s  %7.3f  %6.4f   %7.2f\n",
                nm, r2os, stars, cw$cw, cw$p, delta))
  }       
}

  