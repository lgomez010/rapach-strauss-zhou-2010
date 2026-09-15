#==================
# 06_figure2.R - Cumulative Squared-Error Difference (CSSED) plot
#
# Reproduces figure 2 of Rapach, Strauss, and Zhou 
# Requires: objects from 02_forecast.R and 03_combine.R in memory
#
# What this plots:
#   CSSED_t = cumcum( e^2_bench - e^2_combo )
#   where e = r_actual - forecast
#   upward slope = combination forecast is beating the historical average
#=====================

#-------Block 1: compute CSSED for each combination method

# Squared forecast errors for the benchmark (historical average)
e2_bench <- (r_actual - f_avg)^2
e2_mean     <- (r_actual - f_mean)^2
e2_median   <- (r_actual - f_median)^2
e2_trimmed  <- (r_actual - f_trimmed)^2
e2_dmspe10  <- (r_actual - f_dmspe10)^2
e2_dmspe09  <- (r_actual - f_dmspe09)^2

# CSSED = cumulative sum of (benchmark error - combo error)
# cumsum() computes a running total of a vector
cssed_mean    <- cumsum(e2_bench - e2_mean)
cssed_median  <- cumsum(e2_bench - e2_median)
cssed_trimmed <- cumsum(e2_bench - e2_trimmed)
cssed_dmspe10 <- cumsum(e2_bench - e2_dmspe10)
cssed_dmspe09 <- cumsum(e2_bench - e2_dmspe09)


#--- build the time axis
# convert yyyyq dates to decimal years for plotting
# e.g. 1965 Q1 = 1965.0, 1965 Q2 = 1965.25, etc.
oos_rows <- data[oos_start:nrow(data), ]
time_axis <- oos_rows$year + (oos_rows$quarter -1) / 4

#--- plot helper function
# draws one CSSE panel, call it 5 times
plot_cssed <- function(cssed, title) { plot(time_axis, cssed, type = "l", lwd = 1.5, col = "black", xlab = "", ylab = "CSSED", main = title, ylim = c(-0.05, 0.12)); abline(h = 0, lty = 2, col = "gray40") }

pdf("output/figure2_cssed.pdf", width = 10, height = 6)
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
plot_cssed(cssed_mean, "Mean")
plot_cssed(cssed_median, "Median")
plot_cssed(cssed_trimmed, "Trimmed mean")
plot_cssed(cssed_dmspe10, "DMSPE, theta=1.0")
plot_cssed(cssed_dmspe09, "DMSPE, theta=0.9")
mtext("Figure 2: Cumulative Squared-Error Difference, 1965:Q1 - 2005:Q4", outer = TRUE, cex = 1.1)
dev.off()