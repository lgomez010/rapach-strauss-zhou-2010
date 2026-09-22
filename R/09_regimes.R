# =============================================================================
# 09_regimes.R — Table 5: Regime-dependent R^2_OS
#
# Sorts OOS quarters into "good" / "normal" / "bad" thirds
# based on GDP growth, profit growth, and cash flow growth.
# Computes R^2_OS for each combination method within each regime.
# =============================================================================

# --- Step 1: Download macroeconomic data from FRED ---
gdp_raw <- read.csv(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=GDPC1",
  stringsAsFactors = FALSE
)

profits_raw <- read.csv(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=A446RC1Q027SBEA",
  stringsAsFactors = FALSE
)

cashflow_raw <- read.csv(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=W790RC1Q027SBEA",
  stringsAsFactors = FALSE
)

deflator_raw <- read.csv(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=GDPDEF",
  stringsAsFactors = FALSE
)

cat("Rows — GDP:", nrow(gdp_raw),
    " Profits:", nrow(profits_raw),
    " Cash flow:", nrow(cashflow_raw),
    " Deflator:", nrow(deflator_raw), "\n")

cat("Cash flow starts:", cashflow_raw$observation_date[1], "\n")

# --- Convert FRED dates to our yyyyq format ---
fred_to_yyyyq <- function(date_string) {
  year  <- as.integer(substr(date_string, 1, 4))
  month <- as.integer(substr(date_string, 6, 7))
  quarter <- (month - 1) / 3 + 1
  return(year * 10 + quarter)
}

gdp_df      <- data.frame(yyyyq = fred_to_yyyyq(gdp_raw$observation_date),
                           gdp   = gdp_raw$GDPC1)

profits_df  <- data.frame(yyyyq = fred_to_yyyyq(profits_raw$observation_date),
                           profits_nom = profits_raw$A446RC1Q027SBEA)

cashflow_df <- data.frame(yyyyq = fred_to_yyyyq(cashflow_raw$observation_date),
                           cashflow_nom = cashflow_raw$W790RC1Q027SBEA)

deflator_df <- data.frame(yyyyq = fred_to_yyyyq(deflator_raw$observation_date),
                           deflator = deflator_raw$GDPDEF)

macro <- merge(gdp_df, profits_df, by = "yyyyq")
macro <- merge(macro, cashflow_df, by = "yyyyq")
macro <- merge(macro, deflator_df, by = "yyyyq")

cat("Merged rows:", nrow(macro), "\n")
cat("Range:", macro$yyyyq[1], "to", macro$yyyyq[nrow(macro)], "\n")

# --- Deflate nominal series to real terms ---
macro$profits_real  <- macro$profits_nom  / macro$deflator
macro$cashflow_real <- macro$cashflow_nom / macro$deflator

# --- Compute quarterly growth rates ---
macro$g_gdp      <- c(NA, diff(log(macro$gdp)))
macro$g_profits  <- c(NA, diff(log(macro$profits_real)))
macro$g_cashflow <- c(NA, diff(log(macro$cashflow_real)))

# --- Subset to our OOS period: 1965:Q1 to 2005:Q4 ---
macro_oos <- macro[macro$yyyyq >= 19651 & macro$yyyyq <= 20054, ]

cat("OOS rows:", nrow(macro_oos), "\n")
cat("Any NAs in growth rates?",
    any(is.na(macro_oos$g_gdp)),
    any(is.na(macro_oos$g_profits)),
    any(is.na(macro_oos$g_cashflow)), "\n")

# =============================================================================
# Functions
# =============================================================================

# --- R^2_OS within a subset of quarters ---
r2os_subset <- function(actual, f_bench, f_alt, idx) {
  e_bench <- actual[idx] - f_bench[idx]
  e_alt   <- actual[idx] - f_alt[idx]
  1 - sum(e_alt^2) / sum(e_bench^2)
}

# --- Clark-West test on a subset of quarters ---
cw_subset <- function(actual, f_bench, f_alt, idx) {
  r  <- actual[idx]
  fb <- f_bench[idx]
  fa <- f_alt[idx]
  
  e_bench <- r - fb
  e_alt   <- r - fa
  
  ft <- e_bench^2 - (e_alt^2 - (fb - fa)^2)
  
  n  <- length(ft)
  mu <- mean(ft)
  se <- sd(ft) / sqrt(n)
  cw_stat <- mu / se
  p_value <- 1 - pnorm(cw_stat)
  
  return(p_value)
}

# --- Stars based on p-value ---
stars <- function(p) {
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.10) return("*")
  return("")
}

# --- Regime table: R^2_OS with significance stars ---
regime_table <- function(growth_vec, r_actual, f_avg, combo_list) {
  n <- length(growth_vec)
  
  cuts <- quantile(growth_vec, probs = c(1/3, 2/3))
  
  bad    <- which(growth_vec <= cuts[1])
  good   <- which(growth_vec >  cuts[2])
  normal <- which(growth_vec >  cuts[1] & growth_vec <= cuts[2])
  
  cat("  Regime sizes — Good:", length(good),
      " Normal:", length(normal),
      " Bad:", length(bad), "\n")
  
  results <- matrix("", nrow = length(combo_list), ncol = 4)
  colnames(results) <- c("Overall", "Good", "Normal", "Bad")
  rownames(results) <- names(combo_list)
  
  regimes <- list(Overall = 1:n, Good = good, Normal = normal, Bad = bad)
  
  for (i in seq_along(combo_list)) {
    fc <- combo_list[[i]]
    for (col in names(regimes)) {
      idx  <- regimes[[col]]
      r2   <- r2os_subset(r_actual, f_avg, fc, idx)
      p    <- cw_subset(r_actual, f_avg, fc, idx)
      results[i, col] <- paste0(sprintf("%.2f", r2 * 100), stars(p))
    }
  }
  
  return(noquote(results))
}

# =============================================================================
# Run the three panels
# =============================================================================

combo_list <- list(
  "Mean"        = f_mean,
  "Median"      = f_median,
  "Trimmed"     = f_trimmed,
  "DMSPE 1.0"   = f_dmspe10,
  "DMSPE 0.9"   = f_dmspe09
)

cat("\nPanel A: Sorting on real GDP growth\n")
panel_a <- regime_table(macro_oos$g_gdp, r_actual, f_avg, combo_list)
print(panel_a)

cat("\nPanel B: Sorting on real profit growth\n")
panel_b <- regime_table(macro_oos$g_profits, r_actual, f_avg, combo_list)
print(panel_b)

cat("\nPanel C: Sorting on real net cash flow growth\n")
panel_c <- regime_table(macro_oos$g_cashflow, r_actual, f_avg, combo_list)
print(panel_c)