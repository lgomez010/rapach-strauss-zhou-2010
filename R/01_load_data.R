# ============================================================================
# 01_load_data.R
#
# Purpose: Load Goyal-Welch predictor data, construct the 15 predictors
#          and the equity premium, subset to the paper's sample period
#          (1947:Q1 - 2005:Q4), and save a clean data frame for downstream
#          analysis.
#
# Input:   data/raw/PredictorData2024.xlsx  (Quarterly sheet)
# Output:  data/processed/predictors.rds
# ============================================================================

# --- Libraries --------------------------------------------------------------
library(readxl)
library(dplyr)

# --- Read raw data ----------------------------------------------------------
raw <- read_excel("data/raw/PredictorData2024.xlsx", sheet = "Quarterly")

# --- Parse dates and coerce types ------------------------------------------
# The yyyyq column packs year and quarter into a single integer:
#   19471 = 1947 Q1,  20054 = 2005 Q4.
# Columns below arrived as character because Goyal encodes missing values
# as the literal string "NaN" in early rows.
chr_cols <- c("b/m", "tbl", "AAA", "BAA", "lty", "cay", "ntis",
              "Rfree", "infl", "ltr", "corpr", "svar", "csp",
              "ik", "CRSP_SPvw", "CRSP_SPvwx", "D3", "E3")

data <- raw %>%
  mutate(
    year    = yyyyq %/% 10,
    quarter = yyyyq %%  10,
    month   = quarter * 3,
    date    = as.Date(sprintf("%d-%02d-01", year, month)),
    across(all_of(chr_cols), as.numeric)
  )

data <- data %>%
  arrange(date) %>%
  mutate(
    #---- Valuation ratios (logs)
    DP = log(D12) - log(Index),         # Dividend-price ratio
    DY = log(D12) - log(lag(Index)),    # Dividend yield
    EP = log(E12) - log(Index),         # Earnings-price ratio
    DE = log(D12) - log(E12),           # Dividend-payout ratio
    #----- Volatility / equity market predictors
    SVAR = svar,                        # Stock variance
    BM = `b/m`,                         # Book-to-market ratio
    NTIS = ntis,                        # Net equity issuance
    # --- Interest rates
    TBL = tbl,                          # Treasury bill rate (3 month)
    LTY = lty,                          # Long-term yield (10 year)
    LTR = ltr,                          # Long-term return (10 year)
    TMS = lty - tbl,                    # Term spread
    # --- credit spreads
    DFY = BAA - AAA,                    # Default yield spread
    DFR = corpr - ltr,                  # Default return spread
    # --- Macro predictors
    INFL = infl,                        # Inflation (lagged 1 quarter)
    IK = ik,                            # Investment-to-capital ratio
    # ---- Equity premium
    equity_premium = log(1 + CRSP_SPvw) - log(1 + Rfree)    # log excess return
  )

 # subset to the paper's sample period (1947:Q1 - 2005:Q4) : Rapach-Strauss-Zhou 

data <- data %>%
  filter(yyyyq >=19471, yyyyq <= 20054)

# --- verify that the data frame has the expected number of rows and columns
cat("Rows:", nrow(data), "\n")
cat("NAs per predictor: \n")
colSums(is.na(data[, c("DP", "DY", "EP", "DE", "SVAR", "BM", "NTIS",
                        "TBL", "LTY", "LTR", "TMS", "DFY", "DFR",
                        "INFL", "IK", "equity_premium")]))

# --- save cleaned data
saveRDS(data, "data/processed/predictors.rds")
cat("Saved:", nrow(data), "rows to data/processed/predictors.rds\n")
