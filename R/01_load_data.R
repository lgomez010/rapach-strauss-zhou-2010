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