# 🧹 Clear the environment and set up the script
rm(list = ls())

cat("=====================================\n")
cat(" FIRST TRADES FOR ASTS (flpl2 system)\n")
cat("=====================================\n")

# 📂 Source necessary R scripts
source("backtest_ticker.R")
source("flpl_live_scan.R")

# ⬇️ READ TICKERS FROM FILE (NO HEADER) ⬇️
# header = FALSE tells R that the first row is data, not column names.
ticker_data <- read.csv(
  "Liquid_Stocks_ETFs.csv", 
  header = FALSE, 
  stringsAsFactors = FALSE
)

# Extract the first column as a vector of tickers
# Since there is no header, the first column is named 'V1' by default.
tickers <- as.vector(ticker_data[[1]]) 

cat("\n=====================================\n")
cat(" SCANNING THESE TICKERS:\n")
cat("=====================================\n")

# 📝 Print the list of tickers read from the file
print(tickers)

# 🔍 Run the live scan function
print(scan_live_signals(tickers, from="2024-01-01"))

cat("\n=====================================\n")
cat(" RECENT EXITS (last 5 days)\n")
cat("=====================================\n")

print(scan_recent_exits(tickers, from="2024-01-01", lookback_days=5))

