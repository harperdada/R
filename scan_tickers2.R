# ============================
# FLPL2 Live Scanner
# PPO + EMA21 System
# ============================

rm(list = ls())

source("backtest_ticker.R")
source("flpl_live_scan.R")

tickers <- as.vector(
  read.csv("Liquid_Stocks_ETFs.csv",
           header = FALSE,
           stringsAsFactors = FALSE)[[1]]
)

cat("Loaded", length(tickers), "tickers\n")

# =====================================
#  ALL OPEN POSITIONS
# =====================================
cat("\n=====================================\n")
cat(" ALL OPEN POSITIONS\n")
cat("=====================================\n")
all_open <- scan_live_signals(tickers, from = "2024-01-01")
if (!is.null(all_open)) {
  print(all_open[order(-all_open$UnrealizedPct), ])
}

# =====================================
#  NEW ENTRIES TODAY
# =====================================
cat("\n=====================================\n")
cat(" NEW ENTRIES TODAY\n")
cat("=====================================\n")
if (!is.null(all_open)) {
  new_today <- all_open[!is.na(all_open$AgeTD) & all_open$AgeTD == 0, ]
  if (nrow(new_today) > 0) {
    print(new_today)
  } else {
    cat("No new entries today.\n")
  }
}

# =====================================
#  RECENT EXITS (last 5 trading days)
# =====================================
cat("\n=====================================\n")
cat(" RECENT EXITS (last 5 trading days)\n")
cat("=====================================\n")
print(scan_recent_exits(tickers, from = "2024-01-01", lookback_days = 5))
