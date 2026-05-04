# ============================================================
# backtest_portfolio.R
# Portfolio-level backtest for FLPL2 (PPO + EMA21) system
#
# Rules:
#   - Max 20 open positions at a time
#   - $5,000 per position (fixed)
#   - Starting capital: $100,000
#   - Entry: PPO(12,26) crosses above 0 AND close > EMA21
#     When multiple signals on same day → pick highest PPO
#   - Exit: close < EMA21 for 2 consecutive days
#   - Process exits BEFORE entries each day
# ============================================================

suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
  library(xts)
  library(zoo)
  library(PerformanceAnalytics)  # optional, for Sharpe
})

# ── Parameters ──────────────────────────────────────────────
TICKERS_FILE  <- "Liquid_Stocks_ETFs.csv"
FROM_DATE     <- "2025-01-01"
TO_DATE       <- "2025-12-31"
MAX_POSITIONS <- 20
POSITION_SIZE <- 5000
START_EQUITY  <- 100000
FAST_PPO      <- 12
SLOW_PPO      <- 26
EMA_LEN       <- 21

# ── Load tickers ─────────────────────────────────────────────
tickers <- as.vector(
  read.csv(TICKERS_FILE, header = FALSE, stringsAsFactors = FALSE)[[1]]
)
cat("Loaded", length(tickers), "tickers\n")

# ── Step 1: Download all price data once ─────────────────────
# Use a wider window so indicators are warm before FROM_DATE
WARMUP_FROM <- as.character(as.Date(FROM_DATE) - 180)

cat("Downloading price data (this may take a few minutes)...\n")
price_data <- list()

for (sym in tickers) {
  px <- tryCatch(
    getSymbols(sym, from = WARMUP_FROM, to = TO_DATE,
               auto.assign = FALSE, warnings = FALSE),
    error   = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is.null(px) && NROW(px) > SLOW_PPO + EMA_LEN + 5) {
    price_data[[sym]] <- px
  }
}
cat("Successfully loaded", length(price_data), "tickers\n")

# ── Step 2: Pre-compute indicators for every ticker ──────────
cat("Computing indicators...\n")
indicators <- list()

for (sym in names(price_data)) {
  px    <- price_data[[sym]]
  cl    <- Cl(px)
  fast  <- EMA(cl, n = FAST_PPO)
  slow  <- EMA(cl, n = SLOW_PPO)
  ppo   <- 100 * (fast - slow) / slow
  ema21 <- EMA(cl, n = EMA_LEN)

  indicators[[sym]] <- list(
    close = cl,
    ppo   = ppo,
    ema21 = ema21,
    dates = index(px)
  )
}

# ── Step 3: Build master trading calendar for backtest period ─
all_dates <- sort(unique(unlist(lapply(indicators, function(x) {
  as.character(x$dates[x$dates >= as.Date(FROM_DATE) &
                         x$dates <= as.Date(TO_DATE)])
}))))
all_dates <- as.Date(all_dates)
cat("Trading days in backtest:", length(all_dates), "\n")

# ── Step 4: Portfolio simulation ─────────────────────────────
cash          <- START_EQUITY
open_pos      <- list()   # named list: sym -> list(entryDate, entryPrice, shares, ppo_at_entry)
trade_log     <- data.frame()
equity_curve  <- data.frame(Date = as.Date(character()), Equity = numeric())

cat("Running simulation...\n")

for (d in as.character(all_dates)) {
  today <- as.Date(d)

  # ── A. Process EXITS first ──────────────────────────────────
  if (length(open_pos) > 0) {
    to_close <- c()

    for (sym in names(open_pos)) {
      ind <- indicators[[sym]]
      # Find today's index in this ticker's date series
      idx <- which(ind$dates == today)
      if (length(idx) == 0 || idx < 2) next

      cl_today  <- as.numeric(ind$close[idx])
      cl_yday   <- as.numeric(ind$close[idx - 1])
      ema_today <- as.numeric(ind$ema21[idx])
      ema_yday  <- as.numeric(ind$ema21[idx - 1])

      # Exit: close < EMA21 two consecutive days
      if (!is.na(cl_today) && !is.na(ema_today) &&
          !is.na(cl_yday)  && !is.na(ema_yday)  &&
          cl_today < ema_today && cl_yday < ema_yday) {

        pos    <- open_pos[[sym]]
        shares <- pos$shares
        ret    <- (cl_today / pos$entryPrice - 1) * 100
        proceeds <- shares * cl_today
        cash   <- cash + proceeds

        trade_log <- rbind(trade_log, data.frame(
          Symbol     = sym,
          EntryDate  = pos$entryDate,
          EntryPrice = pos$entryPrice,
          ExitDate   = today,
          ExitPrice  = cl_today,
          Shares     = shares,
          ReturnPct  = ret,
          PnL        = proceeds - POSITION_SIZE,
          stringsAsFactors = FALSE
        ))

        to_close <- c(to_close, sym)
      }
    }
    # Remove closed positions
    for (sym in to_close) open_pos[[sym]] <- NULL
  }

  # ── B. Find ENTRY signals today ─────────────────────────────
  slots_available <- MAX_POSITIONS - length(open_pos)

  if (slots_available > 0 && cash >= POSITION_SIZE) {
    entry_candidates <- data.frame()

    for (sym in names(indicators)) {
      # Skip if already in position
      if (sym %in% names(open_pos)) next

      ind <- indicators[[sym]]
      idx <- which(ind$dates == today)
      if (length(idx) == 0 || idx < 2) next

      ppo_today  <- as.numeric(ind$ppo[idx])
      ppo_yday   <- as.numeric(ind$ppo[idx - 1])
      cl_today   <- as.numeric(ind$close[idx])
      ema_today  <- as.numeric(ind$ema21[idx])

      if (is.na(ppo_today) || is.na(ppo_yday) ||
          is.na(cl_today)  || is.na(ema_today)) next

      # Entry: PPO crosses above 0 AND close > EMA21
      if (ppo_today > 0 && ppo_yday <= 0 && cl_today > ema_today) {
        entry_candidates <- rbind(entry_candidates, data.frame(
          Symbol    = sym,
          PPO       = ppo_today,
          Price     = cl_today,
          stringsAsFactors = FALSE
        ))
      }
    }

    # Rank by highest PPO, take top N slots
    if (nrow(entry_candidates) > 0) {
      entry_candidates <- entry_candidates[order(-entry_candidates$PPO), ]
      n_take <- min(nrow(entry_candidates), slots_available,
                    floor(cash / POSITION_SIZE))

      for (i in seq_len(n_take)) {
        sym   <- entry_candidates$Symbol[i]
        price <- entry_candidates$Price[i]
        ppo_v <- entry_candidates$PPO[i]
        shares <- floor(POSITION_SIZE / price)
        if (shares == 0) next

        actual_cost <- shares * price
        cash <- cash - actual_cost

        open_pos[[sym]] <- list(
          entryDate  = today,
          entryPrice = price,
          shares     = shares,
          ppo_entry  = ppo_v
        )
      }
    }
  }

  # ── C. Mark-to-market equity ────────────────────────────────
  open_value <- 0
  for (sym in names(open_pos)) {
    ind <- indicators[[sym]]
    idx <- which(ind$dates == today)
    if (length(idx) > 0) {
      open_value <- open_value +
        open_pos[[sym]]$shares * as.numeric(ind$close[idx])
    }
  }

  equity_curve <- rbind(equity_curve, data.frame(
    Date   = today,
    Equity = cash + open_value
  ))
}

# ── Step 5: Performance Summary ───────────────────────────────
cat("\n=============================================\n")
cat(sprintf(" BACKTEST RESULTS: %s to %s\n", FROM_DATE, TO_DATE))
cat("=============================================\n")

final_equity  <- tail(equity_curve$Equity, 1)
total_return  <- (final_equity / START_EQUITY - 1) * 100
n_trades      <- nrow(trade_log)
wins          <- sum(trade_log$ReturnPct > 0, na.rm = TRUE)
losses        <- sum(trade_log$ReturnPct <= 0, na.rm = TRUE)
win_rate      <- if (n_trades > 0) wins / n_trades * 100 else NA
avg_win       <- if (wins > 0) mean(trade_log$ReturnPct[trade_log$ReturnPct > 0]) else NA
avg_loss      <- if (losses > 0) mean(trade_log$ReturnPct[trade_log$ReturnPct <= 0]) else NA
avg_return    <- mean(trade_log$ReturnPct, na.rm = TRUE)
total_pnl     <- sum(trade_log$PnL, na.rm = TRUE)

# Max drawdown
rolling_max   <- cummax(equity_curve$Equity)
drawdowns     <- (equity_curve$Equity - rolling_max) / rolling_max * 100
max_dd        <- min(drawdowns)

cat(sprintf("Starting Capital:   $%s\n",   format(START_EQUITY,  big.mark=",")))
cat(sprintf("Ending Equity:      $%s\n",   format(round(final_equity), big.mark=",")))
cat(sprintf("Total Return:       %.2f%%\n", total_return))
cat(sprintf("Total P&L:          $%s\n",   format(round(total_pnl), big.mark=",")))
cat(sprintf("Max Drawdown:       %.2f%%\n", max_dd))
cat(sprintf("Total Trades:       %d\n",     n_trades))
cat(sprintf("Wins:               %d\n",     wins))
cat(sprintf("Losses:             %d\n",     losses))
cat(sprintf("Win Rate:           %.1f%%\n", win_rate))
cat(sprintf("Avg Win:            %.2f%%\n", avg_win))
cat(sprintf("Avg Loss:           %.2f%%\n", avg_loss))
cat(sprintf("Avg Trade Return:   %.2f%%\n", avg_return))
cat(sprintf("Max Positions Used: %d\n",     MAX_POSITIONS))

cat("\n--- TRADE LOG (all closed trades) ---\n")
if (nrow(trade_log) > 0) {
  trade_log$EntryDate <- as.Date(trade_log$EntryDate)
  trade_log$ExitDate  <- as.Date(trade_log$ExitDate)
  trade_log$HoldDays  <- as.integer(trade_log$ExitDate - trade_log$EntryDate)
  print(trade_log[order(trade_log$ExitDate), ])
}

cat(sprintf("\n--- STILL OPEN AT END OF %s ---\n", TO_DATE))
if (length(open_pos) > 0) {
  for (sym in names(open_pos)) {
    p <- open_pos[[sym]]
    cat(sprintf("  %s: entry %s @ %.2f\n", sym, p$entryDate, p$entryPrice))
  }
} else {
  cat("  None\n")
}

# ── Step 6: Save outputs ──────────────────────────────────────
write.csv(trade_log,    sprintf("backtest_trades_%s_%s.csv",   FROM_DATE, TO_DATE), row.names = FALSE)
write.csv(equity_curve, sprintf("backtest_equity_%s_%s.csv",   FROM_DATE, TO_DATE), row.names = FALSE)
cat(sprintf("\nSaved: backtest_trades_%s_%s.csv\n",   FROM_DATE, TO_DATE))
cat(sprintf("Saved: backtest_equity_%s_%s.csv\n",     FROM_DATE, TO_DATE))

# ── Step 7: Equity curve plot ─────────────────────────────────
png(sprintf("backtest_equity_curve_%s_%s.png", FROM_DATE, TO_DATE), width = 1000, height = 500)
plot(equity_curve$Date, equity_curve$Equity,
     type = "l", col = "steelblue", lwd = 2,
     main = sprintf("FLPL2 PPO+EMA21 | Portfolio Equity Curve %s to %s", FROM_DATE, TO_DATE),
     xlab = "Date", ylab = "Portfolio Value ($)",
     ylim = c(min(equity_curve$Equity) * 0.95,
               max(equity_curve$Equity) * 1.05))
abline(h = START_EQUITY, col = "gray50", lty = 2)
grid()
dev.off()
cat(sprintf("Saved: backtest_equity_curve_%s_%s.png\n", FROM_DATE, TO_DATE))
