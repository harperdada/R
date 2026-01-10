# backtest_ticker.R
# ------------------------------------------------------------------------------
# FLPL2 (PPO + EMA21) system
#
# Long Entry when ALL three happen on the same day:
#   1) PPO (12,26) crosses above zero  (fast EMA crosses above slow EMA)
#   2) Price is above the 21-day EMA
#   3) (Implicit: we are flat)
#
# Exit when:
#   Price closes below the 21-day EMA for two consecutive days
#
# Enhancements:
#   If an open trade exists, also compute:
#     - AgeTD (trading days since entry, as of LastDate)
#     - Price_5D/10D/20D (close N trading days after EntryDate)
#     - Return_5D/10D/20D (% return vs EntryPrice)
#   These forward-looking columns are NA if the trade isn't old enough.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
  library(xts)
  library(zoo)
})

run_flpl2 <- function(symbol   = "ASTS",
                      from     = "2015-01-01",
                      to       = Sys.Date(),
                      fastPPO  = 12,
                      slowPPO  = 26,
                      emaLen   = 21,
                      startEq  = 100000,
                      riskFrac = 1.0) {

  px <- getSymbols(symbol, from = from, to = to, auto.assign = FALSE)
  px <- na.omit(px)

  closePx <- Cl(px)

  fastEMA <- EMA(closePx, n = fastPPO)
  slowEMA <- EMA(closePx, n = slowPPO)
  ppo     <- 100 * (fastEMA - slowEMA) / slowEMA
  ema21   <- EMA(closePx, n = emaLen)

  # ENTRY: PPO crosses above 0 AND close > EMA21
  entrySignal <- (ppo > 0) & (Lag(ppo) <= 0) & (closePx > ema21)

  # EXIT: close < EMA21 two days in a row
  belowEma   <- closePx < ema21
  exitSignal <- belowEma & Lag(belowEma, 1)

  entrySignal[is.na(entrySignal)] <- FALSE
  exitSignal[is.na(exitSignal)]   <- FALSE

  dates    <- index(px)
  n        <- NROW(px)
  position <- 0
  shares   <- 0
  cash     <- startEq
  equity   <- rep(NA_real_, n)

  tradeLog <- data.frame(
    EntryDate  = as.Date(character()),
    EntryPrice = numeric(),
    ExitDate   = as.Date(character()),
    ExitPrice  = numeric(),
    ReturnPct  = numeric(),
    stringsAsFactors = FALSE
  )

  entryPrice <- NA_real_
  entryDate  <- as.Date(NA)
  warmup     <- max(fastPPO, slowPPO, emaLen) + 2

  for (i in seq_len(n)) {
    price <- as.numeric(closePx[i])
    d     <- dates[i]

    if (i > warmup) {

      # Open long
      if (position == 0 && entrySignal[i]) {
        equityNow <- cash
        shares    <- floor((equityNow * riskFrac) / price)
        if (shares > 0) {
          cash       <- cash - shares * price
          position   <- 1
          entryPrice <- price
          entryDate  <- as.Date(d)
        }
      }

      # Close long
      if (position == 1 && exitSignal[i]) {
        cash   <- cash + shares * price
        retPct <- (price / entryPrice - 1) * 100

        tradeLog <- rbind(
          tradeLog,
          data.frame(
            EntryDate  = as.Date(entryDate),
            EntryPrice = entryPrice,
            ExitDate   = as.Date(d),
            ExitPrice  = price,
            ReturnPct  = retPct,
            stringsAsFactors = FALSE
          )
        )

        position   <- 0
        shares     <- 0
        entryPrice <- NA_real_
        entryDate  <- as.Date(NA)
      }
    }

    equity[i] <- cash + shares * price
  }

  eqSeries <- xts(equity, order.by = dates)
  colnames(eqSeries) <- "Equity"

  # If we're still in a trade at the end, return it as "open_trade"
  open_trade <- NULL
  if (position == 1 && !is.na(entryPrice)) {

    # Last bar
    lastPrice <- as.numeric(last(closePx))
    lastDate  <- as.Date(last(index(closePx)))
    unrealPct <- (lastPrice / entryPrice - 1) * 100

    # --- Forward performance snapshots (N trading days after entry) ---
    # Use this symbol's trading calendar from closePx index
    idx_entry <- which(index(closePx) == entryDate)
    if (length(idx_entry) != 1) idx_entry <- NA_integer_

    get_fwd_close <- function(k) {
      if (is.na(idx_entry)) return(NA_real_)
      j <- idx_entry + k
      if (j <= NROW(closePx)) as.numeric(closePx[j]) else NA_real_
    }

    price_5d  <- get_fwd_close(5)
    price_10d <- get_fwd_close(10)
    price_20d <- get_fwd_close(20)

    ret_5d  <- if (!is.na(price_5d))  (price_5d  / entryPrice - 1) * 100 else NA_real_
    ret_10d <- if (!is.na(price_10d)) (price_10d / entryPrice - 1) * 100 else NA_real_
    ret_20d <- if (!is.na(price_20d)) (price_20d / entryPrice - 1) * 100 else NA_real_

    age_td <- if (!is.na(idx_entry)) (NROW(closePx) - idx_entry) else NA_integer_

    open_trade <- data.frame(
      Symbol        = symbol,
      EntryDate     = as.Date(entryDate),
      EntryPrice    = entryPrice,
      LastDate      = as.Date(lastDate),
      LastPrice     = lastPrice,
      UnrealizedPct = unrealPct,

      AgeTD         = age_td,
      Price_5D      = price_5d,
      Price_10D     = price_10d,
      Price_20D     = price_20d,
      Return_5D     = ret_5d,
      Return_10D    = ret_10d,
      Return_20D    = ret_20d,

      stringsAsFactors = FALSE
    )
  }

  list(
    symbol     = symbol,
    trades     = tradeLog,   # closed trades
    equity     = eqSeries,
    open_trade = open_trade  # NULL if flat, 1-row data.frame if long
  )
}

# Convenience wrapper for your scanners
get_live_signal <- function(symbol, from = "2020-01-01") {
  res <- run_flpl2(symbol, from = from)
  res$open_trade
}

RUN_DEMO <- TRUE

# ------------------------------------------------------------------------------
# OPTIONAL DEMO (runs only if you set RUN_DEMO <- TRUE before sourcing)
# Example:
#   RUN_DEMO <- TRUE
#   source("backtest_ticker.R")
# ------------------------------------------------------------------------------

if (exists("RUN_DEMO") && isTRUE(RUN_DEMO)) {
  cat("\n========================\n")
  cat(" DEMO: ASTS (flpl2)\n")
  cat("========================\n")
  res_demo <- run_flpl2("ASTS", from = "2024-01-01")
  print(res_demo$trades)
  cat("\nTotal trades:", nrow(res_demo$trades), "\n")
  cat("Final equity:", round(as.numeric(last(res_demo$equity)), 2), "\n")
  cat("Total return (%):",
      round((as.numeric(last(res_demo$equity)) / 100000 - 1) * 100, 2), "\n")
}

