# ============================
# Technical Scan (Yahoo) — FIXED v5 (TOS Squeeze Momentum aligned + Return_1Y)
# - Momentum histogram uses rolling linear regression *VALUE* at last bar
#   (intercept + slope*(n-1)), not intercept alone.
# - DECEL/INTACT based on bar-to-bar delta (like TOS coloring)
# - Adds Return_1Y% similar to Google Finance "1Y" (approx 252 trading days)
# ============================

suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(TTR)
  library(zoo)
  library(xts)
})

# --- Helper: rolling linreg VALUE at last bar (like TOS/Pine linreg(src, n, 0)) ---
roll_linreg_value <- function(y, n = 20) {
  y_num <- as.numeric(y)

  vals <- zoo::rollapply(
    data = seq_along(y_num),
    width = n,
    align = "right",
    fill = NA_real_,
    FUN = function(idx) {
      yy <- y_num[idx]
      if (any(!is.finite(yy))) return(NA_real_)

      # x = 0..(n-1) like most linreg implementations
      xx <- 0:(length(yy) - 1)

      fit <- stats::lm(yy ~ xx)
      co  <- stats::coef(fit)
      if (any(!is.finite(co))) return(NA_real_)

      intercept <- co[1]
      slope     <- co[2]

      # value at last bar (offset = 0): intercept + slope*(n-1)
      intercept + slope * (length(yy) - 1)
    }
  )

  xts::xts(as.numeric(vals), order.by = zoo::index(y))
}

# --- Helper: 1Y return (Google Finance-style) ---
# Uses ~252 trading days (not calendar year).
# By default uses Adjusted close (closer to "total return" behavior).
calc_return_1y <- function(df, use_adjusted = TRUE, lookback = 252) {
  if (is.null(df) || NROW(df) < (lookback + 5)) return(NA_real_)

  # choose series for return
  px <- if (use_adjusted) {
    # if Adjusted missing, fallback to Close
    ad <- tryCatch(Ad(df), error = function(e) NULL)
    if (is.null(ad)) Cl(df) else ad
  } else {
    Cl(df)
  }

  px_num <- as.numeric(px)
  if (length(px_num) < (lookback + 1)) return(NA_real_)

  cur <- px_num[length(px_num)]
  past <- px_num[length(px_num) - lookback]

  if (!is.finite(cur) || !is.finite(past) || past <= 0) return(NA_real_)
  (cur / past - 1) * 100
}

# --- 1. LOAD TICKERS ---
ticker_file <- "Liquid_Stocks_ETFs.csv"
if (!file.exists(ticker_file)) {
  stop("CSV file not found! Please ensure 'Liquid_Stocks_ETFs.csv' is in your working directory.")
}

tickers <- read.csv(ticker_file, header = FALSE, stringsAsFactors = FALSE)$V1
tickers <- trimws(tickers)
tickers <- tickers[!is.na(tickers) & tickers != ""]
tickers <- unique(tickers)

cat("--- RUNNING TECHNICAL SCAN (SYNCED TO TOS):", as.character(Sys.Date()), "---\n")
options(timeout = 300)

rows <- vector("list", length(tickers))
k <- 0

for (ticker in tickers) {
  cat("Scanning:", ticker, "\n")

  out <- tryCatch({

    df <- suppressWarnings(
      getSymbols(ticker, src = "yahoo", auto.assign = FALSE, from = Sys.Date() - 600)
      # ^ bumped from 300 to 600 so 1Y return has better chance to work
    )
    if (is.null(df) || NROW(df) < 80) stop("Not enough data (<80 rows)")

    colnames(df) <- c("Open","High","Low","Close","Volume","Adjusted")
    close <- Cl(df)

    # --- BASIC INDICATORS ---
    sma50 <- SMA(close, n = 50)
    sma20 <- SMA(close, n = 20)
    rsi14 <- RSI(close, n = 14)

    atr14 <- ATR(HLC(df), n = 14)$atr
    atr20 <- ATR(HLC(df), n = 20)$atr

    current_price <- as.numeric(last(close))
    current_rsi   <- as.numeric(last(rsi14))
    current_atr14 <- as.numeric(last(atr14))
    if (!is.finite(current_price) || current_price <= 0) stop("Bad current price")
    if (!is.finite(current_atr14) || current_atr14 <= 0) stop("Bad ATR14")

    current_atrpct <- (current_atr14 / current_price) * 100

    # --- Return_1Y (Google Finance-ish) ---
    # Default: use_adjusted=TRUE. Set to FALSE if you want pure price return.
    ret_1y <- calc_return_1y(df, use_adjusted = TRUE, lookback = 252)

    # --- 1. SQUEEZE DOT LOGIC ---
    bb20 <- BBands(close, n = 20, sd = 2)

    sma20_last <- as.numeric(last(sma20))
    atr20_last <- as.numeric(last(atr20))
    bb_upper   <- as.numeric(last(bb20$up))
    bb_lower   <- as.numeric(last(bb20$dn))
    if (!is.finite(sma20_last) || !is.finite(atr20_last)) stop("Bad SMA20/ATR20 for squeeze")
    if (!is.finite(bb_upper)   || !is.finite(bb_lower))   stop("Bad BBands for squeeze")

    kc_upper <- sma20_last + (1.5 * atr20_last)
    kc_lower <- sma20_last - (1.5 * atr20_last)

    is_squeezing <- (bb_upper < kc_upper) && (bb_lower > kc_lower)
    sqz_status   <- if (is_squeezing) "SQUEEZE" else "RELEASE"

    # --- 2. MOMENTUM HISTOGRAM LOGIC (TOS-style) ---
    highest_20 <- runMax(df$High, n = 20)
    lowest_20  <- runMin(df$Low,  n = 20)

    # Typical squeeze midpoint used in many implementations:
    mid_point <- (((highest_20 + lowest_20) / 2) + sma20) / 2
    y <- close - mid_point

    # linreg VALUE at last bar, not intercept
    mom_series <- roll_linreg_value(y, n = 20)

    mom_hist <- as.numeric(last(mom_series))
    prev_mom <- as.numeric(tail(mom_series, 2)[1])
    if (!is.finite(mom_hist) || !is.finite(prev_mom)) stop("Bad momentum values (NA/Inf)")

    mom_delta <- mom_hist - prev_mom
    mom_accel <- mom_delta > 0

    # --- 3. MOMENTUM INTEGRITY REGIMES (TOS-aligned) ---
    momentum_integrity <- dplyr::case_when(
      mom_hist > 0  & mom_accel  ~ "✅ INTACT",   # light blue
      mom_hist > 0  & !mom_accel ~ "⚠️ DECEL",    # dark blue
      mom_hist < 0  & !mom_accel ~ "❌ FAILED",   # red
      mom_hist < 0  & mom_accel  ~ "🟨 RECOVER",  # yellow
      TRUE ~ "—"
    )

    # --- 4. TRADING LOGIC & CHOP FILTER ---
    spread <- close - sma50
    last30 <- tail(spread, 30)
    fake_count <- sum(diff(sign(as.numeric(last30))) != 0, na.rm = TRUE)

    new_buy_action <- dplyr::case_when(
      fake_count > 5 ~ "AVOID (CHOP)",
      is_squeezing & mom_accel ~ "BUY (SQZ FIRE)",
      momentum_integrity == "✅ INTACT" & current_rsi < 65 ~ "BUY",
      momentum_integrity == "✅ INTACT" & current_rsi >= 65 ~ "DON'T CHASE",
      momentum_integrity == "🟨 RECOVER" ~ "WATCH RECLAIM",
      TRUE ~ "DO NOTHING"
    )

    hold_action <- ifelse(momentum_integrity == "❌ FAILED", "EXIT/AVOID", "HOLD")

    # --- QUALITY SCORE ---
    q_score <- 10
    if (fake_count > 4) q_score <- q_score - 4
    if (is_squeezing)   q_score <- q_score + 1
    if (current_rsi > 70) q_score <- q_score - 2
    if (momentum_integrity == "❌ FAILED") q_score <- q_score - 3

    data.frame(
      Ticker    = ticker,
      Quality   = as.numeric(q_score),
      SQZ       = sqz_status,
      Price     = round(current_price, 2),
      RSI       = round(current_rsi, 0),
      MomInt    = momentum_integrity,
      ATRPct    = round(current_atrpct, 2),
      Return_1Y = ifelse(is.finite(ret_1y), round(ret_1y, 2), NA_real_),
      Action    = paste0(new_buy_action, " | ", hold_action),
      Fake      = as.integer(fake_count),
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    cat("  ERROR for", ticker, ":", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(out)) {
    k <- k + 1
    rows[[k]] <- out
  }
}

cat("  ⛔ TOO SOON        → Don’t sell covered calls (recent damage / rebound risk)\n")
cat("  (blank)           → CCs OK, but be careful — go wider on strikes (≈2× ATR)\n")
cat("  ✅ CC SWEET SPOT   → Ideal covered-call environment (chop, paid, low stress, can sell inside noise)\n\n")

cat("\nCC DURATION / PREMIUM RULES (HARD STOPS):\n")
cat("  • Max CC duration = 7 DTE  if ATR% ≥ 4%\n")
cat("  • Max CC duration = 10 DTE if ATR% ≥ 3%\n")
cat("  • If premium < 0.3% of notional → skip\n\n")

cat(" ATR% < 2% -    Very Stable\n")
cat(" ATR%  2-5% -   Normal Tradable\n")
cat(" ATR%  5-8% -   Aggressive\n")
cat(" ATR%  8-12% -  Speculative\n")
cat(" ATR%  >15% -   Untradable for trend\n")
cat(" ATR%  >50% -   Pure Noise\n")

# --- 5. FINAL TABLE PROCESSING & SORTING ---
if (k > 0) {
  results <- do.call(rbind, rows[1:k])
  results$Quality <- as.numeric(results$Quality)
  results <- results[order(-results$Quality), ]

  cat(sprintf("\n%-6s %4s %-8s %8s %4s %-10s %5s %9s %-25s %4s\n",
              "Ticker", "Qual", "SQZ", "Price", "RSI", "MomInt", "ATR%", "Ret1Y%", "Action", "Fake"))
  cat(paste0(rep("-", 112), collapse = ""), "\n")

  for (i in 1:nrow(results)) {
    r <- results[i, ]
    ret_str <- ifelse(is.na(r$Return_1Y), "   NA", sprintf("%6.2f", r$Return_1Y))
    cat(sprintf("%-6s %4d %-8s %8.2f %4d %-10s %5.2f %9s %-25s %4d\n",
                r$Ticker, r$Quality, r$SQZ, r$Price, r$RSI, r$MomInt, r$ATRPct, ret_str, r$Action, r$Fake))
  }
} else {
  cat("Scan complete: No data was successfully retrieved.\n")
}

