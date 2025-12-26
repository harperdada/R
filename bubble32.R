library(quantmod)
library(dplyr)

# --- 1. LOAD TICKERS ---
tickers <- read.csv("Liquid_Stocks_ETFs.csv", header = FALSE)$V1

results <- NULL

cat("--- CALCULATING FUNDAMENTALS ---\n")
cat("downloading set: 1 , 2 , ...done\n")

# --- 2. RUNNING TECHNICAL SCAN ---
cat("\n--- RUNNING TECHNICAL SCAN:", as.character(Sys.Date()), "---\n")
options(timeout = 20)

rows <- vector("list", length(tickers))
k <- 0

for (ticker in tickers) {
  cat("Scanning:", ticker, "\n")

  out <- tryCatch({
    df <- suppressWarnings(
      getSymbols(ticker, src="yahoo", auto.assign=FALSE,
                 from = Sys.Date() - 300)
    )

    colnames(df) <- c("Open","High","Low","Close","Volume","Adjusted")

    close <- Cl(df)
    sma50 <- SMA(close, n=50)

    # --- RSI ---
    rsi14 <- RSI(close, n=14)
    current_rsi <- as.numeric(last(rsi14))

    # 5-day RSI slope (less noisy)
    rsi_clean <- na.omit(rsi14)
    rsi_slope_5d <- if (length(rsi_clean) >= 6) {
        as.numeric(last(rsi_clean)) - as.numeric(tail(rsi_clean, 6))[1]
    } else {
        NA_real_
    }

    rsi_trend_5d <- if (is.na(rsi_slope_5d)) {
        NA_character_
    } else if (rsi_slope_5d > 1.5) {
        "RISING"
    } else if (rsi_slope_5d < -1.5) {
       "FALLING"
    } else {
       "FLAT"
    }
 
    

    # --- ATR ---
    atr14 <- ATR(HLC(df), n = 14)$atr
    current_atrd <- as.numeric(last(atr14))
    current_price <- as.numeric(last(close))

    current_atrpct <- if (!is.na(current_atrd) && !is.na(current_price) && current_price > 0) {
      100 * current_atrd / current_price
    } else {
      NA_real_
    }

    weekly_noise <- current_atrd * sqrt(5)
    safe_strike  <- current_price + weekly_noise

    current_sma <- as.numeric(last(sma50))

    # --- Momentum Integrity (risk/regime flag; NOT a buy/sell) ---
    momentum_integrity <- dplyr::case_when(
    # Failed: trend not holding + momentum falling
    !is.na(current_price) && !is.na(current_sma) &&
    current_price < current_sma && rsi_trend_5d == "FALLING" ~ "❌ FAILED",

    # Recovery attempt: below SMA50 but RSI rising/flat (watch for reclaim)
    !is.na(current_price) && !is.na(current_sma) &&
    current_price < current_sma && rsi_trend_5d %in% c("RISING") ~ "🟨 RECOVER",
   
    # Base-building: below SMA50 and RSI flat (stabilizing, not yet recovery)
    !is.na(current_price) && !is.na(current_sma) &&
    current_price < current_sma && rsi_trend_5d == "FLAT" ~ "🟧 BASE",


    # Decelerating: still above SMA50 but RSI falling
    !is.na(current_price) && !is.na(current_sma) &&
    current_price >= current_sma && rsi_trend_5d == "FALLING" ~ "⚠️ DECEL",

    # Grind: low vol + flat RSI
    !is.na(current_atrpct) && current_atrpct < 2 && rsi_trend_5d == "FLAT" ~ "🟦 GRIND",

    # Intact: above SMA50 + RSI rising/flat
    !is.na(current_price) && !is.na(current_sma) &&
    current_price >= current_sma && rsi_trend_5d %in% c("RISING","FLAT") ~ "✅ INTACT",

    TRUE ~ "—"
    )

    trade_bias <- dplyr::case_when(
        momentum_integrity == "❌ FAILED" ~ "AVOID",
        momentum_integrity == "🟧 BASE"   ~ "WAIT",
        momentum_integrity == "🟨 RECOVER"~ "WATCH",
        momentum_integrity == "🟦 GRIND"  ~ "CC",
        momentum_integrity == "⚠️ DECEL"  ~ "TIGHTEN",
        momentum_integrity == "✅ INTACT" ~ "TREND",
        TRUE ~ "—"
    )
    # --- Final actions depend on whether you already hold the ticker ---
    new_buy_action <- dplyr::case_when(
      trade_bias == "TREND"  & momentum_integrity == "✅ INTACT" ~ "DON'T CHASE",
      trade_bias == "WATCH"  & momentum_integrity == "🟨 RECOVER" ~ "WAIT CONFIRM",
      trade_bias == "WAIT"   & momentum_integrity %in% c("🟧 BASE","❌ FAILED") ~ "WAIT BASE",
      trade_bias == "AVOID"  ~ "AVOID",
      TRUE ~ "DO NOTHING"
    )

    hold_action <- dplyr::case_when(
      trade_bias == "TREND"  & momentum_integrity == "✅ INTACT" ~ "TRAIL STOP",
      trade_bias == "WATCH"  & momentum_integrity == "🟨 RECOVER" ~ "WATCH",
      trade_bias == "WAIT"   ~ "WAIT",
      trade_bias == "AVOID"  ~ "EXIT/AVOID",
      TRUE ~ "HOLD"
    )

    final_action <- paste0("NEW: ", new_buy_action, " | HOLD: ", hold_action)


    too_soon_cc <- (
      !is.na(current_rsi) &&
      current_price < current_sma &&
      current_atrpct >= 4 &&
      current_rsi <= 35
    )

    # --- FAST Fake_30d: count sign-crossings of (Close - SMA50) over last 30 bars
    spread <- close - sma50
    last30 <- tail(spread, 30)
    fake_count <- sum(diff(sign(as.numeric(last30))) != 0, na.rm = TRUE)

    perfect_cc <- (
      !too_soon_cc &&
      !is.na(current_rsi) &&
      current_price >= current_sma &&
      current_rsi >= 40 && current_rsi <= 60 &&
      current_atrpct >= 2 && current_atrpct <= 6 &&
      fake_count >= 3
    )

    is_macro <- ticker %in% c("GLD","SLV","IAU","PSLV","GDX","GDXJ","SPY","QQQ")

    if (is_macro) {
      quality_score <- 10
      status <- ifelse(current_price > current_sma, "SAFE HAVEN", "MACRO DIP")
    } else {
      quality_score <- 10
      if (fake_count > 3) quality_score <- quality_score - 3
      if (!is.na(current_rsi) && current_rsi > 70) quality_score <- quality_score - 2
      if (fake_count > 5) quality_score <- 3
      status <- ifelse(current_price > current_sma, "HEALTHY", "CAUTION")
    }

    signal <- "None"
    if (quality_score >= 7 && !is.na(current_rsi) && current_rsi < 30) signal <- "🔥 SALE!"

    data.frame(
  Ticker   = ticker,
  Quality  = as.integer(quality_score),
  Status   = status,
  Price    = round(current_price, 2),
  SMA50    = round(current_sma, 2),
  RSI      = round(current_rsi, 0),

  # keep ONLY 5d (less noisy)
  RSI_Slope_5d = round(rsi_slope_5d, 2),
  RSI_Trend_5d = rsi_trend_5d,
  
  Momentum_Integrity = momentum_integrity,
  TradeBias = trade_bias,

  ATRD     = round(current_atrd, 2),
  ATRPct   = round(current_atrpct, 2),
  FinalAction = final_action,
  CCFlag   = ifelse(too_soon_cc, "⛔ TOO SOON",
                    ifelse(perfect_cc, "✅ CC SWEET SPOT", "")),
  Fake_30d = as.integer(fake_count),
  stringsAsFactors = FALSE
)

  }, error = function(e) {
    cat("  -> failed:", ticker, "|", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(out)) {
    k <- k + 1
    rows[[k]] <- out
  }
}

results <- if (k > 0) do.call(rbind, rows[1:k]) else NULL

# --- 7. FORMATTED OUTPUT ---
if (!is.null(results)) {

  cat("\nCC INTERPRETATION GUIDE:\n")
  cat("  ⛔ TOO SOON        → Don’t sell covered calls (recent damage / rebound risk)\n")
  cat("  (blank)           → CCs OK, but be careful — go wider on strikes (≈2× ATR)\n")
  cat("  ✅ CC SWEET SPOT   → Ideal covered-call environment (chop, paid, low stress, can sell inside noise)\n\n")

  cat("\nCC DURATION / PREMIUM RULES (HARD STOPS):\n")
  cat("  • Max CC duration = 7 DTE  if ATR% ≥ 4%\n")
  cat("  • Max CC duration = 10 DTE if ATR% ≥ 3%\n")
  cat("  • If premium < 0.3% of notional → skip\n\n")
 
  cat("\nRecommended final MomInt regimes (clean + actionable):\n")
  cat("  ❌ FAILED = below SMA50 + RSI falling\n")
  cat("  🟨 RECOVER = below SMA50 + RSI rising/flat (reclaim candidate)\n")
  cat("  ⚠️ DECEL = above SMA50 + RSI falling (divergence / slowing)\n")
  cat("  🟦 GRIND = ATR% < 2 + RSI flat (CC-friendly chop)\n")
  cat("  ✅ INTACT = above SMA50 + RSI rising/flat\n\n")
  
  cat("  TREND + low ATR → boring grind up → shares\n")
  cat("  TREND + rising ATR → expansion → calls work\n")
  cat("  TREND + very high ATR → spreads > naked calls\n\n")

  cat("DEBUG columns:", paste(names(results), collapse=", "), "\n")

  # header
  cat(sprintf(
  "%-6s %7s %-10s %8s %8s %4s %7s %-8s %-12s %-8s %6s %6s %-26s %6s\n",
  "Ticker","Quality","Status",
  "Price","SMA50","RSI",
  "RSIΔ5d","RSITr5d",
  "MomInt","Bias",
  "ATRD","ATR%",
  "FinalAction","Fake"
  ))
  cat(paste0(rep("-", 165), collapse=""), "\n")

  # Sort by Quality Descending
  results <- results[order(-results$Quality), ]

  for (i in 1:nrow(results)) {
    r <- results[i, ]
    cat(sprintf(
  "%-6s %7d %-10s %8.2f %8.2f %4d %7.2f %-8s %-12s %-8s %6.2f %6.2f %-26s %6d\n",
  r$Ticker,
  r$Quality,
  r$Status,
  r$Price,
  r$SMA50,
  r$RSI,
  r$RSI_Slope_5d,
  r$RSI_Trend_5d,
  r$Momentum_Integrity,
  r$TradeBias,
  r$ATRD,
  r$ATRPct,
  r$FinalAction,
  r$Fake_30d
  ))


  }
}

