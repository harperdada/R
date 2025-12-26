library(quantmod)
library(dplyr)

# --- 1. LOAD TICKERS ---
# Ensure this file exists in your local R directory
tickers <- read.csv("Liquid_Stocks_ETFs.csv", header = FALSE)$V1

results <- NULL
cat("--- RUNNING TECHNICAL SCAN:", as.character(Sys.Date()), "---\n")
options(timeout = 20)

rows <- vector("list", length(tickers))
k <- 0

for (ticker in tickers) {
  cat("Scanning:", ticker, "\n")

  out <- tryCatch({
    df <- suppressWarnings(
      getSymbols(ticker, src="yahoo", auto.assign=FALSE, from = Sys.Date() - 300)
    )

    colnames(df) <- c("Open","High","Low","Close","Volume","Adjusted")
    close <- Cl(df)
    
    # --- BASIC INDICATORS ---
    sma50  <- SMA(close, n=50)
    sma20  <- SMA(close, n=20)
    rsi14  <- RSI(close, n=14)
    atr14  <- ATR(HLC(df), n=14)$atr
    atr20_obj <- ATR(HLC(df), n=20)
    atr20  <- atr20_obj$atr

    current_price <- as.numeric(last(close))
    current_sma   <- as.numeric(last(sma50))
    current_rsi   <- as.numeric(last(rsi14))
    current_atrd  <- as.numeric(last(atr14))
    current_atrpct <- (current_atrd / current_price) * 100

    # --- SQUEEZE LOGIC (Carter Setup) ---
    bb20 <- BBands(close, n=20, sd=2)
    # Keltner Channel = SMA20 +/- (1.5 * ATR20)
    kc_upper <- as.numeric(last(sma20)) + (1.5 * as.numeric(last(atr20)))
    kc_lower <- as.numeric(last(sma20)) - (1.5 * as.numeric(last(atr20)))
    bb_upper <- as.numeric(last(bb20$up))
    bb_lower <- as.numeric(last(bb20$dn))

    is_squeezing <- (bb_upper < kc_upper) && (bb_lower > kc_lower)
    sqz_status   <- if (is_squeezing) "SQUEEZE" else "RELEASE"

    # --- MOMENTUM SLOPE ---
    rsi_clean <- na.omit(rsi14)
    rsi_slope_5d <- if (length(rsi_clean) >= 6) {
        as.numeric(last(rsi_clean)) - as.numeric(tail(rsi_clean, 6))[1]
    } else { NA_real_ }

    rsi_trend_5d <- if (is.na(rsi_slope_5d)) "—" 
                    else if (rsi_slope_5d > 1.5) "RISING" 
                    else if (rsi_slope_5d < -1.5) "FALLING" 
                    else "FLAT"

    # --- FAKE COUNT (SMA CROSSINGS) ---
    spread <- close - sma50
    last30 <- tail(spread, 30)
    fake_count <- sum(diff(sign(as.numeric(last30))) != 0, na.rm = TRUE)

    # --- MOMENTUM INTEGRITY REGIMES ---
    momentum_integrity <- dplyr::case_when(
      current_price < current_sma & rsi_trend_5d == "FALLING" ~ "❌ FAILED",
      current_price < current_sma & rsi_trend_5d == "RISING"  ~ "🟨 RECOVER",
      current_price < current_sma & rsi_trend_5d == "FLAT"    ~ "🟧 BASE",
      current_price >= current_sma & rsi_trend_5d == "FALLING"~ "⚠️ DECEL",
      current_atrpct < 2 & rsi_trend_5d == "FLAT"             ~ "🟦 GRIND",
      current_price >= current_sma                           ~ "✅ INTACT",
      TRUE ~ "—"
    )

    # --- BUY/SELL/HOLD LOGIC ---
    new_buy_action <- dplyr::case_when(
      fake_count > 5 ~ "AVOID (CHOP)",
      is_squeezing & rsi_trend_5d == "RISING" ~ "BUY (SQUEEZE)",
      momentum_integrity == "✅ INTACT" & current_rsi < 65 ~ "BUY",
      momentum_integrity == "✅ INTACT" & current_rsi >= 65 ~ "DON'T CHASE",
      momentum_integrity == "🟨 RECOVER" ~ "WATCH RECLAIM",
      TRUE ~ "DO NOTHING"
    )

    hold_action <- ifelse(momentum_integrity == "❌ FAILED", "EXIT/AVOID", "HOLD")

    # --- CC FLAG (Vetoed by Squeeze) ---
    too_soon_cc <- (current_rsi <= 35 && current_price < current_sma)
    perfect_cc  <- (!is_squeezing && !too_soon_cc && current_rsi >= 40 && current_rsi <= 60 && fake_count >= 3)

    cc_flag <- dplyr::case_when(
      is_squeezing ~ "🤐 SQZ: NO CC",
      too_soon_cc  ~ "⛔ TOO SOON",
      perfect_cc   ~ "✅ CC SWEET SPOT",
      TRUE ~ ""
    )

    # --- QUALITY SCORE ---
    quality_score <- 10
    if (fake_count > 4) quality_score <- quality_score - 4
    if (is_squeezing)   quality_score <- quality_score + 1 # Squeezes are high potential
    if (current_rsi > 70) quality_score <- quality_score - 2
    if (current_atrpct > 8) quality_score <- quality_score - 1

    data.frame(
      Ticker = ticker, Quality = as.integer(quality_score), SQZ = sqz_status,
      Price = round(current_price, 2), RSI = round(current_rsi, 0),
      RSISlope = round(rsi_slope_5d, 1), MomInt = momentum_integrity,
      ATRPct = round(current_atrpct, 2),
      Action = paste0(new_buy_action, " | ", hold_action),
      CC = cc_flag, Fake = as.integer(fake_count), stringsAsFactors = FALSE
    )

  }, error = function(e) { NULL })

  if (!is.null(out)) { k <- k + 1; rows[[k]] <- out }
}

results <- do.call(rbind, rows[1:k])
results <- results[order(-results$Quality), ]

# --- PRINTING ---
cat(sprintf("%-6s %3s %-8s %8s %4s %6s %-10s %5s %-25s %-15s %4s\n", 
            "Ticker", "Qual", "SQZ", "Price", "RSI", "RSISl", "MomInt", "ATR%", "Action", "CC Status", "Fake"))
cat(paste0(rep("-", 110), collapse=""), "\n")

for (i in 1:nrow(results)) {
  r <- results[i, ]
  cat(sprintf("%-6s %4d %-8s %8.2f %4d %6.1f %-10s %5.2f %-25s %-15s %4d\n",
              r$Ticker, r$Quality, r$SQZ, r$Price, r$RSI, r$RSISlope, r$MomInt, r$ATRPct, r$Action, r$CC, r$Fake))
}
