# ============================
# Technical Scan (Yahoo) — FIXED v7.3
# - Fixes "'what' must be a function..." by removing TTR::BBands() dispatch
# - Bollinger Bands computed manually (SMA + runSD)
# - TOS-aligned Squeeze Momentum
# - Return_1Y = Google Finance-style (calendar 365 days, nearest trading day)
# - Return_1Y column placed after Price
# ============================

suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(TTR)
  library(zoo)
  library(xts)
})

# ---- SAFE Yahoo fetch ----
getSymbols_safe <- function(symbol, from, to = Sys.Date()) {
  out <- tryCatch(
    suppressWarnings(quantmod::getSymbols(symbol, src="yahoo", auto.assign=FALSE, from=from, to=to)),
    error = function(e) e
  )
  if (!inherits(out, "error")) return(out)

  out2 <- tryCatch(
    suppressWarnings(quantmod:::getSymbols.yahoo(Symbols=symbol, env=NULL, auto.assign=FALSE, from=from, to=to)),
    error = function(e) e
  )
  if (!inherits(out2, "error")) return(out2)

  stop(paste0(
    "getSymbols failed. Primary: ", conditionMessage(out),
    " | Fallback getSymbols.yahoo failed: ", conditionMessage(out2)
  ))
}

# --- Helper: rolling linreg VALUE at last bar (TOS/Pine linreg(src, n, 0)) ---
roll_linreg_value <- function(y, n = 20) {
  y_num <- as.numeric(y)

  vals <- zoo::rollapply(
    data  = y_num,
    width = n,
    align = "right",
    fill  = NA_real_,
    FUN   = function(w) {
      if (any(!is.finite(w))) return(NA_real_)
      x <- 0:(length(w) - 1)
      fit <- .lm.fit(x = cbind(1, x), y = w)
      co  <- fit$coefficients
      if (any(!is.finite(co))) return(NA_real_)
      co[1] + co[2] * (length(w) - 1)
    }
  )

  xts::xts(as.numeric(vals), order.by = zoo::index(y))
}

# --- Helper: Return_1Y (Google Finance-style: calendar 365d, nearest trading day) ---
calc_return_1y_google <- function(df, use_adjusted = TRUE, days_back = 365) {
  if (is.null(df) || NROW(df) < 200) return(NA_real_)

  px <- if (use_adjusted) {
    tryCatch(quantmod::Ad(df), error = function(e) quantmod::Cl(df))
  } else {
    quantmod::Cl(df)
  }

  px <- stats::na.omit(px)
  if (NROW(px) < 200) return(NA_real_)

  target_date <- Sys.Date() - days_back
  idx <- which.min(abs(zoo::index(px) - target_date))
  if (!is.finite(idx) || idx < 1) return(NA_real_)

  cur  <- as.numeric(tail(px, 1))
  past <- as.numeric(px[idx])
  if (!is.finite(cur) || !is.finite(past) || past <= 0) return(NA_real_)

  (cur / past - 1) * 100
}

# --- 1. LOAD TICKERS ---
ticker_file <- "Liquid_Stocks_ETFs.csv"
if (!file.exists(ticker_file)) {
  stop("CSV file not found! Please ensure 'Liquid_Stocks_ETFs.csv' is in your working directory.")
}
tickers <- read.csv(ticker_file, header=FALSE, stringsAsFactors=FALSE)$V1
tickers <- unique(trimws(tickers[!is.na(tickers) & tickers != ""]))

cat("--- RUNNING TECHNICAL SCAN (SYNCED TO TOS):", as.character(Sys.Date()), "---\n")
options(timeout = 300)

rows <- vector("list", length(tickers))
k <- 0

for (ticker in tickers) {
  cat("Scanning:", ticker, "\n")

  out <- tryCatch({

    df <- getSymbols_safe(ticker, from = Sys.Date() - 500, to = Sys.Date())
    if (is.null(df) || NROW(df) < 120) stop("Not enough data (<120 rows)")

    colnames(df) <- c("Open","High","Low","Close","Volume","Adjusted")
    close <- quantmod::Cl(df)

    # --- BASIC INDICATORS ---
    sma50 <- TTR::SMA(close, 50)
    sma20 <- TTR::SMA(close, 20)
    rsi14 <- TTR::RSI(close, 14)

    atr14 <- TTR::ATR(quantmod::HLC(df), 14)$atr
    atr20 <- TTR::ATR(quantmod::HLC(df), 20)$atr

    price <- as.numeric(tail(close, 1))
    rsi   <- as.numeric(tail(rsi14, 1))
    atrp  <- as.numeric(tail(atr14, 1)) / price * 100

    if (!is.finite(price) || price <= 0) stop("Bad price")
    if (!is.finite(rsi)) stop("Bad RSI")
    if (!is.finite(atrp)) stop("Bad ATR%")

    # --- Return_1Y (Google style) ---
    ret_1y <- calc_return_1y_google(df, use_adjusted = TRUE, days_back = 365)

    # --- 1. BOLLINGER BANDS (manual, avoids BBands() dispatch bug) ---
    bb_mid <- sma20
    bb_sd  <- TTR::runSD(close, 20)
    bb_up  <- bb_mid + 2 * bb_sd
    bb_dn  <- bb_mid - 2 * bb_sd

    sma20_last <- as.numeric(tail(bb_mid, 1))
    atr20_last <- as.numeric(tail(atr20, 1))
    bb_up_last <- as.numeric(tail(bb_up, 1))
    bb_dn_last <- as.numeric(tail(bb_dn, 1))

    if (!all(is.finite(c(sma20_last, atr20_last, bb_up_last, bb_dn_last)))) {
      stop("Bad BB/KC inputs (NA/Inf)")
    }

    kc_up <- sma20_last + 1.5 * atr20_last
    kc_dn <- sma20_last - 1.5 * atr20_last

    squeezing <- (bb_up_last < kc_up) && (bb_dn_last > kc_dn)
    sqz <- ifelse(squeezing, "SQUEEZE", "RELEASE")

    # --- 2. MOMENTUM HISTOGRAM LOGIC (TOS-style) ---
    hi  <- TTR::runMax(df$High, 20)
    lo  <- TTR::runMin(df$Low,  20)
    mid <- (((hi + lo) / 2) + sma20) / 2
    y   <- close - mid

    mom <- roll_linreg_value(y, 20)
    mh  <- as.numeric(tail(mom, 1))
    pm  <- as.numeric(tail(mom, 2)[1])
    if (!is.finite(mh) || !is.finite(pm)) stop("Bad momentum values")

    accel <- (mh - pm) > 0

    momint <- dplyr::case_when(
      mh > 0 & accel  ~ "✅ INTACT",
      mh > 0 & !accel ~ "⚠️ DECEL",
      mh < 0 & accel  ~ "🟨 RECOVER",
      mh < 0 & !accel ~ "❌ FAILED",
      TRUE ~ "—"
    )

    # --- 3. CHOP FILTER ---
    spread <- close - sma50
    fake <- sum(diff(sign(as.numeric(tail(spread, 30)))) != 0, na.rm = TRUE)

    # --- 4. ACTION ---
    action <- dplyr::case_when(
      fake > 5                         ~ "AVOID (CHOP)",
      sqz == "SQUEEZE" & accel         ~ "BUY (SQZ FIRE)",
      momint == "✅ INTACT" & rsi < 65  ~ "BUY",
      momint == "✅ INTACT" & rsi >= 65 ~ "DON'T CHASE",
      momint == "🟨 RECOVER"            ~ "WATCH RECLAIM",
      TRUE                              ~ "DO NOTHING"
    )

    hold <- ifelse(momint == "❌ FAILED", "EXIT/AVOID", "HOLD")

    # --- QUALITY SCORE ---
    q <- 10 -
      ifelse(fake > 4, 4, 0) -
      ifelse(rsi > 70, 2, 0) -
      ifelse(momint == "❌ FAILED", 3, 0) +
      ifelse(sqz == "SQUEEZE", 1, 0)

    data.frame(
      Ticker    = ticker,
      Quality   = as.integer(q),
      SQZ       = sqz,
      Price     = round(price, 2),
      Return_1Y = ifelse(is.finite(ret_1y), round(ret_1y, 2), NA_real_),
      RSI       = round(rsi),
      MomInt    = momint,
      ATRPct    = round(atrp, 2),
      Action    = paste(action, "|", hold),
      Fake      = as.integer(fake),
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(out)) { k <- k + 1; rows[[k]] <- out }
}

# --- OUTPUT ---
if (k > 0) {
  res <- do.call(rbind, rows[1:k])
  res <- res[order(-res$Quality), ]

  cat(sprintf(
    "\n%-6s %4s %-8s %8s %9s %4s %-10s %5s %-25s %4s\n",
    "Ticker","Qual","SQZ","Price","Ret1Y%","RSI","MomInt","ATR%","Action","Fake"
  ))
  cat(paste(rep("-", 115), collapse=""), "\n")

  for (i in 1:nrow(res)) {
    r <- res[i,]
    ret_str <- ifelse(is.na(r$Return_1Y), "   NA", sprintf("%6.2f", r$Return_1Y))
    cat(sprintf(
      "%-6s %4d %-8s %8.2f %9s %4d %-10s %5.2f %-25s %4d\n",
      r$Ticker, r$Quality, r$SQZ, r$Price, ret_str,
      r$RSI, r$MomInt, r$ATRPct, r$Action, r$Fake
    ))
  }
} else {
  cat("Scan complete: No data was successfully retrieved.\n")
}

