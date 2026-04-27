# ============================
# Squeeze Probability Engine v1
# Upgrades: SQZ_PROB, BIAS, VC_SCORE, SQZ_DAYS, SQZ_HIST%, FIRE_RISK
# ============================

suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(TTR)
  library(zoo)
  library(xts)
})

# --- Rolling linreg value (TOS-style) ---
roll_linreg_value <- function(y, n = 20) {
  y_num <- as.numeric(y)
  vals <- zoo::rollapply(
    data = seq_along(y_num), width = n, align = "right", fill = NA_real_,
    FUN = function(idx) {
      yy <- y_num[idx]
      if (any(!is.finite(yy))) return(NA_real_)
      xx <- 0:(length(yy) - 1)
      fit <- stats::lm(yy ~ xx)
      co <- stats::coef(fit)
      if (any(!is.finite(co))) return(NA_real_)
      co[1] + co[2] * (length(yy) - 1)
    }
  )
  xts::xts(as.numeric(vals), order.by = zoo::index(y))
}

# --- 1Y return ---
calc_return_1y <- function(df, use_adjusted = TRUE, lookback = 252) {
  if (is.null(df) || NROW(df) < lookback + 5) return(NA_real_)
  px <- if (use_adjusted) {
    ad <- tryCatch(Ad(df), error = function(e) NULL)
    if (is.null(ad)) Cl(df) else ad
  } else Cl(df)
  px_num <- as.numeric(px)
  cur  <- px_num[length(px_num)]
  past <- px_num[length(px_num) - lookback]
  if (!is.finite(cur) || !is.finite(past) || past <= 0) return(NA_real_)
  (cur / past - 1) * 100
}

# ============================================================
# NEW: Squeeze Probability Engine helpers
# ============================================================

# Rolling BB/KC squeeze ratio series (full history)
calc_squeeze_ratio_series <- function(close, hlc, n_bb = 20, n_atr = 20, kc_mult = 1.5) {
  bb   <- BBands(close, n_bb)
  atr_ <- ATR(hlc, n_atr)$atr
  sma_ <- SMA(close, n_bb)

  bb_w  <- as.numeric(bb$up) - as.numeric(bb$dn)
  kc_w  <- 2 * kc_mult * as.numeric(atr_)

  ratio <- bb_w / kc_w
  ratio
}

# Days currently in squeeze (consecutive bars where ratio < 1.0)
calc_squeeze_days <- function(ratio_series) {
  r <- rev(as.numeric(ratio_series))   # most recent first
  r <- r[!is.na(r)]
  count <- 0L
  for (v in r) {
    if (v < 1.0) count <- count + 1L else break
  }
  count
}

# Historical squeeze frequency (% of bars in squeeze over full history)
calc_squeeze_hist_pct <- function(ratio_series) {
  r <- as.numeric(ratio_series)
  r <- r[!is.na(r)]
  if (length(r) == 0) return(NA_real_)
  mean(r < 1.0) * 100
}

# Volatility compression score (0–100): how compressed vs own history
calc_vc_score <- function(ratio_series, lookback = 252) {
  r <- as.numeric(ratio_series)
  r <- r[!is.na(r)]
  if (length(r) < 20) return(NA_real_)
  window <- tail(r, lookback)
  cur    <- tail(r, 1)
  pct    <- mean(window >= cur, na.rm = TRUE) * 100   # percentile (low ratio = tight = high score)
  round(pct, 0)
}

# Breakout direction bias from momentum + SMA slope
calc_bias <- function(mom_hist, sma50_last, sma50_prev, close_last) {
  sma_slope <- sma50_last - sma50_prev
  case_when(
    mom_hist > 0 & sma_slope > 0  ~ "UP",
    mom_hist < 0 & sma_slope < 0  ~ "DOWN",
    TRUE                           ~ "FLAT"
  )
}

# Squeeze Probability: weighted composite (0–100)
calc_sqz_prob <- function(squeeze_ratio, vc_score, sqz_days, sqz_hist_pct,
                           mom_hist, chop_score) {
  # Component 1: how tight is the current squeeze (40% weight)
  tightness <- max(0, min(100, (1 - squeeze_ratio) * 130))

  # Component 2: volatility compression percentile (25% weight)
  vc <- if (!is.na(vc_score)) vc_score else 50

  # Component 3: duration pressure — longer squeeze = higher prob (20% weight)
  duration_score <- min(100, sqz_days * 5)

  # Component 4: historical base rate for this ticker (10% weight)
  base <- if (!is.na(sqz_hist_pct)) sqz_hist_pct else 20

  # Penalty: chop kills probability (subtract up to 30 pts)
  chop_penalty <- chop_score * 80

  # Momentum alignment bonus (+10 if positive)
  mom_bonus <- if (!is.na(mom_hist) && mom_hist > 0) 10 else 0

  raw <- 0.40 * tightness +
         0.25 * vc +
         0.20 * duration_score +
         0.10 * base +
         mom_bonus -
         chop_penalty

  round(max(0, min(100, raw)), 0)
}

# Fire risk tier
calc_fire_risk <- function(sqz_prob, sqz_days, mom_hist, mom_delta) {
  case_when(
    sqz_prob >= 75 & sqz_days >= 8 & mom_hist > 0 & mom_delta > 0 ~ "HIGH",
    sqz_prob >= 50                                                  ~ "MED",
    TRUE                                                            ~ "LOW"
  )
}

# ============================================================
# LOAD TICKERS
# ============================================================
ticker_file <- "Liquid_Stocks_ETFs.csv"
tickers <- unique(trimws(read.csv(ticker_file, header = FALSE)$V1))

cat("--- RUNNING SQUEEZE PROBABILITY ENGINE ---\n")

rows <- list()
k    <- 0L

for (ticker in tickers) {

  cat("Scanning:", ticker, "\n")

  out <- tryCatch({

    df <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE, from = Sys.Date() - 600)
    if (NROW(df) < 80) stop("Not enough data")

    colnames(df) <- c("Open","High","Low","Close","Volume","Adjusted")
    close  <- Cl(df)
    hlc_df <- HLC(df)

    # --- Base indicators ---
    sma50  <- SMA(close, 50)
    sma20  <- SMA(close, 20)
    rsi14  <- RSI(close, 14)
    atr14  <- ATR(hlc_df, 14)$atr
    atr20  <- ATR(hlc_df, 20)$atr

    price     <- as.numeric(last(close))
    rsi       <- as.numeric(last(rsi14))
    atrpct    <- (as.numeric(last(atr14)) / price) * 100
    ret_1y    <- calc_return_1y(df)

    sma50_last <- as.numeric(last(sma50))
    sma50_prev <- as.numeric(tail(sma50, 2)[1])
    sma20_last <- as.numeric(last(sma20))
    atr20_last <- as.numeric(last(atr20))

    # --- Squeeze (current bar) ---
    bb <- BBands(close, 20)

    bb_upper <- as.numeric(last(bb$up))
    bb_lower <- as.numeric(last(bb$dn))
    kc_upper <- sma20_last + 1.5 * atr20_last
    kc_lower <- sma20_last - 1.5 * atr20_last

    bb_width     <- bb_upper - bb_lower
    kc_width     <- kc_upper - kc_lower
    squeeze_ratio <- bb_width / kc_width

    sqz_score  <- round((1 - squeeze_ratio) * 12, 0)

    sqz_status <- case_when(
      squeeze_ratio < 0.75 ~ "TIGHT",
      squeeze_ratio < 1.00 ~ "SQUEEZE",
      TRUE                 ~ "RELEASE"
    )

    # --- Momentum ---
    highest_20 <- runMax(df$High, 20)
    lowest_20  <- runMin(df$Low,  20)
    mid        <- (((highest_20 + lowest_20) / 2) + sma20) / 2
    y          <- close - mid

    mom       <- roll_linreg_value(y, 20)
    mom_hist  <- as.numeric(last(mom))
    prev_mom  <- as.numeric(tail(mom, 2)[1])
    mom_delta <- mom_hist - prev_mom

    momentum_integrity <- case_when(
      mom_hist > 0 & mom_delta > 0  ~ "✅ INTACT",
      mom_hist > 0 & mom_delta <= 0 ~ "⚠️ DECEL",
      mom_hist < 0 & mom_delta <= 0 ~ "❌ FAILED",
      mom_hist < 0 & mom_delta > 0  ~ "🟨 RECOVER",
      TRUE                          ~ "—"
    )

    # --- Chop ---
    spread     <- close - sma50
    last30     <- tail(spread, 30)
    fake_count <- sum(diff(sign(as.numeric(last30))) != 0, na.rm = TRUE)
    chop_score <- fake_count / 30
    choppy     <- chop_score > 0.25

    # ============================================================
    # NEW: Squeeze Probability Engine
    # ============================================================
    ratio_series  <- calc_squeeze_ratio_series(close, hlc_df)
    sqz_days      <- calc_squeeze_days(ratio_series)
    sqz_hist_pct  <- round(calc_squeeze_hist_pct(ratio_series), 1)
    vc_score      <- calc_vc_score(ratio_series)

    bias          <- calc_bias(mom_hist, sma50_last, sma50_prev, price)

    sqz_prob      <- calc_sqz_prob(squeeze_ratio, vc_score, sqz_days,
                                    sqz_hist_pct, mom_hist, chop_score)

    fire_risk     <- calc_fire_risk(sqz_prob, sqz_days, mom_hist, mom_delta)

    # --- Entry / hold logic (unchanged) ---
    new_buy_action <- case_when(
      choppy                                                    ~ "AVOID (CHOP)",
      squeeze_ratio < 0.85 & mom_hist > 0 & mom_delta > 0     ~ "BUY (SQZ FIRE)",
      momentum_integrity == "✅ INTACT" & rsi < 65            ~ "BUY",
      momentum_integrity == "⚠️ DECEL"                        ~ "WATCH",
      TRUE                                                      ~ "DO NOTHING"
    )

    hold_action <- ifelse(momentum_integrity == "❌ FAILED", "EXIT/AVOID", "HOLD")

    # --- CC logic ---
    cc_signal <- case_when(
      sqz_status != "RELEASE"                                            ~ "⛔ TOO SOON",
      momentum_integrity == "⚠️ DECEL" & chop_score > 0.25 ~ "✅ CC SWEET SPOT",
      TRUE                                                               ~ ""
    )

    # --- Quality score ---
    q_score <- 10L
    if (choppy)                            q_score <- q_score - 4L
    if (sqz_status == "TIGHT")             q_score <- q_score + 1L
    if (rsi > 70)                          q_score <- q_score - 2L
    if (momentum_integrity == "❌ FAILED") q_score <- q_score - 3L

    data.frame(
      Ticker       = ticker,
      Quality      = q_score,
      SQZ          = sqz_status,
      SQZ_Score    = sqz_score,
      Price        = round(price, 2),
      RSI          = round(rsi, 0),
      MomInt       = momentum_integrity,
      ATRPct       = round(atrpct, 2),
      Return_1Y    = round(ret_1y, 2),
      Action       = paste(new_buy_action, "|", hold_action),
      CC           = cc_signal,
      Chop         = round(chop_score, 2),
      # --- NEW COLUMNS ---
      SQZ_Prob     = sqz_prob,
      Bias         = bias,
      VC_Score     = vc_score,
      SQZ_Days     = sqz_days,
      SQZ_Hist_Pct = sqz_hist_pct,
      Fire_Risk    = fire_risk,
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    cat("ERROR:", ticker, "-", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(out)) {
    k <- k + 1L
    rows[[k]] <- out
  }
}

# ============================================================
# PRINT — sorted by SQZ_Prob desc, then Quality desc
# ============================================================
cat("\n--- SQUEEZE PROBABILITY ENGINE — RESULTS ---\n\n")

if (k > 0) {

  results <- do.call(rbind, rows[1:k])
  results <- results[order(-results$SQZ_Prob, -results$Quality), ]

  cat(sprintf(
    "%-6s %4s %-8s %4s %8s %4s %-10s %6s %9s %-25s %-15s %5s %5s %-5s %5s %5s %7s %9s\n",
    "Ticker","Qual","SQZ","Scr","Price","RSI","MomInt","ATR%","Ret1Y",
    "Action","CC","Chop","Prob%","Bias","VCScr","SqDys","Hist%","FireRisk"
  ))

  cat(paste(rep("-", 160), collapse = ""), "\n")

  for (i in 1:nrow(results)) {
    r <- results[i, ]
    cat(sprintf(
      "%-6s %4d %-8s %4d %8.2f %4d %-10s %6.2f %9.2f %-25s %-15s %5.2f %5d %-5s %5d %5d %7.1f %9s\n",
      r$Ticker, r$Quality, r$SQZ, r$SQZ_Score,
      r$Price, r$RSI, r$MomInt, r$ATRPct, r$Return_1Y,
      r$Action, r$CC, r$Chop,
      r$SQZ_Prob, r$Bias, r$VC_Score, r$SQZ_Days, r$SQZ_Hist_Pct,
      r$Fire_Risk
    ))
  }

} else {
  cat("No valid results.\n")
}
