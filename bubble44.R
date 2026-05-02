# ============================
# Single Ticker Deep Dive
# Squeeze Probability Engine — Last 60 Bars
# Fix: RELEASE gate on all BUY signals
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

# --- Squeeze ratio series ---
calc_squeeze_ratio_series <- function(close, hlc, n_bb = 20, n_atr = 20, kc_mult = 1.5) {
  bb   <- BBands(close, n_bb)
  atr_ <- ATR(hlc, n_atr)$atr
  sma_ <- SMA(close, n_bb)
  bb_w <- as.numeric(bb$up) - as.numeric(bb$dn)
  kc_w <- 2 * kc_mult * as.numeric(atr_)
  bb_w / kc_w
}

calc_squeeze_days <- function(ratio_series) {
  r <- rev(as.numeric(ratio_series))
  r <- r[!is.na(r)]
  count <- 0L
  for (v in r) { if (v < 1.0) count <- count + 1L else break }
  count
}

calc_squeeze_hist_pct <- function(ratio_series) {
  r <- as.numeric(ratio_series)
  r <- r[!is.na(r)]
  if (length(r) == 0) return(NA_real_)
  mean(r < 1.0) * 100
}

calc_vc_score <- function(ratio_series, lookback = 252) {
  r <- as.numeric(ratio_series)
  r <- r[!is.na(r)]
  if (length(r) < 20) return(NA_real_)
  window <- tail(r, lookback)
  cur    <- tail(r, 1)
  round(mean(window >= cur, na.rm = TRUE) * 100, 0)
}

calc_bias <- function(mom_hist, sma50_last, sma50_prev) {
  sma_slope <- sma50_last - sma50_prev
  case_when(
    mom_hist > 0 & sma_slope > 0 ~ "UP",
    mom_hist < 0 & sma_slope < 0 ~ "DOWN",
    TRUE                          ~ "FLAT"
  )
}

calc_sqz_prob <- function(squeeze_ratio, vc_score, sqz_days, sqz_hist_pct,
                           mom_hist, chop_score) {
  tightness      <- max(0, min(100, (1 - squeeze_ratio) * 130))
  vc             <- if (!is.na(vc_score)) vc_score else 50
  duration_score <- min(100, sqz_days * 5)
  base           <- if (!is.na(sqz_hist_pct)) sqz_hist_pct else 20
  chop_penalty   <- chop_score * 80
  mom_bonus      <- if (!is.na(mom_hist) && mom_hist > 0) 10 else 0
  raw <- 0.40 * tightness +
         0.25 * vc +
         0.20 * duration_score +
         0.10 * base +
         mom_bonus - chop_penalty
  round(max(0, min(100, raw)), 0)
}

calc_fire_risk <- function(sqz_prob, sqz_days, mom_hist, mom_delta) {
  case_when(
    sqz_prob >= 75 & sqz_days >= 8 & mom_hist > 0 & mom_delta > 0 ~ "HIGH",
    sqz_prob >= 50                                                  ~ "MED",
    TRUE                                                            ~ "LOW"
  )
}

# ============================================================
# ACTION LOGIC — with RELEASE gate
# ============================================================
calc_action <- function(sqz_status, choppy, ratio, mom_hist, mom_delta,
                         mom_int, rsi) {
  case_when(
    choppy                                              ~ "AVOID(CHOP)",
    sqz_status == "RELEASE"                            ~ "DO NOTHING",     # gate
    ratio < 0.85 & mom_hist > 0 & mom_delta > 0       ~ "BUY(SQZ FIRE)",
    mom_int == "INTACT" & rsi < 65                     ~ "BUY",
    mom_int == "DECEL"                                 ~ "WATCH",
    TRUE                                               ~ "DO NOTHING"
  )
}

# ============================================================
# ASK FOR TICKER
# ============================================================
ticker <- toupper(trimws(readline(prompt = "Enter ticker: ")))
n_days <- 60

cat("\n--- LOADING", ticker, "---\n\n")

df <- tryCatch(
  getSymbols(ticker, src = "yahoo", auto.assign = FALSE, from = Sys.Date() - 600),
  error = function(e) { cat("ERROR fetching", ticker, ":", conditionMessage(e), "\n"); NULL }
)

if (is.null(df) || NROW(df) < 80) {
  cat("Not enough data for", ticker, "\n")
  quit(save = "no")
}

colnames(df) <- c("Open","High","Low","Close","Volume","Adjusted")
close  <- Cl(df)
hlc_df <- HLC(df)

# --- Full history indicators ---
sma50  <- SMA(close, 50)
sma20  <- SMA(close, 20)
rsi14  <- RSI(close, 14)
atr14  <- ATR(hlc_df, 14)$atr
atr20  <- ATR(hlc_df, 20)$atr
bb     <- BBands(close, 20)

highest_20 <- runMax(df$High, 20)
lowest_20  <- runMin(df$Low,  20)
mid        <- (((highest_20 + lowest_20) / 2) + sma20) / 2
y          <- close - mid
mom_series <- roll_linreg_value(y, 20)

ratio_series  <- calc_squeeze_ratio_series(close, hlc_df)
sqz_hist_pct  <- round(calc_squeeze_hist_pct(ratio_series), 1)
vc_score_full <- calc_vc_score(ratio_series)
ret_1y        <- calc_return_1y(df)

spread_full <- close - sma50

# ============================================================
# PRINT HEADER
# ============================================================
cat(sprintf("=== %s — LAST %d TRADING DAYS ===\n\n", ticker, n_days))

cat(sprintf(
  "%-12s %8s %8s %8s %8s %4s %-10s %6s %8s %8s %6s %-8s %5s %-5s %5s %5s %7s %13s\n",
  "Date","Open","High","Low","Close","RSI","MomInt","ATR%",
  "SQZ","Ratio","Prob%","FireRisk","VCScr","Bias","SqDys","Chop","Ret1Y","Action"
))

cat(paste(rep("-", 195), collapse = ""), "\n")

n_total   <- NROW(df)
idx_range <- max(1, n_total - n_days + 1):n_total

for (i in idx_range) {

  if (i < 55) next

  bar_date <- as.character(index(df)[i])

  o  <- round(as.numeric(df$Open[i]),  2)
  h  <- round(as.numeric(df$High[i]),  2)
  l  <- round(as.numeric(df$Low[i]),   2)
  cl <- round(as.numeric(close[i]),    2)

  rsi_i    <- round(as.numeric(rsi14[i]), 0)
  atr14_i  <- as.numeric(atr14[i])
  atrpct_i <- round((atr14_i / cl) * 100, 2)

  sma50_i  <- as.numeric(sma50[i])
  sma50_p  <- if (i > 1) as.numeric(sma50[i-1]) else sma50_i
  atr20_i  <- as.numeric(atr20[i])

  bb_up_i  <- as.numeric(bb$up[i])
  bb_dn_i  <- as.numeric(bb$dn[i])
  kc_w_i   <- 2 * 1.5 * atr20_i
  bb_w_i   <- bb_up_i - bb_dn_i
  ratio_i  <- bb_w_i / kc_w_i

  sqz_i <- dplyr::case_when(
    ratio_i < 0.75 ~ "TIGHT",
    ratio_i < 1.00 ~ "SQUEEZE",
    TRUE           ~ "RELEASE"
  )

  mom_i       <- as.numeric(mom_series[i])
  mom_prev_i  <- if (i > 1) as.numeric(mom_series[i-1]) else mom_i
  mom_delta_i <- mom_i - mom_prev_i

  mom_int_i <- dplyr::case_when(
    mom_i > 0 & mom_delta_i > 0  ~ "INTACT",
    mom_i > 0 & mom_delta_i <= 0 ~ "DECEL",
    mom_i < 0 & mom_delta_i <= 0 ~ "FAILED",
    mom_i < 0 & mom_delta_i > 0  ~ "RECOVER",
    TRUE                          ~ "—"
  )

  chop_start <- max(1, i - 29)
  spread_i   <- as.numeric(spread_full[chop_start:i])
  fake_i     <- sum(diff(sign(spread_i)) != 0, na.rm = TRUE)
  chop_i     <- round(fake_i / 30, 2)
  choppy_i   <- chop_i > 0.25

  ratio_to_i <- as.numeric(ratio_series[1:i])
  sqz_days_i <- 0L
  for (v in rev(ratio_to_i)) {
    if (!is.na(v) && v < 1.0) sqz_days_i <- sqz_days_i + 1L else break
  }

  bias_i <- calc_bias(mom_i, sma50_i, sma50_p)

  prob_i <- calc_sqz_prob(ratio_i, vc_score_full, sqz_days_i,
                           sqz_hist_pct, mom_i, chop_i)

  fire_i <- calc_fire_risk(prob_i, sqz_days_i, mom_i, mom_delta_i)

  # --- FIXED action with RELEASE gate ---
  action_i <- calc_action(sqz_i, choppy_i, ratio_i, mom_i,
                           mom_delta_i, mom_int_i, rsi_i)

  cat(sprintf(
    "%-12s %8.2f %8.2f %8.2f %8.2f %4d %-10s %6.2f %8s %8.2f %6d %-8s %5d %-5s %5d %5.2f %7.2f %13s\n",
    bar_date, o, h, l, cl, rsi_i, mom_int_i, atrpct_i,
    sqz_i, ratio_i, prob_i, fire_i, vc_score_full,
    bias_i, sqz_days_i, chop_i, ret_1y, action_i
  ))
}

# ============================================================
# SUMMARY BLOCK — current bar
# ============================================================
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat(sprintf("  SUMMARY — %s as of %s\n", ticker, as.character(index(df)[n_total])))
cat(paste(rep("=", 80), collapse = ""), "\n\n")

last_ratio   <- as.numeric(tail(ratio_series[!is.na(ratio_series)], 1))
last_sqz     <- dplyr::case_when(
  last_ratio < 0.75 ~ "TIGHT",
  last_ratio < 1.00 ~ "SQUEEZE",
  TRUE              ~ "RELEASE"
)
last_mom     <- as.numeric(last(mom_series))
last_mom_p   <- as.numeric(tail(mom_series, 2)[1])
last_delta   <- last_mom - last_mom_p
last_sma50   <- as.numeric(last(sma50))
last_sma50_p <- as.numeric(tail(sma50, 2)[1])
last_bias    <- calc_bias(last_mom, last_sma50, last_sma50_p)
last_sqzdays <- calc_squeeze_days(ratio_series)
last_chop_v  <- as.numeric(tail(spread_full, 30))
last_chop_sc <- round(sum(diff(sign(as.numeric(last_chop_v))) != 0, na.rm = TRUE) / 30, 2)
last_choppy  <- last_chop_sc > 0.25
last_prob    <- calc_sqz_prob(last_ratio, vc_score_full, last_sqzdays,
                               sqz_hist_pct, last_mom, last_chop_sc)
last_fire    <- calc_fire_risk(last_prob, last_sqzdays, last_mom, last_delta)
last_rsi     <- round(as.numeric(last(rsi14)), 0)
last_price   <- round(as.numeric(last(close)), 2)
last_atrpct  <- round((as.numeric(last(atr14)) / last_price) * 100, 2)

last_mom_int <- dplyr::case_when(
  last_mom > 0 & last_delta > 0  ~ "INTACT",
  last_mom > 0 & last_delta <= 0 ~ "DECEL",
  last_mom < 0 & last_delta <= 0 ~ "FAILED",
  last_mom < 0 & last_delta > 0  ~ "RECOVER",
  TRUE                            ~ "—"
)

# --- FIXED summary action with RELEASE gate ---
last_action <- calc_action(last_sqz, last_choppy, last_ratio, last_mom,
                            last_delta, last_mom_int, last_rsi)

cat(sprintf("  Price        : $%.2f\n",       last_price))
cat(sprintf("  RSI          : %d\n",           last_rsi))
cat(sprintf("  ATR%%         : %.2f%%\n",      last_atrpct))
cat(sprintf("  1Y Return    : %.2f%%\n",       ret_1y))
cat(sprintf("  SQZ Status   : %s (ratio %.3f)\n", last_sqz, last_ratio))
cat(sprintf("  Prob%%        : %d\n",          last_prob))
cat(sprintf("  VC Score     : %d\n",           vc_score_full))
cat(sprintf("  Bias         : %s\n",           last_bias))
cat(sprintf("  SQZ Days     : %d\n",           last_sqzdays))
cat(sprintf("  Hist Squeeze : %.1f%%\n",       sqz_hist_pct))
cat(sprintf("  Chop         : %.2f\n",         last_chop_sc))
cat(sprintf("  Fire Risk    : %s\n",           last_fire))
cat(sprintf("  Action       : %s\n\n",         last_action))
