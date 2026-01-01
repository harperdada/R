# ============================
# bubble_one.R — ONE TICKER, HOURLY (via Python yfinance CSV)
# - Avoids Yahoo Chart API 429s
# - Fetches hourly bars using your .venv python + yf_1h.py
# - Computes ALL indicators on hourly bars (TOS-style squeeze momentum logic)
# ============================

suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
  library(zoo)
  library(xts)
  library(dplyr)
})

# ---------- CONFIG ----------
PY   <- path.expand("~/Python/.venv/bin/python")
YF1H <- path.expand("~/Python/yf_1h.py")
TMP  <- "/tmp"                          # where CSV gets saved

Sys.setenv(TZ = "America/New_York")     # to reduce TZ weirdness
options(xts_check_TZ = FALSE)

# ---------- Helper: rolling linreg VALUE at last bar ----------
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
      xx <- 0:(length(yy) - 1)
      fit <- stats::lm(yy ~ xx)
      co  <- stats::coef(fit)
      if (any(!is.finite(co))) return(NA_real_)
      intercept <- co[1]
      slope     <- co[2]
      intercept + slope * (length(yy) - 1)
    }
  )

  xts::xts(as.numeric(vals), order.by = zoo::index(y))
}

# ---------- Helper: read the yfinance CSV (your fixed logic) ----------
read_yf_csv <- function(path) {
  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

  # drop yfinance's 2 junk rows
  raw <- raw[-c(1,2), , drop = FALSE]
  names(raw)[1] <- "Datetime"

  need <- c("Datetime","Open","High","Low","Close","Volume")
  missing <- setdiff(need, names(raw))
  if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse=", "))

  raw <- raw[, need, drop = FALSE]

  # "+00:00" -> "+0000"
  raw$Datetime <- trimws(raw$Datetime)
  raw$Datetime <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", raw$Datetime)

  idx <- as.POSIXct(raw$Datetime, format="%Y-%m-%d %H:%M:%S%z", tz="UTC")

  good_time <- !is.na(idx)
  raw <- raw[good_time, , drop = FALSE]
  idx <- idx[good_time]

  if (NROW(raw) == 0) stop("Datetime parsing failed (0 rows).")

  for (cname in c("Open","High","Low","Close","Volume")) {
    raw[[cname]] <- as.numeric(raw[[cname]])
  }

  good_ohlc <- is.finite(raw$Open) & is.finite(raw$High) & is.finite(raw$Low) & is.finite(raw$Close)
  raw <- raw[good_ohlc, , drop = FALSE]
  idx <- idx[good_ohlc]

  if (NROW(raw) == 0) stop("All rows removed after OHLC cleanup (0 rows left).")

  m <- as.matrix(raw[, c("Open","High","Low","Close","Volume")])
  mode(m) <- "numeric"

  x <- xts(m, order.by = idx)

  # convert to New York time (so 09:30 etc)
  index(x) <- as.POSIXct(format(index(x), tz="America/New_York", usetz=TRUE),
                         tz="America/New_York")
  x
}

# ---------- Helper: run python fetcher ----------
fetch_hourly_csv <- function(ticker) {
  ticker <- toupper(trimws(ticker))
  outcsv <- file.path(TMP, paste0(ticker, "_1h.csv"))

  cmd <- sprintf('%s %s %s %s',
                 shQuote(PY),
                 shQuote(YF1H),
                 shQuote(ticker),
                 shQuote(outcsv))

  cat("Running:", cmd, "\n")
  rc <- system(cmd)

  if (rc != 0) stop("Python fetch failed (exit code ", rc, ").")
  if (!file.exists(outcsv)) stop("CSV not created: ", outcsv)

  outcsv
}

# ---------- MAIN ----------
ticker <- readline("Enter ticker (e.g., AAPL): ")
ticker <- toupper(trimws(ticker))
if (ticker == "") stop("No ticker entered.")

cat("--- TECHNICAL SCAN (ONE TICKER, HOURLY via yfinance):", as.character(Sys.Date()), "---\n")
cat("Ticker:", ticker, "\n")

csv_path <- fetch_hourly_csv(ticker)
df <- read_yf_csv(csv_path)

# If you want RTH only (optional):
df <- df["T09:30/T16:00"]

close <- df$Close

# --- Indicators (HOURLY) ---
sma50 <- SMA(close, n = 50)
sma20 <- SMA(close, n = 20)
rsi14 <- RSI(close, n = 14)

atr14 <- ATR(HLC(df), n = 14)$atr
atr20 <- ATR(HLC(df), n = 20)$atr

current_price <- as.numeric(last(close))
current_rsi   <- as.numeric(last(rsi14))
current_atr14 <- as.numeric(last(atr14))
current_atrpct <- (current_atr14 / current_price) * 100

# --- Squeeze (BB inside KC) ---
bb20 <- BBands(close, n = 20, sd = 2)
sma20_last <- as.numeric(last(sma20))
atr20_last <- as.numeric(last(atr20))
bb_upper   <- as.numeric(last(bb20$up))
bb_lower   <- as.numeric(last(bb20$dn))

kc_upper <- sma20_last + (1.5 * atr20_last)
kc_lower <- sma20_last - (1.5 * atr20_last)

is_squeezing <- (bb_upper < kc_upper) && (bb_lower > kc_lower)
sqz_status   <- if (is_squeezing) "SQUEEZE" else "RELEASE"

# --- Momentum (TOS-style squeeze momentum) ---
highest_20 <- runMax(df$High, n = 20)
lowest_20  <- runMin(df$Low,  n = 20)
mid_point  <- (((highest_20 + lowest_20) / 2) + sma20) / 2
y <- close - mid_point

mom_series <- roll_linreg_value(y, n = 20)
mom_hist <- as.numeric(last(mom_series))
prev_mom <- as.numeric(tail(mom_series, 2)[1])
mom_delta <- mom_hist - prev_mom
mom_accel <- mom_delta > 0

mom_integrity <- dplyr::case_when(
  mom_hist > 0  & mom_accel  ~ "✅ INTACT",
  mom_hist > 0  & !mom_accel ~ "⚠️ DECEL",
  mom_hist < 0  & !mom_accel ~ "❌ FAILED",
  mom_hist < 0  & mom_accel  ~ "🟨 RECOVER",
  TRUE ~ "—"
)

# --- Chop / fakeout count based on Close vs SMA50 sign flips (last 30 bars) ---
spread <- close - sma50
last30 <- tail(spread, 30)
fake_count <- sum(diff(sign(as.numeric(last30))) != 0, na.rm = TRUE)

action <- dplyr::case_when(
  fake_count > 5 ~ "AVOID (CHOP)",
  is_squeezing & mom_accel ~ "BUY (SQZ FIRE)",
  mom_integrity == "✅ INTACT" & current_rsi < 65 ~ "BUY",
  mom_integrity == "✅ INTACT" & current_rsi >= 65 ~ "DON'T CHASE",
  mom_integrity == "🟨 RECOVER" ~ "WATCH RECLAIM",
  TRUE ~ "DO NOTHING"
)

hold_action <- ifelse(mom_integrity == "❌ FAILED", "EXIT/AVOID", "HOLD")

# ---------- OUTPUT ----------
cat("\nRESULT:\n")
cat(sprintf("  Price:   %.4f\n", current_price))
cat(sprintf("  RSI14:   %.0f\n", current_rsi))
cat(sprintf("  ATR14%%:  %.2f%%\n", current_atrpct))
cat(sprintf("  Squeeze: %s\n", sqz_status))
cat(sprintf("  MomInt:  %s\n", mom_integrity))
cat(sprintf("  Fake30:  %d\n", fake_count))
cat(sprintf("  Action:  %s | %s\n", action, hold_action))
cat(sprintf("  Bars:    %d (hourly)\n", NROW(df)))
cat(sprintf("  From:    %s\n", format(first(index(df)), "%Y-%m-%d %H:%M:%S %Z")))
cat(sprintf("  To:      %s\n", format(last(index(df)),  "%Y-%m-%d %H:%M:%S %Z")))
cat(sprintf("  CSV:     %s\n", csv_path))

