rm(list=ls())
library(quantmod)
library(TTR)

# -----------------------------
# PARAMETERS
# -----------------------------
symbol <- "AMD"
from <- "2024-01-01"

lookback_low_days <- 30   # lookback window to find "local low" before reclaim
lookahead_high_days <- 60 # forward window to find "post reclaim high"

horizons <- c(5, 10, 20, 60)  # forward return horizons (trading days)

# -----------------------------
# LOAD DATA
# -----------------------------
px <- suppressWarnings(getSymbols(symbol, from=from, src="yahoo", auto.assign=FALSE))
close <- Cl(px)

sma50 <- SMA(close, n=50)

df <- data.frame(
  Date  = as.Date(index(close)),
  Close = as.numeric(close),
  SMA50 = as.numeric(sma50)
)

df <- df[!is.na(df$SMA50), ]
n <- nrow(df)

# -----------------------------
# FIND "RECLAIM" EVENTS:
# reclaim day = Close crosses from below SMA50 to above SMA50 (close-based)
# -----------------------------
below <- df$Close < df$SMA50
reclaim <- (below[-1] == FALSE) & (below[-length(below)] == TRUE) & (df$Close[-1] > df$SMA50[-1])
reclaim_idx <- which(reclaim) + 1  # +1 because reclaim computed on shifted vectors

# -----------------------------
# HELPERS
# -----------------------------
safe_forward_return <- function(i, k) {
  j <- i + k
  if (j > n) return(NA_real_)
  (df$Close[j] / df$Close[i]) - 1
}

# "Local low" before reclaim: min close in prior lookback_low_days
local_low_before <- function(i, lookback) {
  a <- max(1, i - lookback)
  min(df$Close[a:i], na.rm=TRUE)
}

# "Post high" after reclaim: max close in next lookahead_high_days
post_high_after <- function(i, lookahead) {
  b <- min(n, i + lookahead)
  max(df$Close[i:b], na.rm=TRUE)
}

# Portion of swing captured AFTER reclaim:
# (High - ReclaimClose) / (High - LocalLow)
portion_after_reclaim <- function(i, lookback, lookahead) {
  low  <- local_low_before(i, lookback)
  high <- post_high_after(i, lookahead)
  rc   <- df$Close[i]
  denom <- (high - low)
  if (is.na(denom) || denom <= 0) return(NA_real_)
  (high - rc) / denom
}

# -----------------------------
# BUILD EVENT TABLE
# -----------------------------
out <- data.frame(
  Date = df$Date[reclaim_idx],
  ReclaimClose = df$Close[reclaim_idx],
  SMA50 = df$SMA50[reclaim_idx]
)

# Forward returns
for (k in horizons) {
  out[[paste0("FwdRet_", k, "d")]] <- sapply(reclaim_idx, safe_forward_return, k=k)
}

# Swing decomposition
out$LocalLow_lookback <- sapply(reclaim_idx, local_low_before, lookback=lookback_low_days)
out$PostHigh_lookahead <- sapply(reclaim_idx, post_high_after, lookahead=lookahead_high_days)
out$PortionMoveAfterReclaim <- sapply(
  reclaim_idx,
  portion_after_reclaim,
  lookback=lookback_low_days,
  lookahead=lookahead_high_days
)

# Clean formatting
pct_cols <- grep("^FwdRet_|PortionMoveAfterReclaim$", names(out), value=TRUE)
out[pct_cols] <- lapply(out[pct_cols], function(x) round(100*x, 1))

out$ReclaimClose <- round(out$ReclaimClose, 2)
out$SMA50 <- round(out$SMA50, 2)
out$LocalLow_lookback <- round(out$LocalLow_lookback, 2)
out$PostHigh_lookahead <- round(out$PostHigh_lookahead, 2)

# -----------------------------
# SUMMARY STATS (THE "QUANTIFY" PART)
# -----------------------------
cat("\n===== AMD SMA50 RECLAIM STUDY =====\n")
cat("Events found:", nrow(out), "\n")
cat("Local low lookback:", lookback_low_days, "days\n")
cat("Post high lookahead:", lookahead_high_days, "days\n\n")

summ <- function(x) c(
  mean=mean(x, na.rm=TRUE),
  median=median(x, na.rm=TRUE),
  p25=quantile(x, 0.25, na.rm=TRUE),
  p75=quantile(x, 0.75, na.rm=TRUE)
)

for (k in horizons) {
  col <- paste0("FwdRet_", k, "d")
  cat("Forward return", k, "d (%):\n")
  print(round(summ(out[[col]]), 1))
  cat("\n")
}

cat("Portion of swing remaining AFTER reclaim (% of Low→High move, over lookahead window):\n")
print(round(summ(out$PortionMoveAfterReclaim), 1))
cat("\n")

# -----------------------------
# PRINT TOP OF TABLE
# -----------------------------
cat("Sample events (first 15 rows):\n")
print(head(out, 15), row.names = FALSE)

# If you want the full table:
# View(out)
# write.csv(out, "AMD_reclaim_study.csv", row.names=FALSE)

