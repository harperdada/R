# flpl_live_scan1.R
# NA-safe scan functions — all EMA/PPO calculations protected
# against mid-series NAs from getSymbols data gaps

suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
  library(zoo)
  library(xts)
})

# ── helper: clean a close series before any EMA/PPO work ──────────────────────
clean_close <- function(cl) {
  cl <- na.approx(cl, na.rm = FALSE)   # interpolate interior NAs
  na.omit(cl)                           # drop leading/trailing NAs
}

# ── safe PPO helper ────────────────────────────────────────────────────────────
calc_ppo <- function(cl) {
  fast <- EMA(cl, n = 12)
  slow <- EMA(cl, n = 26)
  100 * (fast - slow) / slow
}


# ============================================================
# scan_live_signals
# Returns all currently open positions across tickers
# ============================================================
scan_live_signals <- function(tickers, from = "2020-01-01") {
  out <- lapply(tickers, function(sym) {
    sig <- tryCatch(get_live_signal(sym, from = from),
                    error = function(e) NULL)
    if (is.null(sig)) return(NULL)
    sig
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) {
    cat("No live signals for this basket.\n")
    return(invisible(NULL))
  }
  live_signals <- do.call(rbind, out)
  rownames(live_signals) <- NULL
  live_signals
}


# ============================================================
# scan_entry_candidates
# Returns tickers where PPO > 0 AND Close > EMA21 today
# ============================================================
scan_entry_candidates <- function(tickers, from = "2024-01-01") {

  out <- lapply(tickers, function(sym) {
    tryCatch({
      px <- getSymbols(sym, from = from, auto.assign = FALSE)
      if (is.null(px)) return(NULL)

      close <- clean_close(Cl(px))
      if (NROW(close) < 35) return(NULL)      # need enough bars for EMA26

      ema21 <- EMA(close, n = 21)
      ppo   <- calc_ppo(close)
      today <- NROW(close)

      if (isTRUE(ppo[today] > 0) && isTRUE(close[today] > ema21[today])) {
        data.frame(
          Symbol           = sym,
          LastDate         = index(close)[today],
          Close            = as.numeric(close[today]),
          EMA21            = as.numeric(ema21[today]),
          PPO              = as.numeric(ppo[today]),
          stringsAsFactors = FALSE
        )
      } else NULL

    }, error = function(e) {
      message("scan_entry skipping ", sym, ": ", e$message)
      NULL
    })
  })

  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) { cat("No entry candidates.\n"); return(invisible(NULL)) }
  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}


# ============================================================
# scan_cross_candidates
# Returns tickers where PPO crossed above zero TODAY
# ============================================================
scan_cross_candidates <- function(tickers, from = "2024-01-01") {

  out <- lapply(tickers, function(sym) {
    tryCatch({
      px <- getSymbols(sym, from = from, auto.assign = FALSE)
      if (is.null(px)) return(NULL)

      close <- clean_close(Cl(px))
      if (NROW(close) < 35) return(NULL)

      ppo   <- calc_ppo(close)
      today <- NROW(close)

      if (isTRUE(ppo[today] > 0) && isTRUE(ppo[today - 1] <= 0)) {
        data.frame(
          Symbol           = sym,
          CrossDate        = index(close)[today],
          PPO_Today        = as.numeric(ppo[today]),
          stringsAsFactors = FALSE
        )
      } else NULL

    }, error = function(e) {
      message("scan_cross skipping ", sym, ": ", e$message)
      NULL
    })
  })

  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) { cat("No PPO cross candidates.\n"); return(invisible(NULL)) }
  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}


# ============================================================
# scan_exit_risk
# Returns open positions where Close just dropped below EMA21
# (1st day below = WARNING, exit expected tomorrow)
# ============================================================
scan_exit_risk <- function(tickers, from = "2024-01-01") {

  out <- lapply(tickers, function(sym) {
    tryCatch({
      res <- run_flpl2(sym, from = from)
      if (is.null(res$open_trade)) return(NULL)

      px    <- getSymbols(sym, from = from, auto.assign = FALSE)
      close <- clean_close(Cl(px))
      if (NROW(close) < 22) return(NULL)

      ema21 <- EMA(close, n = 21)
      today <- NROW(close)

      below_today <- isTRUE(close[today]     < ema21[today])
      below_yday  <- isTRUE(close[today - 1] < ema21[today - 1])

      if (below_today && !below_yday) {
        data.frame(
          Symbol        = sym,
          EntryDate     = res$open_trade$EntryDate,
          EntryPrice    = res$open_trade$EntryPrice,
          LastDate      = index(close)[today],
          Close         = as.numeric(close[today]),
          EMA21         = as.numeric(ema21[today]),
          UnrealizedPct = res$open_trade$UnrealizedPct,
          Warning       = "Possible exit tomorrow",
          stringsAsFactors = FALSE
        )
      } else NULL

    }, error = function(e) {
      message("scan_exit_risk skipping ", sym, ": ", e$message)
      NULL
    })
  })

  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) { cat("No exit-risk tickers.\n"); return(invisible(NULL)) }
  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}


# ============================================================
# scan_recent_exits
# Returns trades that closed within the last N trading days
# ============================================================
scan_recent_exits <- function(tickers, from = "2024-01-01", lookback_days = 5) {

  out <- lapply(tickers, function(sym) {
    tryCatch({
      res <- run_flpl2(sym, from = from)
      if (is.null(res) || is.null(res$trades) || nrow(res$trades) == 0) return(NULL)

      tr <- res$trades
      tr$ExitDate <- as.Date(tr$ExitDate)

      px <- getSymbols(sym, from = from, auto.assign = FALSE)
      if (is.null(px)) return(NULL)

      last_mkt_date <- as.Date(last(index(px)))
      cutoff  <- last_mkt_date - lookback_days
      recent  <- tr[tr$ExitDate >= cutoff, , drop = FALSE]
      if (nrow(recent) == 0) return(NULL)

      if (!("Symbol" %in% names(recent))) recent$Symbol <- sym

      keep <- c("Symbol","EntryDate","EntryPrice","ExitDate",
                "ExitPrice","ReturnPct","ExitReason")
      keep <- keep[keep %in% names(recent)]
      recent[, keep, drop = FALSE]

    }, error = function(e) {
      message("scan_recent_exits skipping ", sym, ": ", e$message)
      NULL
    })
  })

  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) { cat("No recent exits.\n"); return(invisible(NULL)) }
  df <- do.call(rbind, out)
  rownames(df) <- NULL
  if ("ExitDate" %in% names(df)) df <- df[order(df$ExitDate, df$Symbol), ]
  df
}
