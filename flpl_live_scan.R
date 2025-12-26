scan_live_signals <- function(tickers,
                              from = "2020-01-01") {
  out <- lapply(tickers, function(sym) {
    sig <- get_live_signal(sym, from = from)
    if (is.null(sig)) return(NULL)
    sig
  })

  # drop NULLs
  out <- out[!vapply(out, is.null, logical(1))]

  if (length(out) == 0) {
    cat("No live signals for this basket.\n")
    return(invisible(NULL))
  }

  live_signals <- do.call(rbind, out)
  rownames(live_signals) <- NULL
  live_signals
}

scan_entry_candidates <- function(tickers, from = "2024-01-01") {

  out <- lapply(tickers, function(sym) {
    px <- tryCatch(getSymbols(sym, from = from, auto.assign = FALSE),
                   error = function(e) return(NULL))
    if (is.null(px)) return(NULL)

    close <- Cl(px)
    ema21 <- EMA(close, n = 21)

    fastEMA <- EMA(close, n = 12)
    slowEMA <- EMA(close, n = 26)
    ppo     <- 100 * (fastEMA - slowEMA) / slowEMA

    today <- NROW(close)

    # require both
    if (ppo[today] > 0 && close[today] > ema21[today]) {
      data.frame(
        Symbol     = sym,
        LastDate   = index(close)[today],
        Close      = as.numeric(close[today]),
        EMA21      = as.numeric(ema21[today]),
        PPO        = as.numeric(ppo[today]),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) {
    cat("No entry candidates.\n")
    return(invisible(NULL))
  }

  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}

scan_cross_candidates <- function(tickers, from = "2024-01-01") {

  out <- lapply(tickers, function(sym) {
    px <- tryCatch(getSymbols(sym, from = from, auto.assign = FALSE),
                   error = function(e) return(NULL))
    if (is.null(px)) return(NULL)

    close <- Cl(px)
    fastEMA <- EMA(close, n = 12)
    slowEMA <- EMA(close, n = 26)
    ppo     <- 100 * (fastEMA - slowEMA) / slowEMA

    today <- NROW(close)

    # PPO cross above zero
    if (ppo[today] > 0 && ppo[today-1] <= 0) {
      data.frame(
        Symbol   = sym,
        CrossDate = index(close)[today],
        PPO_Today = as.numeric(ppo[today]),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) {
    cat("No PPO cross candidates.\n")
    return(invisible(NULL))
  }

  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}


scan_exit_risk <- function(tickers, from = "2024-01-01") {

  out <- lapply(tickers, function(sym) {
    res <- tryCatch(run_flpl2(sym, from = from),
                    error = function(e) return(NULL))
    if (is.null(res$open_trade)) return(NULL)

    # ticker is in an active trade
    px  <- getSymbols(sym, from = from, auto.assign = FALSE)
    close <- Cl(px)
    ema21 <- EMA(close, n = 21)

    today <- NROW(close)

    below_today  <- close[today] < ema21[today]
    below_yday   <- close[today-1] < ema21[today-1]

    # exit tomorrow if below two days in a row
    if (below_today && !below_yday) {
      # only today's below → 1 day below = WARNING
      data.frame(
        Symbol         = sym,
        EntryDate      = res$open_trade$EntryDate,
        EntryPrice     = res$open_trade$EntryPrice,
        LastDate       = index(close)[today],
        Close          = as.numeric(close[today]),
        EMA21          = as.numeric(ema21[today]),
        UnrealizedPct  = res$open_trade$UnrealizedPct,
        Warning        = "Possible exit tomorrow",
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) {
    cat("No exit-risk tickers.\n")
    return(invisible(NULL))
  }

  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}

scan_recent_exits <- function(tickers,
                              from = "2024-01-01",
                              lookback_days = 5) {

  out <- lapply(tickers, function(sym) {

    res <- tryCatch(run_flpl2(sym, from = from),
                    error = function(e) return(NULL))
    if (is.null(res) || is.null(res$trades) || nrow(res$trades) == 0) return(NULL)

    tr <- res$trades
    tr$ExitDate <- as.Date(tr$ExitDate)  # safety

    # Find the last available market date for this symbol (so weekends don't mess you up)
    px <- tryCatch(getSymbols(sym, from = from, auto.assign = FALSE),
                   error = function(e) return(NULL))
    if (is.null(px)) return(NULL)

    last_mkt_date <- as.Date(last(index(px)))
    cutoff <- last_mkt_date - lookback_days

    recent <- tr[tr$ExitDate >= cutoff, , drop = FALSE]
    if (nrow(recent) == 0) return(NULL)

    # Add symbol column if your tradeLog doesn't already include it
    if (!("Symbol" %in% names(recent))) recent$Symbol <- sym

    # Keep only columns that exist (works whether or not you have ExitReason)
    keep <- c("Symbol","EntryDate","EntryPrice","ExitDate","ExitPrice","ReturnPct","ExitReason")
    keep <- keep[keep %in% names(recent)]

    recent <- recent[, keep, drop = FALSE]
    recent
  })

  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) {
    cat("No recent exits.\n")
    return(invisible(NULL))
  }

  df <- do.call(rbind, out)
  rownames(df) <- NULL

  # Sort nicely if ExitDate exists
  if ("ExitDate" %in% names(df)) df <- df[order(df$ExitDate, df$Symbol), ]

  df
}

