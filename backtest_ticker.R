	#Long Entry when ALL three happen on the same day:PPO (12,26) crosses above zero (fast EMA crosses above slow EMA)
	#Price is above the 21-day EMA
	#(Implicit: we are flat)

	#Exit when:Price closes below the 21-day EMA for two consecutive days




	library(quantmod)
	library(TTR)
	library(xts)
	library(zoo)

	run_flpl2 <- function(symbol  = "ASTS",
			      from    = "2015-01-01",
			      to      = Sys.Date(),
			      fastPPO = 12,
			      slowPPO = 26,
			      emaLen  = 21,
			      startEq = 100000,
			      riskFrac = 1.0) {

	  px      <- getSymbols(symbol, from = from, to = to, auto.assign = FALSE)
          px      <- na.omit(px)
	  closePx <- Cl(px)

	  fastEMA <- EMA(closePx, n = fastPPO)
	  slowEMA <- EMA(closePx, n = slowPPO)
	  ppo     <- 100 * (fastEMA - slowEMA) / slowEMA
	  ema21   <- EMA(closePx, n = emaLen)

	  # ENTRY: PPO crosses above 0 AND close > EMA21
	  entrySignal <- (ppo > 0) & (Lag(ppo) <= 0) & (closePx > ema21)

	  # EXIT: close < EMA21 two days in a row
	  belowEma    <- closePx < ema21
	  exitSignal  <- belowEma & Lag(belowEma, 1)

	  entrySignal[is.na(entrySignal)] <- FALSE
	  exitSignal[is.na(exitSignal)]   <- FALSE

	  dates    <- index(px)
	  n        <- NROW(px)
	  position <- 0
	  shares   <- 0
	  cash     <- startEq
	  equity   <- rep(NA_real_, n)

	  tradeLog <- data.frame(
	    EntryDate  = as.Date(character()),
	    EntryPrice = numeric(),
	    ExitDate   = as.Date(character()),
	    ExitPrice  = numeric(),
	    ReturnPct  = numeric(),
	    stringsAsFactors = FALSE
	  )

	  entryPrice <- NA_real_
	  entryDate  <- as.Date(NA)
	  warmup     <- max(fastPPO, slowPPO, emaLen) + 2

	  for (i in seq_len(n)) {
	    price <- as.numeric(closePx[i])
	    d     <- dates[i]

	    if (i > warmup) {

	      # open long
	      if (position == 0 && entrySignal[i]) {
		equityNow <- cash
		shares    <- floor((equityNow * riskFrac) / price)
		if (shares > 0) {
		  cash       <- cash - shares * price
		  position   <- 1
		  entryPrice <- price
		  entryDate  <- d
		}
	      }

	      # close long
	      if (position == 1 && exitSignal[i]) {
		cash <- cash + shares * price
		retPct <- (price / entryPrice - 1) * 100
		tradeLog <- rbind(
		  tradeLog,
		  data.frame(
		    EntryDate  = entryDate,
		    EntryPrice = entryPrice,
		    ExitDate   = d,
		    ExitPrice  = price,
		    ReturnPct  = retPct,
		    stringsAsFactors = FALSE
		  )
		)
		position   <- 0
		shares     <- 0
		entryPrice <- NA_real_
		entryDate  <- as.Date(NA)
	      }
	    }

	    equity[i] <- cash + shares * price
	  }

	  eqSeries <- xts(equity, order.by = dates)
	  colnames(eqSeries) <- "Equity"

	  ### NEW: if we're still in a trade at the end, return it as "open_trade"
	  open_trade <- NULL
	  if (position == 1 && !is.na(entryPrice)) {
	    lastPrice <- as.numeric(last(closePx))
	    lastDate  <- as.Date(last(index(closePx)))
	    unrealPct <- (lastPrice / entryPrice - 1) * 100

	    open_trade <- data.frame(
	      Symbol        = symbol,
	      EntryDate     = entryDate,
	      EntryPrice    = entryPrice,
	      LastDate      = lastDate,
	      LastPrice     = lastPrice,
	      UnrealizedPct = unrealPct,
	      stringsAsFactors = FALSE
	    )
	  }

	  list(
	    symbol     = symbol,
	    trades     = tradeLog,   # closed trades
	    equity     = eqSeries,
	    open_trade = open_trade  # NULL if flat, 1-row data.frame if long
	  )
	}

	## Demo when sourcing flpl4.R (you already had this for app, keep or remove as you like)
	res_demo <- run_flpl2("ASTS", from = "2024-01-01")
	print(res_demo$trades)
	cat("\nTotal trades:", nrow(res_demo$trades), "\n")
	cat("Final equity:", round(as.numeric(last(res_demo$equity)), 2), "\n")
	cat("Total return (%):",
	    round((as.numeric(last(res_demo$equity)) / 100000 - 1) * 100, 2), "\n")

	get_live_signal <- function(symbol, from = "2020-01-01") {
	  res <- run_flpl2(symbol, from = from)
	  res$open_trade
	}

	# example:
	#get_live_signal("ASTS", from = "2020-01-01")
	#get_live_signal("ASTS", from = "2024-01-01")
	#get_live_signal("ASTS", from = "2024-01-01")

tickers <- c("ASTS", "ASTS", "COST", "ASTS", "ASTS")

live <- lapply(tickers, function(sym) {
  sig <- get_live_signal(sym, from = "2020-01-01")
  if (is.null(sig)) return(NULL)
  sig
})

# Drop NULLs and row-bind the rest
live_signals <- do.call(rbind, live)
print(live_signals)

