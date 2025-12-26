library(ggplot2)
library(tidyverse)
library(quantmod)
library(TTR)
library(patchwork)

# --- INPUT ---
ticker1 <- toupper(trimws(readline("Enter first ticker: ")))
ticker2 <- toupper(trimws(readline("Enter second ticker: ")))
start_date <- "2024-10-01"

# --- FETCH ---
getSymbols(c(ticker1, ticker2), src="yahoo", from=start_date)

make_df <- function(tk) {
  raw <- get(tk)
  df <- data.frame(
    Date   = index(raw),
    Price  = as.numeric(Ad(raw)),
    Volume = as.numeric(Vo(raw))
  ) %>% na.omit()

  base_price <- df$Price[1]

  df %>%
    mutate(
      Ticker = tk,
      NormPrice = (Price / base_price) * 100,
      SMA50 = SMA(NormPrice, 50),
      RSI = RSI(Price, 14)
    ) %>%
    drop_na()
}

df1 <- make_df(ticker1)
df2 <- make_df(ticker2)
df  <- bind_rows(df1, df2)

# --- PRICE PANEL ---
p_price <- ggplot(df, aes(x = Date)) +
  geom_line(aes(y = NormPrice, color = Ticker), linewidth = 1) +
  geom_line(aes(y = SMA50, color = Ticker), linetype="dashed", alpha=0.5) +
  labs(
    title = paste(ticker1, "vs", ticker2, "— Normalized Price (Base = 100)"),
    y = "Normalized Price"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# --- RSI PANEL ---
p_rsi <- ggplot(df, aes(x = Date, y = RSI, color = Ticker)) +
  geom_line(alpha = 0.8) +
  geom_hline(yintercept = c(30, 70), linetype="dashed", color="gray") +
  labs(y = "RSI") +
  theme_minimal() +
  theme(legend.position = "none")

# --- FINAL ---
final_plot <- p_price / p_rsi + plot_layout(heights = c(3, 1))
print(final_plot)

