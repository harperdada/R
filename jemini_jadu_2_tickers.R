library(ggplot2); library(tidyverse); library(quantmod); library(TTR); library(patchwork)

# 1. Fetch & Clean for Two Tickers
t1 <- toupper(trimws(readline(prompt = "Enter First Ticker: ")))
t2 <- toupper(trimws(readline(prompt = "Enter Second Ticker: ")))

get_clean_data <- function(t) {
  getSymbols(t, src = "yahoo", from = "2024-10-01", auto.assign = FALSE) %>%
    {data.frame(Date = index(.), Price = as.numeric(Ad(.)), Volume = as.numeric(Vo(.)))} %>%
    na.omit() %>% mutate(Ticker = t)
}

df1 <- get_clean_data(t1); df2 <- get_clean_data(t2)

# 2. Indicators & Logic Function
process_logic <- function(df) {
  df$SMA50 <- as.numeric(SMA(df$Price, n = 50))
  df$RSI <- as.numeric(RSI(df$Price, n = 14))
  df$AvgVol <- as.numeric(SMA(df$Volume, n = 50))
  df %>% drop_na() %>%
    mutate(prev_px = lag(Price), prev_sma = lag(SMA50),
           is_cross = prev_px <= prev_sma & Price > SMA50,
           vol_surge = Volume > (AvgVol * 1.5),
           cross_above = is_cross & vol_surge, 
           is_fakeout = is_cross & !vol_surge,
           cross_below = prev_px >= prev_sma & Price < SMA50)
}

df1 <- process_logic(df1); df2 <- process_logic(df2)
combined_df <- bind_rows(df1, df2)

# 3. Success Stats (Focusing on Ticker 1 for Footer)
calc_stats <- function(df) {
  df$five_day_return <- lead(df$Price, 5) - df$Price 
  signals <- df %>% filter(cross_above == TRUE) %>% tail(5)
  pos_ret <- sum(signals$five_day_return[signals$five_day_return > 0], na.rm = TRUE)
  neg_ret <- abs(sum(signals$five_day_return[signals$five_day_return < 0], na.rm = TRUE))
  list(win_r = ifelse(nrow(signals) > 0, round((sum(signals$five_day_return > 0, na.rm=T) / nrow(signals)) * 100, 1), 0),
       profit_f = ifelse(neg_ret > 0, round(pos_ret / neg_ret, 2), "Inf"))
}
stats1 <- calc_stats(df1)

# 4. Header Components
header_info <- function(df) {
  curr_px <- round(tail(df$Price, 1), 2)
  curr_sma <- round(tail(df$SMA50, 1), 2)
  fake_30 <- sum(df$is_fakeout[df$Date >= (max(df$Date) - 30)], na.rm = TRUE)
  list(px = curr_px, sma = curr_sma, healthy = curr_px > curr_sma, fakes = fake_30)
}
h1 <- header_info(df1)

status_text <- paste0("Trend: ", ifelse(h1$healthy, "ABOVE SMA (Healthy)", "BELOW SMA (Caution)"))
header_sub <- paste0(status_text, " | Fakeouts (30d): ", h1$fakes, " | ", t1, ": $", h1$px, " | SMA: $", h1$sma)

# 5. Triple-Panel Assembly with Overlays
# Panel 1: Price Overlay (Uses 'Ticker' for color/legend)
p1 <- ggplot(combined_df, aes(x = Date, y = Price, color = Ticker)) +
  geom_line(linewidth = 1) + 
  geom_line(aes(y = SMA50, group = Ticker), linetype = "dotted", alpha = 0.5) +
  geom_point(data = filter(combined_df, cross_above), aes(y = Price), color = "gold", shape = 17, size = 4) +
  scale_color_manual(values = c("blue", "darkorange")) +
  labs(title = paste(t1, "vs", t2, "Trend Overlay"), subtitle = header_sub) +
  theme_minimal() + theme(legend.position = "top", plot.subtitle = element_text(face = "bold", color = ifelse(h1$healthy, "darkgreen", "red")))

# Panel 2: Volume (Showing Ticker 1 by default to avoid clutter)
p2 <- ggplot(df1, aes(x = Date, y = Volume)) +
  geom_bar(stat = "identity", aes(fill = vol_surge)) +
  scale_fill_manual(values = c("gray80", "gold")) + geom_line(aes(y = AvgVol), color = "blue") +
  theme_minimal() + guides(fill = "none") + labs(y = paste(t1, "Vol"))

# Panel 3: RSI Comparison
p3 <- ggplot(combined_df, aes(x = Date, y = RSI, color = Ticker)) +
  geom_line() + geom_hline(yintercept = c(30, 70), linetype = "dashed") +
  scale_color_manual(values = c("blue", "darkorange")) +
  theme_minimal() + labs(y = "RSI") + theme(legend.position = "none")

# FINAL ASSEMBLY
final_plot <- (p1 / p2 / p3) + 
  plot_layout(heights = c(3, 1, 1)) +
  plot_annotation(caption = paste0(t1, " Strategy Stats | Success: ", stats1$win_r, "% | Profit Factor: ", stats1$profit_f))

print(final_plot)
