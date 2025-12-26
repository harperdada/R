library(ggplot2); library(tidyverse); library(quantmod); library(TTR); library(patchwork)

# 1. Fetch & Clean
ticker <- toupper(trimws(readline(prompt = "Enter Ticker: ")))
getSymbols(ticker, src = "yahoo", from = "2024-10-01", auto.assign = TRUE)
raw_data <- get(ticker)
df <- data.frame(Date = index(raw_data), Price = as.numeric(Ad(raw_data)), 
                 Volume = as.numeric(Vo(raw_data))) %>% na.omit()

# 2. Indicators & Logic
df$SMA50 <- as.numeric(SMA(df$Price, n = 50)); df$RSI <- as.numeric(RSI(df$Price, n = 14))
df$AvgVol <- as.numeric(SMA(df$Volume, n = 50))
df <- df %>% drop_na() %>%
  mutate(prev_px = lag(Price), prev_sma = lag(SMA50),
         is_cross = prev_px <= prev_sma & Price > SMA50,
         vol_surge = Volume > (AvgVol * 1.5)) %>%
  mutate(cross_above = is_cross & vol_surge, 
         is_fakeout = is_cross & !vol_surge,
         cross_below = prev_px >= prev_sma & Price < SMA50)

# 3. Success Stats (For the Footer)
df$five_day_return <- lead(df$Price, 5) - df$Price 
signals <- df %>% filter(cross_above == TRUE) %>% tail(5)
pos_ret <- sum(signals$five_day_return[signals$five_day_return > 0], na.rm = TRUE)
neg_ret <- abs(sum(signals$five_day_return[signals$five_day_return < 0], na.rm = TRUE))
profit_f <- ifelse(neg_ret > 0, round(pos_ret / neg_ret, 2), "Inf")
win_r <- ifelse(nrow(signals) > 0, round((sum(signals$five_day_return > 0, na.rm=T) / nrow(signals)) * 100, 1), 0)

# 4. Header Components (Restoring Fakeout to Subtitle)
fakeouts_30d <- sum(df$is_fakeout[df$Date >= (max(df$Date) - 30)], na.rm = TRUE)
curr_px <- round(tail(df$Price, 1), 2)
curr_sma <- round(tail(df$SMA50, 1), 2)
is_healthy <- curr_px > curr_sma
status_text <- paste0("The trend is ", ifelse(is_healthy, "ABOVE SMA (Healthy)", "BELOW SMA (Caution)"))

# TOP SUBTITLE: Adding Fakeouts back here!
header_sub <- paste0(status_text, " | Fakeouts (30d): ", fakeouts_30d, " | Price: $", curr_px, " | SMA: $", curr_sma)
status_col <- ifelse(is_healthy, "darkgreen", "red")

# 5. Triple-Panel Assembly
p1 <- ggplot(df, aes(x = Date)) +
  geom_line(aes(y = Price)) + geom_line(aes(y = SMA50), color = "blue", alpha = 0.3) +
  geom_point(data = filter(df, RSI < 30), aes(y = Price), color = "red", size = 2, alpha = 0.5) +
  geom_point(data = filter(df, cross_above), aes(y = Price), color = "gold", shape = 17, size = 6) +
  geom_point(data = filter(df, cross_below), aes(y = Price), color = "red", shape = 25, fill = "red", size = 5) +
  labs(title = paste(ticker, "Full Trend Radar"), subtitle = header_sub) +
  theme_minimal() + theme(plot.subtitle = element_text(face = "bold", color = status_col))

p2 <- ggplot(df, aes(x = Date, y = Volume)) +
  geom_bar(stat = "identity", aes(fill = vol_surge)) +
  scale_fill_manual(values = c("gray80", "gold")) + geom_line(aes(y = AvgVol), color = "blue") +
  theme_minimal() + guides(fill = "none") + labs(y = "Vol")

p3 <- ggplot(df, aes(x = Date, y = RSI)) +
  geom_line(color = "purple") + geom_hline(yintercept = c(30, 70), linetype = "dashed") +
  theme_minimal() + labs(y = "RSI")

# FINAL ASSEMBLY
final_plot <- (p1 / p2 / p3) + 
  plot_layout(heights = c(3, 1, 1)) +
  plot_annotation(caption = paste0("Strategy Stats | Success: ", win_r, "% | Profit Factor: ", profit_f))

print(final_plot)
