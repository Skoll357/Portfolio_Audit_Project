# ==============================================================================
# 项目名称：宏观制度驱动下的资产配置审计 (终极交付版 - 2026)
# 功能：相关性断裂、效率缺口分析、战术对冲回测
# ==============================================================================

# --- 0. 环境与零件补全 ---
required_packages <- c("tidyverse", "tidyquant", "PerformanceAnalytics", 
                       "PortfolioAnalytics", "lubridate", "scales", 
                       "ggrepel", "zoo")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(tidyquant)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(lubridate)
library(scales)
library(ggrepel)
library(zoo)

# --- 1. 数据读取与清洗 (针对本地 CSV 文件) ---
clean_local_data <- function(file_path, name) {
  read_csv(file_path) %>%
    select(Date, Price) %>%
    mutate(Date = mdy(Date), Price = as.numeric(gsub(",", "", Price))) %>%
    arrange(Date) %>%
    mutate(ret = log(Price / lag(Price))) %>% 
    select(Date, !!paste0("ret_", name) := ret) %>% drop_na()
}

voo_ret <- clean_local_data("../raw_data/VOO ETF Stock Price History.csv", "VOO")
qqq_ret <- clean_local_data("../raw_data/QQQ ETF Stock Price History.csv", "QQQ")
vglt_ret <- clean_local_data("../raw_data/VGLT ETF Stock Price History.csv", "VGLT")

# 读取 Sticky CPI 数据 (基于你的 observation_date / CORESTICKM159SFRBATL 格式)
inflation_raw <- read_csv("../raw_data/cpi.csv") %>%
  rename(Date = observation_date, inf_percent = CORESTICKM159SFRBATL) %>%
  mutate(Date = mdy(Date), inf_yoy = inf_percent / 100) %>% select(Date, inf_yoy)

# 合并所有数据
df_daily <- voo_ret %>%
  inner_join(qqq_ret, by = "Date") %>%
  inner_join(vglt_ret, by = "Date") %>%
  left_join(inflation_raw, by = "Date") %>%
  fill(inf_yoy, .direction = "down") %>%
  mutate(Regime = ifelse(inf_yoy > 0.03, "High Inflation", "Low Inflation")) %>%
  drop_na()

# --- 2. 宏观对冲逻辑准备 (均线计算 & 策略准备) ---
df_strat_data <- df_daily %>%
  mutate(VOO_Price = cumprod(1 + ret_VOO)) %>%
  # 200 日均线计算
  mutate(VOO_SMA200 = rollmean(VOO_Price, k = 200, fill = NA, align = "right")) %>%
  # 解决 NA 冷启动问题：前 200 天默认看多 (Up)
  mutate(Trend = ifelse(is.na(VOO_SMA200) | VOO_Price > VOO_SMA200, "Up", "Down")) %>%
  mutate(
    # 动态策略：低通胀=1/3均衡；高通胀牛市=股票；高通胀熊市=避险
    Dynamic_Ret = case_when(
      Regime == "Low Inflation" ~ (ret_VOO/3 + ret_QQQ/3 + ret_VGLT/3),
      Regime == "High Inflation" & Trend == "Up" ~ (ret_VOO/2 + ret_QQQ/2),
      Regime == "High Inflation" & Trend == "Down" ~ 0.00008, # 现金避险收益
      TRUE ~ (ret_VOO/3 + ret_QQQ/3 + ret_VGLT/3)
    )
  )

# ==============================================================================
# Graph 1: 相关性制度断裂 (Correlation Regime Shift)
# ==============================================================================
# [新增：定量审计相关性显著性 - 用于为绘图提供数据并生成附录 I]
cor_audit_df <- df_daily %>%
  tq_mutate_xy(x = ret_VOO, y = ret_VGLT, mutate_fun = runCor, n = 60, col_rename = "roll_cor") %>%
  filter(!is.na(roll_cor))

# 计算均值
mean_low_inf <- mean(cor_audit_df$roll_cor[cor_audit_df$Regime == "Low Inflation"], na.rm = TRUE)
mean_high_inf <- mean(cor_audit_df$roll_cor[cor_audit_df$Regime == "High Inflation"], na.rm = TRUE)

# 运行 t-test 并在 Console 输出结果
t_test_result <- t.test(roll_cor ~ Regime, data = cor_audit_df)
print("--- Appendix I: Correlation Regime Shift Audit (t-test) ---")
print(t_test_result)

# 计算高通胀底色块
regime_blocks <- df_daily %>%
  mutate(is_high = Regime == "High Inflation") %>%
  mutate(group = cumsum(is_high != lag(is_high, default = first(is_high)))) %>%
  filter(is_high) %>%
  group_by(group) %>%
  summarise(start = min(Date), end = max(Date))

p1 <- df_daily %>%
  tq_mutate_xy(x = ret_VOO, y = ret_VGLT, mutate_fun = runCor, n = 60, col_rename = "roll_cor") %>%
  ggplot() +
  geom_rect(data = regime_blocks, aes(xmin = start, xmax = end, ymin = -1, ymax = 1), 
            fill = "#CC3333", alpha = 0.08) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(aes(x = Date, y = roll_cor), size = 0.9, color = "#2C3E50") +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  annotate("label", x = as.Date("2019-06-01"), y = -0.85, 
           label = paste0("Mean Cor: ", round(mean_low_inf, 2)), 
           family = "serif", size = 4, fill = "white", alpha = 0.7) +
  annotate("label", x = as.Date("2024-01-01"), y = 0.85, 
           label = paste0("Mean Cor: +", round(mean_high_inf, 2)), 
           family = "serif", size = 4, color = "#CC3333", fill = "white", alpha = 0.7) +
  labs(title = "Graph 1: Stock-Bond Correlation Breakdown",
       subtitle = "60-Day Rolling Correlation (VOO vs VGLT). Shaded area denotes Sticky CPI > 3% regime.",
       y = "Correlation Coefficient", x = "") +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    legend.position = "bottom",
    plot.margin = margin(t = 30, r = 20, b = 10, l = 20),
    plot.title = element_text(face = "bold", size = 20, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "gray30", size = 11, margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95")
  )

# ==============================================================================
# Graph 2: 效率缺口审计 (Opportunity Set Contraction)
# ==============================================================================
# A. 数值模拟与单调前沿线计算 (保持原样)
get_stats <- function(data, w_vec) {
  r <- as.matrix(data[, c("ret_VOO", "ret_QQQ", "ret_VGLT")]) %*% w_vec
  data.frame(ret = mean(r) * 252, vol = sd(r) * sqrt(252))
}

simulate_monotonic_frontier <- function(data, n = 3000) {
  set.seed(123)
  cloud <- map_df(1:n, ~{
    w <- runif(3); w <- w/sum(w)
    get_stats(data, w)
  })
  cloud %>% arrange(vol) %>% mutate(ret_f = cummax(ret)) %>% filter(ret == ret_f) %>%
    mutate(ret_smooth = predict(loess(ret ~ vol, span = 0.5)))
}

smooth_low <- simulate_monotonic_frontier(df_daily[df_daily$Regime == "Low Inflation",])
smooth_high <- simulate_monotonic_frontier(df_daily[df_daily$Regime == "High Inflation",])

# B. 效率缺口阴影 (The Gap) (保持原样)
common_vol <- seq(0.10, 0.25, length.out = 100)
gap_data <- data.frame(
  vol = common_vol,
  y_low = approx(smooth_low$vol, smooth_low$ret_smooth, xout = common_vol)$y,
  y_high = approx(smooth_high$vol, smooth_high$ret_smooth, xout = common_vol)$y
) %>% drop_na()

# C. 资产迁移轨迹 (保持原样)
pure_assets <- c("VOO", "QQQ", "VGLT")
pure_weights <- list(c(1,0,0), c(0,1,0), c(0,0,1))
asset_points <- map_df(1:3, ~{
  w <- pure_weights[[.x]]; name <- pure_assets[.x]
  rbind(cbind(get_stats(df_daily[df_daily$Regime == "Low Inflation",], w), Regime = "Low Inflation", Asset = name),
        cbind(get_stats(df_daily[df_daily$Regime == "High Inflation",], w), Regime = "High Inflation", Asset = name))
})
trajectory_arrows <- asset_points %>% pivot_wider(names_from = Regime, values_from = c(ret, vol)) %>%
  rename(x_start = `vol_Low Inflation`, y_start = `ret_Low Inflation`, x_end = `vol_High Inflation`, y_end = `ret_High Inflation`)

# --- 【关键：把计算块放在这里！！！】 ---
# 因为到这一行，asset_points 和 smooth_high 已经全部生成完毕了

# 1. 计算各资产 SR
asset_points <- asset_points %>% mutate(SR = ret / vol)

# 2. 计算前沿面最高 SR
max_sr_low <- max(smooth_low$ret_smooth / smooth_low$vol, na.rm = TRUE)
max_sr_high <- max(smooth_high$ret_smooth / smooth_high$vol, na.rm = TRUE)

# 3. 计算衰减百分比
eff_decay_pct <- (max_sr_high / max_sr_low - 1) * 100

# 4. 生成审计汇总表
sr_audit_summary <- asset_points %>%
  mutate(Regime_Temp = gsub(" ", "_", Regime)) %>% 
  select(Asset, Regime_Temp, SR) %>%
  pivot_wider(names_from = Regime_Temp, values_from = SR, names_prefix = "SR_") %>%
  mutate(SR_Drop = SR_High_Inflation - SR_Low_Inflation)

print("--- Appendix II: Asset Efficiency Audit (Sharpe Ratio) ---")
print(sr_audit_summary)

# D. 绘图
p2 <- ggplot() +
  geom_ribbon(data = gap_data, aes(x = vol, ymin = y_high, ymax = y_low), fill = "gray92", alpha = 0.8) +
  annotate("text", x = 0.14, y = 0.07, label = "THE EFFICIENCY GAP", color = "gray60", fontface = "bold", size = 4, angle = 22, family = "serif") +
  geom_line(data = smooth_low, aes(x = vol, y = ret_smooth), color = "#003366", size = 1.3) +
  geom_line(data = smooth_high, aes(x = vol, y = ret_smooth), color = "#CC0000", size = 1.3) +
  annotate("text", x = 0.23, y = 0.23, label = "Low Inflation Frontier", color = "#003366", fontface = "bold", size = 4.5, hjust = 0, family = "serif") +
  annotate("text", x = 0.23, y = 0.08, label = "High Inflation Frontier", color = "#CC0000", fontface = "bold", size = 4.5, hjust = 0, family = "serif") +
  geom_point(data = asset_points, aes(x = vol, y = ret, color = Regime, shape = Asset), size = 2.5, stroke = 1) +
  geom_curve(data = trajectory_arrows, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
             arrow = arrow(length = unit(0.18, "cm"), type = "closed"), curvature = -0.15, color = "gray40", size = 0.6) +
  scale_y_continuous(labels = percent, limits = c(-0.12, 0.25)) + 
  scale_x_continuous(labels = percent, limits = c(0.10, 0.32)) +
  annotate("text", x = 0.28, y = 0.02, 
           label = paste0("Frontier Efficiency Decay: ", 
                          round((max_sr_high/max_sr_low - 1) * 100, 1), "%"),
           family = "serif", size = 4.5, color = "gray40", fontface = "bold") +
  scale_color_manual(values = c("High Inflation" = "#CC0000", "Low Inflation" = "#003366")) +
  scale_shape_manual(values = c("VOO" = 16, "QQQ" = 17, "VGLT" = 15)) + 
  labs(title = "Graph 2: Systematic Opportunity Set Contraction",
       subtitle = "Tracing the shift from Low to High Inflation regimes. Note the systemic collapse in asset efficiency.",
       x = "Annualized Volatility (Systematic Risk)", y = "Annualized Return (Portfolio Reward)") +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    legend.position = "bottom",
    plot.margin = margin(t = 30, r = 20, b = 10, l = 20),
    plot.title = element_text(face = "bold", size = 20, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "gray30", size = 11, margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95")
  )

# ==============================================================================
# Graph 3: 策略对比 (Tactical vs. Static)
# ==============================================================================
cum_matrix <- data.frame(
  Date = df_strat_data$Date,
  `Pure Equity (VOO)` = cumprod(1 + df_strat_data$ret_VOO) * 1000,
  `Pure Bond (VGLT)` = cumprod(1 + df_strat_data$ret_VGLT) * 1000,
  `Balanced Static (60/40)` = cumprod(1 + (df_strat_data$ret_VOO/3 + df_strat_data$ret_QQQ/3 + df_strat_data$ret_VGLT/3)) * 1000,
  `Dynamic Tactical Strategy` = cumprod(1 + df_strat_data$Dynamic_Ret) * 1000,
  check.names = FALSE
) %>% pivot_longer(cols = -Date, names_to = "Strategy", values_to = "Value") %>%
  mutate(Strategy = factor(Strategy, levels = c("Dynamic Tactical Strategy", "Balanced Static (60/40)", "Pure Equity (VOO)", "Pure Bond (VGLT)")))

p3 <- ggplot(cum_matrix, aes(x = Date, y = Value, color = Strategy)) +
  geom_line(aes(size = Strategy == "Dynamic Tactical Strategy"), alpha = 0.9) +
  scale_size_manual(values = c("TRUE" = 1.3, "FALSE" = 0.7), guide = "none") +
  scale_color_manual(values = c("Dynamic Tactical Strategy" = "#006633", "Balanced Static (60/40)" = "#6699CC", "Pure Equity (VOO)" = "gray60", "Pure Bond (VGLT)" = "#CC3333")) +
  scale_y_continuous(labels = dollar_format(accuracy = 1), position = "right") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  labs(title = "Graph 3: Strategic Capital Preservation: Tactical vs. Static",
       subtitle = "Cumulative Growth of a $1,000 Initial Investment. Dynamic strategy leverages macro-regime detection.",
       y = "Portfolio Value ($)", x = "") +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    legend.position = "bottom",
    plot.margin = margin(t = 30, r = 20, b = 10, l = 20),
    plot.title = element_text(face = "bold", size = 20, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "gray30", size = 11, margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95")
  ) +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 1.5)))

# ==============================================================================
# Appendix III: 策略绩效核心审计矩阵 (Performance Matrix)
# ==============================================================================

# 1. 提取四种策略的日收益率序列 (确保 1/3 Balanced 逻辑严格准确)
audit_returns <- df_strat_data %>%
  mutate(
    # 严格执行 (ret_VOO + ret_QQQ + ret_VGLT) / 3 以对标 1/3 Balanced
    Static_Balanced_Ret = (ret_VOO + ret_QQQ + ret_VGLT) / 3
  ) %>%
  select(Date, 
         `Dynamic Tactical` = Dynamic_Ret, 
         `Pure Equity (VOO)` = ret_VOO, 
         `Pure Bond (VGLT)` = ret_VGLT, 
         `1/3 Balanced (Static)` = Static_Balanced_Ret)

# 2. 转换为 xts 格式以进行金融指标计算
audit_xts <- xts::xts(audit_returns[,-1], order.by = audit_returns$Date)

# 3. 计算 4x4 审计指标并转置 (Transpose)
perf_metrics_audit <- function(returns_xts) {
  # 计算四大核心指标
  ann_ret <- PerformanceAnalytics::Return.annualized(returns_xts)
  ann_vol <- PerformanceAnalytics::StdDev.annualized(returns_xts)
  ann_sr  <- PerformanceAnalytics::SharpeRatio.annualized(returns_xts, Rf = 0)
  max_dd  <- PerformanceAnalytics::maxDrawdown(returns_xts)
  
  # 纵向合并指标
  audit_matrix <- rbind(ann_ret, ann_vol, ann_sr, max_dd)
  rownames(audit_matrix) <- c("Annualized Return", "Annualized Vol", "Sharpe Ratio", "Max Drawdown")
  
  # 转置表格：让策略作为行，指标作为列，符合专业阅读习惯
  return(t(audit_matrix))
}

# 4. 执行审计并输出至 Console
final_audit_table <- perf_metrics_audit(audit_xts)

print("==========================================================")
print("   APPENDIX III: FINAL STRATEGIC PERFORMANCE AUDIT        ")
print("==========================================================")
print(round(final_audit_table, 4))
print("==========================================================")

# --- 执行最终渲染 ---
print(p1)
print(p2)
print(p3)