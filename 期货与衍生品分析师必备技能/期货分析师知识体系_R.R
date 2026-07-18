# ============================================================
#  顶级期货分析师知识体系 — R语言实现
#  涵盖：定价、技术分析、量化模型、风险管理、期权定价、套利策略
# ============================================================

# ------------------------------------------------------------
# 0. 加载所需包
# ------------------------------------------------------------
required_packages <- c(
  "TTR", "forecast", "tseries", "urca", "rugarch",
  "randomForest", "caret", "PerformanceAnalytics",
  "quantmod", "openxlsx", "dplyr", "tidyr"
)

install_if_missing <- function(packages) {
  new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_pkgs) > 0) {
    cat("安装缺失包:", paste(new_pkgs, collapse = ", "), "\n")
    install.packages(new_pkgs, repos = "https://cloud.r-project.org/")
  }
}

install_if_missing(required_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

cat("═══════════════════════════════════════════\n")
cat("  顶级期货分析师知识体系 — R语言实现\n")
cat("═══════════════════════════════════════════\n\n")


# ============================================================
# 模块一：期货定价理论
# ============================================================
cat("━━━ 模块一：期货定价理论 ━━━\n\n")

# 1.1 持有成本模型
cost_of_carry_pricing <- function(S, r, u, y, T) {
  #' 持有成本模型计算期货理论价格
  #' @param S 现货价格
  #' @param r 无风险利率
  #' @param u 仓储费率
  #' @param y 便利收益率
  #' @param T 到期时间(年)
  F <- S * exp((r + u - y) * T)
  basis <- S - F

  cat("═══ 持有成本模型定价 ═══\n")
  cat("现货价格:    ", S, "\n")
  cat("无风险利率:  ", paste0(r * 100, "%"), "\n")
  cat("仓储费率:    ", paste0(u * 100, "%"), "\n")
  cat("便利收益率:  ", paste0(y * 100, "%"), "\n")
  cat("到期时间:    ", paste0(T * 12, "个月"), "\n")
  cat("理论期货价格:", round(F, 2), "\n")
  cat("基差 (S-F):  ", round(basis, 2), "\n")

  if (F > S) {
    cat("市场结构:    正向市场 (Contango)\n")
  } else {
    cat("市场结构:    反向市场 (Backwardation)\n")
  }

  return(list(theoretical_F = F, basis = basis, market_structure = ifelse(F > S, "Contango", "Backwardation")))
}

# 示例：沪铜期货定价
cat("【示例】沪铜期货6个月合约定价\n")
result_coc <- cost_of_carry_pricing(
  S = 72000,   # 现货价格（元/吨）
  r = 0.025,   # 无风险利率 2.5%
  u = 0.008,   # 仓储费率 0.8%
  y = 0.005,   # 便利收益率 0.5%
  T = 0.5      # 6个月到期
)
cat("\n")

# 1.2 期限结构分析
term_structure_analysis <- function(near_price, far_price, near_T, far_T, spot_price) {
  #' 期限结构分析
  slope <- (far_price - near_price) / (far_T - near_T)
  roll_yield <- (near_price - far_price) / spot_price * (1 / far_T)

  cat("═══ 期限结构分析 ═══\n")
  cat("近月合约价格:", near_price, "\n")
  cat("远月合约价格:", far_price, "\n")
  cat("期限结构斜率:", round(slope, 2), "\n")
  cat("滚动收益(年化):", round(roll_yield * 100, 2), "%\n")

  if (far_price > near_price) {
    cat("结构: Contango (远月升水) — 持有多头有滚动损失\n")
  } else {
    cat("结构: Backwardation (近月升水) — 持有多头有滚动收益\n")
  }

  return(list(slope = slope, roll_yield = roll_yield))
}

cat("【示例】沪铜期限结构分析\n")
result_ts <- term_structure_analysis(
  near_price = 72200, far_price = 72800,
  near_T = 1/12, far_T = 3/12, spot_price = 72000
)
cat("\n")


# ============================================================
# 模块二：技术分析体系
# ============================================================
cat("━━━ 模块二：技术分析体系 ━━━\n\n")

# 生成模拟数据
set.seed(42)
n <- 500
close_price <- cumsum(rnorm(n, 2, 40)) + 5000
high_price  <- close_price + abs(rnorm(n, 30, 15))
low_price   <- close_price - abs(rnorm(n, 30, 15))
volume_data <- abs(rnorm(n, 100000, 30000))
dates <- seq.Date(as.Date("2024-01-01"), by = "day", length.out = n)

price_df <- data.frame(
  Date   = dates,
  Open   = close_price + rnorm(n, 0, 10),
  High   = high_price,
  Low    = low_price,
  Close  = close_price,
  Volume = volume_data
)

# 2.1 移动平均线系统
cat("【2.1】移动平均线系统\n")

# 计算各类均线
price_df$SMA_5  <- SMA(price_df$Close, n = 5)
price_df$SMA_20 <- SMA(price_df$Close, n = 20)
price_df$SMA_60 <- SMA(price_df$Close, n = 60)
price_df$EMA_12 <- EMA(price_df$Close, n = 12)
price_df$EMA_26 <- EMA(price_df$Close, n = 26)

# MACD
macd_result <- MACD(price_df$Close, nFast = 12, nSlow = 26, nSig = 9)
price_df$MACD_Line   <- macd_result[, "macd"]
price_df$Signal_Line <- macd_result[, "signal"]
price_df$MACD_Hist   <- macd_result[, "macd"] - macd_result[, "signal"]

# 布林带
bb <- BBands(price_df$Close, n = 20, sd = 2)
price_df$BB_UP  <- bb[, "up"]
price_df$BB_MID <- bb[, "mavg"]
price_df$BB_LO  <- bb[, "dn"]

# 金叉/死叉信号
price_df$MA_Signal <- ifelse(price_df$SMA_5 > price_df$SMA_20, 1, -1)
price_df$Golden_Cross <- diff(price_df$MA_Signal) == 2
price_df$Death_Cross  <- diff(price_df$MA_Signal) == -2

cat("金叉信号次数:", sum(price_df$Golden_Cross, na.rm = TRUE), "\n")
cat("死叉信号次数:", sum(price_df$Death_Cross, na.rm = TRUE), "\n")
cat("最近5日收盘价与均线:\n")
print(tail(price_df[, c("Date", "Close", "SMA_5", "SMA_20", "SMA_60")], 5))
cat("\n")

# 2.2 RSI指标
cat("【2.2】RSI相对强弱指数\n")

calculate_rsi <- function(price, n = 14) {
  delta <- diff(price)
  gain <- ifelse(delta > 0, delta, 0)
  loss <- ifelse(delta < 0, -delta, 0)
  avg_gain <- SMA(gain, n)
  avg_loss <- SMA(loss, n)
  rs <- avg_gain / avg_loss
  rsi <- 100 - 100 / (1 + rs)
  return(rsi)
}

price_df$RSI_14 <- c(NA, calculate_rsi(price_df$Close, 14))
price_df$RSI_Signal <- ifelse(price_df$RSI_14 > 70, "超买",
                        ifelse(price_df$RSI_14 < 30, "超卖", "中性"))

cat("最近5日RSI值与信号:\n")
print(tail(price_df[, c("Date", "Close", "RSI_14", "RSI_Signal")], 5))
cat("\n")

# 2.3 KDJ指标
cat("【2.3】KDJ随机指标\n")

calculate_kdj <- function(high, low, close, n = 9) {
  lowest_low  <- rollapply(low,  n, min, fill = NA, align = "right")
  highest_high <- rollapply(high, n, max, fill = NA, align = "right")
  rsv <- (close - lowest_low) / (highest_high - lowest_low) * 100

  k <- rep(NA, length(close))
  d <- rep(NA, length(close))
  k[1] <- 50; d[1] <- 50

  for (i in 2:length(close)) {
    if (!is.na(rsv[i])) {
      k[i] <- 2/3 * k[i-1] + 1/3 * rsv[i]
      d[i] <- 2/3 * d[i-1] + 1/3 * k[i]
    }
  }
  j <- 3 * k - 2 * d
  return(data.frame(K = k, D = d, J = j))
}

kdj <- calculate_kdj(price_df$High, price_df$Low, price_df$Close, n = 9)
price_df$K_KDJ <- kdj$K
price_df$D_KDJ <- kdj$D
price_df$J_KDJ <- kdj$J

cat("最近5日KDJ值:\n")
print(tail(price_df[, c("Date", "K_KDJ", "D_KDJ", "J_KDJ")], 5))
cat("\n")

# 2.4 ATR真实波动幅度
cat("【2.4】ATR真实波动幅度\n")

calculate_atr <- function(high, low, close, n = 14) {
  tr <- pmax(high - low, abs(high - lag(close)), abs(low - lag(close)))
  atr <- SMA(tr, n)
  return(list(TR = tr, ATR = atr))
}

atr_result <- calculate_atr(price_df$High, price_df$Low, price_df$Close, n = 14)
price_df$ATR_14 <- atr_result$ATR
cat("最近5日ATR值:\n")
print(tail(price_df[, c("Date", "Close", "ATR_14")], 5))
cat("\n")

# 2.5 VWAP成交量加权平均价
cat("【2.5】VWAP成交量加权平均价\n")

calculate_vwap <- function(close, volume) {
  cum_vp <- cumsum(close * volume)
  cum_v  <- cumsum(volume)
  vwap   <- cum_vp / cum_v
  return(vwap)
}

price_df$VWAP <- calculate_vwap(price_df$Close, price_df$Volume)
cat("最近5日VWAP:\n")
print(tail(price_df[, c("Date", "Close", "VWAP")], 5))
cat("\n")


# ============================================================
# 模块三：基本面分析框架
# ============================================================
cat("━━━ 模块三：基本面分析框架 ━━━\n\n")

# 3.1 供需平衡表
cat("【3.1】供需平衡表构建\n")

balance_sheet <- data.frame(
  年份     = 2019:2025,
  期初库存 = c(180, 165, 155, 140, 130, 120, 135),
  产量     = c(985, 1002, 1049, 1106, 1150, 1198, 1240),
  进口量   = c(380, 450, 550, 530, 480, 510, 525),
  消费量   = c(1180, 1380, 1480, 1510, 1520, 1560, 1600),
  出口量   = c(5, 8, 6, 10, 8, 7, 9)
) %>%
  mutate(
    总供给   = 期初库存 + 产量 + 进口量,
    总需求   = 消费量 + 出口量,
    期末库存 = 总供给 - 总需求,
    供需缺口 = 总供给 - 总需求,
    库存消费比 = round(期末库存 / 消费量 * 100, 1)
  )

cat("沪铜供需平衡表（万吨）:\n")
print(balance_sheet)

# 导出Excel
if (requireNamespace("openxlsx", quietly = TRUE)) {
  write.xlsx(balance_sheet, "供需平衡表_沪铜.xlsx")
  cat("已导出: 供需平衡表_沪铜.xlsx\n")
}
cat("\n")


# ============================================================
# 模块四：量化分析模型
# ============================================================
cat("━━━ 模块四：量化分析模型 ━━━\n\n")

# 4.1 ARIMA模型
cat("【4.1】ARIMA时间序列模型\n")

price_ts <- ts(close_price)

# ADF平稳性检验
adf_test <- adf.test(price_ts)
cat("ADF统计量:", round(adf_test$statistic, 4), "\n")
cat("p值:      ", round(adf_test$p.value, 4), "\n")

# 自动选择最优ARIMA阶数
fit_arima <- auto.arima(price_ts,
                         seasonal = FALSE,
                         trace = TRUE,
                         stepwise = FALSE,
                         approximation = FALSE)

cat("\n最优模型:\n")
print(summary(fit_arima))

# 预测未来20个交易日
fc <- forecast(fit_arima, h = 20, level = c(80, 95))
cat("\n未来20日预测:\n")
print(data.frame(
  预测值 = round(as.numeric(fc$mean), 2),
  下限80 = round(as.numeric(fc$lower[, 1]), 2),
  上限80 = round(as.numeric(fc$upper[, 1]), 2),
  下限95 = round(as.numeric(fc$lower[, 2]), 2),
  上限95 = round(as.numeric(fc$upper[, 2]), 2)
))
cat("\n")

# 4.2 GARCH模型
cat("【4.2】GARCH波动率模型\n")

returns <- diff(log(close_price)) * 100

# 设定GARCH(1,1)模型
spec <- ugarchspec(
  mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  distribution.model = "norm"
)

fit_garch <- ugarchfit(spec, returns, solver = "hybrid")
cat("GARCH(1,1)模型参数:\n")
coef_garch <- coef(fit_garch)
cat("  μ (均值):     ", round(coef_garch["mu"], 6), "\n")
cat("  ω (常数):     ", round(coef_garch["omega"], 6), "\n")
cat("  α₁ (ARCH):    ", round(coef_garch["alpha1"], 4), "\n")
cat("  β₁ (GARCH):   ", round(coef_garch["beta1"], 4), "\n")
cat("  α+β (持续性): ", round(coef_garch["alpha1"] + coef_garch["beta1"], 4), "\n")

# 波动率预测
forecast_garch <- ugarchforecast(fit_garch, n.ahead = 20)
sigma_fc <- sigma(forecast_garch)
cat("\n未来5日波动率预测(%):\n")
print(round(as.numeric(sigma_fc[1:5]), 4))
cat("\n")

# 4.3 协整检验与配对套利
cat("【4.3】协整检验与配对套利\n")

set.seed(42)
common_factor <- cumsum(rnorm(n, 0.1, 1))
y1 <- 4000 + common_factor * 10 + rnorm(n, 0, 20)  # 螺纹钢
y2 <- 3800 + common_factor * 9  + rnorm(n, 0, 18)   # 热卷

# OLS回归
reg <- lm(y1 ~ y2)
hedge_ratio <- coef(reg)[2]
cat("对冲比 (Hedge Ratio):", round(hedge_ratio, 4), "\n")

# 残差ADF检验
residuals_reg <- resid(reg)
adf_resid <- adf.test(residuals_reg)
cat("残差ADF p值:", round(adf_resid$p.value, 4), "\n")
if (adf_resid$p.value < 0.05) {
  cat("✓ 存在协整关系，可以构建套利组合\n")
} else {
  cat("✗ 不存在协整关系\n")
}

# 构建套利信号
spread <- y1 - hedge_ratio * y2
spread_mean <- mean(spread)
spread_std  <- sd(spread)
z_score <- (spread - spread_mean) / spread_std

signal <- ifelse(z_score > 2, "做空价差",
          ifelse(z_score < -2, "做多价差", "观望"))

cat("\n配对套利统计:\n")
cat("价差均值:  ", round(spread_mean, 2), "\n")
cat("价差标准差:", round(spread_std, 2), "\n")
cat("做空信号:  ", sum(signal == "做空价差"), "次\n")
cat("做多信号:  ", sum(signal == "做多价差"), "次\n")
cat("观望:      ", sum(signal == "观望"), "次\n")
cat("\n")

# 4.4 随机森林预测模型
cat("【4.4】随机森林涨跌预测\n")

features_rf <- data.frame(
  returns    = diff(close_price) / head(close_price, -1),
  volatility = runSD(diff(close_price) / head(close_price, -1), 20),
  ma5_gap    = (SMA(close_price, 5) - close_price) / close_price,
  ma20_gap   = (SMA(close_price, 20) - close_price) / close_price,
  rsi        = RSI(close_price, 14),
  volume_chg = diff(volume_data) / head(volume_data, -1)
)
features_rf <- features_rf[complete.cases(features_rf), ]

# 标签：次日涨跌
features_rf$label <- ifelse(lead(features_rf$returns) > 0, 1, 0)
features_rf <- features_rf[complete.cases(features_rf), ]
features_rf$label <- factor(features_rf$label, levels = c(0, 1), labels = c("跌", "涨"))

# 划分训练/测试集
set.seed(42)
train_idx <- createDataPartition(features_rf$label, p = 0.8, list = FALSE)
train_rf <- features_rf[train_idx, ]
test_rf  <- features_rf[-train_idx, ]

# 训练
rf_fit <- randomForest(
  label ~ ., data = train_rf,
  ntree = 500, mtry = 3, importance = TRUE
)

# 预测
pred_rf <- predict(rf_fit, test_rf)
cm <- confusionMatrix(pred_rf, test_rf$label)
cat("随机森林预测结果:\n")
print(cm$overall[c("Accuracy", "Kappa")])
cat("\n特征重要性:\n")
print(importance(rf_fit, type = 1))
cat("\n")


# ============================================================
# 模块五：风险管理体系
# ============================================================
cat("━━━ 模块五：风险管理体系 ━━━\n\n")

# 5.1 VaR计算
cat("【5.1】VaR在险价值\n")

portfolio_returns <- diff(log(close_price))

# 历史模拟法
var_hist <- quantile(portfolio_returns, 0.05)
cat("历史模拟法 95% VaR:", round(var_hist * 100, 4), "%\n")

# 参数法(正态)
var_param <- mean(portfolio_returns) - 1.645 * sd(portfolio_returns)
cat("参数法 95% VaR:     ", round(var_param * 100, 4), "%\n")

# CVaR (Expected Shortfall)
cvar_val <- mean(portfolio_returns[portfolio_returns <= var_hist])
cat("CVaR (ES):          ", round(cvar_val * 100, 4), "%\n")

# 蒙特卡洛模拟
set.seed(42)
n_sims <- 10000
sim_returns <- rnorm(n_sims, mean(portfolio_returns), sd(portfolio_returns))
var_mc <- quantile(sim_returns, 0.05)
cat("蒙特卡洛 95% VaR:   ", round(var_mc * 100, 4), "%\n")
cat("\n")

# 5.2 凯利公式
cat("【5.2】凯利公式仓位管理\n")

kelly_criterion <- function(win_rate, avg_win, avg_loss) {
  odds <- avg_win / avg_loss
  q    <- 1 - win_rate
  kelly      <- (odds * win_rate - q) / odds
  half_kelly <- kelly / 2

  cat("═══ 凯利公式仓位计算 ═══\n")
  cat("胜率:          ", paste0(win_rate * 100, "%"), "\n")
  cat("赔率(盈亏比):  ", round(odds, 2), "\n")
  cat("最优仓位(凯利):", paste0(round(kelly * 100, 1), "%"), "\n")
  cat("建议仓位(半凯利):", paste0(round(half_kelly * 100, 1), "%"), "\n")

  if (kelly <= 0) cat("⚠️ 凯利值为负，不应交易此策略\n")

  return(list(kelly = kelly, half_kelly = half_kelly))
}

result_kelly <- kelly_criterion(win_rate = 0.55, avg_win = 3000, avg_loss = 2000)
cat("\n")

# 5.3 最大回撤计算
cat("【5.3】最大回撤\n")

equity_curve <- cumsum(portfolio_returns)
running_max  <- cummax(equity_curve)
drawdown     <- running_max - equity_curve
max_dd       <- max(drawdown)

cat("最大回撤:", round(max_dd * 100, 2), "%\n")
cat("最大回撤持续期:", which.max(drawdown) - which(equity_curve == running_max[which.max(drawdown)])[1], "天\n")
cat("\n")


# ============================================================
# 模块六：期权定价模型
# ============================================================
cat("━━━ 模块六：期权定价与希腊字母 ━━━\n\n")

# 6.1 Black-Scholes定价
cat("【6.1】Black-Scholes期权定价\n")

bs_pricing <- function(S, K, T, r, sigma, type = "call") {
  #' Black-Scholes期权定价与希腊字母
  #' @param S 标的资产价格
  #' @param K 行权价格
  #' @param T 到期时间(年)
  #' @param r 无风险利率
  #' @param sigma 年化波动率
  #' @param type "call"或"put"

  d1 <- (log(S / K) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)

  if (type == "call") {
    price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
    delta <- pnorm(d1)
    rho   <- K * T * exp(-r * T) * pnorm(d2) / 100
  } else {
    price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
    delta <- pnorm(d1) - 1
    rho   <- -K * T * exp(-r * T) * pnorm(-d2) / 100
  }

  gamma <- dnorm(d1) / (S * sigma * sqrt(T))
  vega  <- S * dnorm(d1) * sqrt(T) / 100
  theta_call <- -(S * dnorm(d1) * sigma) / (2 * sqrt(T)) / 365 -
                r * K * exp(-r * T) * pnorm(d2) / 365

  cat("═══ Black-Scholes定价 ═══\n")
  cat("类型:       ", ifelse(type == "call", "看涨期权", "看跌期权"), "\n")
  cat("标的价:     ", S, "\n")
  cat("行权价:     ", K, "\n")
  cat("到期时间:   ", round(T * 365, 0), "天\n")
  cat("波动率:     ", paste0(sigma * 100, "%"), "\n")
  cat("─────────────────────\n")
  cat("期权价格:   ", round(price, 2), "\n")
  cat("Delta:      ", round(delta, 4), "\n")
  cat("Gamma:      ", round(gamma, 6), "\n")
  cat("Vega:       ", round(vega, 4), "\n")
  cat("Theta:      ", round(theta_call, 4), " (每日)\n")
  cat("Rho:        ", round(rho, 4), "\n")

  return(list(price = price, delta = delta, gamma = gamma,
              vega = vega, theta = theta_call, rho = rho))
}

# 示例：沪铜看涨期权
result_bs_call <- bs_pricing(S = 72000, K = 74000, T = 90/365,
                              r = 0.025, sigma = 0.25, type = "call")
cat("\n")
result_bs_put <- bs_pricing(S = 72000, K = 74000, T = 90/365,
                             r = 0.025, sigma = 0.25, type = "put")
cat("\n")

# 6.2 隐含波动率计算
cat("【6.2】隐含波动率计算\n")

implied_vol <- function(S, K, T, r, market_price, type = "call") {
  #' Newton-Raphson法求解隐含波动率
  sigma <- 0.3  # 初始猜测

  for (i in 1:100) {
    d1 <- (log(S / K) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T))
    d2 <- d1 - sigma * sqrt(T)

    if (type == "call") {
      price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
    } else {
      price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
    }

    vega <- S * dnorm(d1) * sqrt(T)

    diff <- price - market_price
    if (abs(diff) < 1e-6) break

    sigma <- sigma - diff / vega
  }

  cat("隐含波动率(IV):", round(sigma * 100, 2), "%\n")
  cat("迭代次数:      ", i, "\n")
  return(sigma)
}

iv_result <- implied_vol(S = 72000, K = 74000, T = 90/365,
                          r = 0.025, market_price = result_bs_call$price, type = "call")
cat("\n")


# ============================================================
# 模块七：套利交易策略
# ============================================================
cat("━━━ 模块七：套利交易策略 ━━━\n\n")

# 7.1 跨期套利回测
cat("【7.1】跨期套利回测\n")

set.seed(42)
n_arb <- 250
near_month <- cumsum(rnorm(n_arb, 2, 40)) + 3800
far_month  <- near_month + rnorm(n_arb, 80, 20)  # 远月升水

# 计算价差与z-score
spread_arb <- far_month - near_month
z_arb <- (spread_arb - mean(spread_arb)) / sd(spread_arb)

# 交易信号
position_arb <- ifelse(z_arb > 1.5, -1,
                ifelse(z_arb < -1.5, 1, 0))

# 计算收益
spread_return   <- c(0, diff(spread_arb))
strategy_return <- Lag(position_arb) * spread_return
strategy_return[1] <- 0

# 绩效评估
total_return <- sum(strategy_return, na.rm = TRUE)
sharpe_ratio <- mean(strategy_return, na.rm = TRUE) / sd(strategy_return, na.rm = TRUE) * sqrt(252)
cumret <- cumsum(strategy_return)
max_dd_arb <- max(cummax(cumret) - cumret)
win_rate_arb <- mean(strategy_return > 0, na.rm = TRUE)

cat("═══ 跨期套利策略绩效 ═══\n")
cat("总收益:   ", round(total_return, 2), "\n")
cat("夏普比率: ", round(sharpe_ratio, 2), "\n")
cat("最大回撤:  ", round(max_dd_arb, 2), "\n")
cat("胜率:     ", paste0(round(win_rate_arb * 100, 1), "%"), "\n")
cat("\n")

# 7.2 期现套利
cat("【7.2】期现套利机会识别\n")

spot_prices  <- seq(70000, 75000, by = 100)
future_prices <- spot_prices + rnorm(length(spot_prices), 200, 50)

arbitrage_check <- function(spot, future, r, u, T) {
  theoretical <- spot * exp((r + u) * T)
  basis       <- future - spot
  cost_carry  <- theoretical - spot

  arb_profit <- future - theoretical

  result <- data.frame(
    现货价   = spot,
    期货价   = future,
    理论期货价 = round(theoretical, 2),
    基差     = round(basis, 2),
    套利空间 = round(arb_profit, 2),
    方向     = ifelse(arb_profit > 0, "正向套利(卖期货买现货)",
               ifelse(arb_profit < -50, "反向套利(买期货卖现货)", "无套利空间"))
  )
  return(result)
}

arb_result <- arbitrage_check(spot_prices[1:5], future_prices[1:5],
                               r = 0.025, u = 0.008, T = 0.25)
print(arb_result)
cat("\n")


# ============================================================
# 模块八：NLP情绪分析
# ============================================================
cat("━━━ 模块八：前沿技术 — NLP情绪分析 ━━━\n\n")

# 简化版情绪词典
positive_words <- c("上涨", "利好", "突破", "支撑", "反弹", "增长",
                     "需求旺盛", "供应紧张", "超预期", "强势")
negative_words <- c("下跌", "利空", "跌破", "压力", "回调", "收缩",
                     "需求疲弱", "库存高企", "不及预期", "弱势")

analyze_sentiment <- function(text) {
  pos_count <- sum(sapply(positive_words, function(w) grepl(w, text)))
  neg_count <- sum(sapply(negative_words, function(w) grepl(w, text)))
  total <- pos_count + neg_count

  if (total == 0) return(list(score = 0, label = "中性", pos = 0, neg = 0))

  score <- (pos_count - neg_count) / total
  label <- ifelse(score > 0.3, "看多",
           ifelse(score < -0.3, "看空", "中性"))

  return(list(score = round(score, 2), label = label, pos = pos_count, neg = neg_count))
}

# 示例新闻
news_list <- c(
  "铜价突破关键阻力位，需求旺盛支撑上涨",
  "库存高企叠加需求疲弱，螺纹钢承压下跌",
  "原油供应紧张，市场预期OPEC将延长减产",
  "铁矿石价格窄幅震荡，多空因素交织"
)

cat("新闻情绪分析结果:\n")
for (news in news_list) {
  result <- analyze_sentiment(news)
  cat(sprintf("  [%s] %s (分数: %.2f, 正面: %d, 负面: %d)\n",
              result$label, news, result$score, result$pos, result$neg))
}


# ============================================================
# 总结
# ============================================================
cat("\n═══════════════════════════════════════════\n")
cat("  全部模块执行完毕！\n")
cat("  涵盖：定价理论 / 技术分析 / 基本面 / 量化模型\n")
cat("        风险管理 / 期权定价 / 套利策略 / NLP情绪\n")
cat("═══════════════════════════════════════════\n")
