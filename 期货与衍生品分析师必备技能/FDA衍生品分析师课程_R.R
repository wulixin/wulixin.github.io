# ============================================================
#  FDA衍生品分析师技能课程 — R语言实现
#  基于中国期货业协会FDA I/II/III级培训课程
# ============================================================

# ------------------------------------------------------------
# 0. 包加载
# ------------------------------------------------------------
required_packages <- c("stats", "ggplot2", "dplyr", "tidyr", "PerformanceAnalytics")

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
cat("  FDA衍生品分析师技能课程 — R语言实现\n")
cat("═══════════════════════════════════════════\n\n")


# ============================================================
# FDA I级: 微观经济学基础
# ============================================================
cat("━━━ FDA I级: 微观经济学基础 ━━━\n\n")

# I-1: 需求供给均衡模型
cat("【I-1】需求供给均衡模型\n")

equilibrium_model <- function(a, b, c, d) {
  #' 求均衡价格和数量
  #' 需求: Qd = a - bP
  #' 供给: Qs = c + dP
  P_star <- (a - c) / (b + d)
  Q_star <- a - b * P_star
  cat("═══ 均衡分析 ═══\n")
  cat("均衡价格: ", round(P_star, 2), "\n")
  cat("均衡数量: ", round(Q_star, 2), "\n")
  cat("需求弹性: ", round(b * P_star / Q_star, 4), "\n")
  cat("供给弹性: ", round(d * P_star / Q_star, 4), "\n")
  return(list(P = P_star, Q = Q_star))
}

equilibrium_model(a = 100, b = 2, c = 10, d = 3)
cat("\n")

# I-2: 股息折现模型(DDM)
cat("【I-2】股息折现模型(DDM)\n")

ddm_valuation <- function(D1, r, g, years = 5) {
  dividends <- D1 * (1 + g)^(0:(years - 1))
  pv <- dividends / (1 + r)^(1:years)
  terminal <- D1 * (1 + g)^years / (r - g)
  pv_terminal <- terminal / (1 + r)^years
  intrinsic_value <- sum(pv) + pv_terminal

  cat("═══ DDM估值 ═══\n")
  cat("当前股息: ", D1, "\n")
  cat("折现率: ", r * 100, "%\n")
  cat("增长率: ", g * 100, "%\n")
  cat("内在价值:", round(intrinsic_value, 2), "\n")
  cat("现价/内在价值比率: ", round(intrinsic_value / D1, 2), "%\n")
  return(intrinsic_value)
}

ddm_valuation(D1 = 2, r = 0.08, g = 0.03)
cat("\n")


# ============================================================
# FDA II级: 进阶内容
# ============================================================
cat("━━━ FDA II级: 进阶课程 ━━━\n\n")

# II-2: 回归分析
cat("【II-2】OLS回归分析\n")

ols_regression <- function() {
  set.seed(42)
  n <- 200
  X1 <- rnorm(n, 50, 15)
  X2 <- rnorm(n, 30, 10)
  Y <- 10 + 1.5 * X1 + 0.8 * X2 + rnorm(n, 0, 5)

  model <- lm(Y ~ X1 + X2)
  cat("═══ OLS回归结果 ═══\n")
  cat("截距: ", round(coef(model)[1], 4), "\n")
  cat("X1系数: ", round(coef(model)[2], 4), "\n")
  cat("X2系数: ", round(coef(model)[3], 4), "\n")
  cat("R²: ", round(summary(model)$r.squared, 4), "\n")
  cat("调整R²: ", round(summary(model)$adj.r.squared, 4), "\n")

  new_data <- data.frame(X1 = 55, X2 = 32)
  pred <- predict(model, new_data, interval = "prediction", level = 0.95)
  cat("预测值: ", round(pred[1], 2), "\n")
  cat("95%置信区间: [", round(pred[2], 2), ", ", round(pred[3], 2), "]\n")
  return(model)
}

ols_regression()
cat("\n")

# II-3: Black-Scholes期权定价
cat("【II-3】Black-Scholes期权定价\n")

bs_pricing <- function(S, K, T, r, sigma, type = "call") {
  d1 <- (log(S / K) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)

  if (type == "call") {
    price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
    delta <- pnorm(d1)
  } else {
    price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
    delta <- pnorm(d1) - 1
  }

  gamma <- dnorm(d1) / (S * sigma * sqrt(T))
  vega <- S * dnorm(d1) * sqrt(T) / 100
  theta <- -(S * dnorm(d1) * sigma) / (2 * sqrt(T)) / 365

  type_cn <- ifelse(type == "call", "看涨期权", "看跌期权")
  cat("═══ Black-Scholes定价 ═══\n")
  cat("类型: ", type_cn, "\n")
  cat("标的价: ", S, "\n")
  cat("行权价: ", K, "\n")
  cat("到期时间: ", round(T * 365, 0), "天\n")
  cat("波动率: ", sigma * 100, "%\n")
  cat("─────────────────────\n")
  cat("期权价格: ", round(price, 2), "\n")
  cat("Delta: ", round(delta, 4), "\n")
  cat("Gamma: ", round(gamma, 6), "\n")
  cat("Vega: ", round(vega, 4), "\n")
  cat("Theta: ", round(theta, 4), "\n")

  return(list(price = price, delta = delta, gamma = gamma, vega = vega, theta = theta))
}

bs_pricing(S = 72000, K = 74000, T = 90/365, r = 0.025, sigma = 0.25, type = "call")
cat("\n")

# II-4: 股指期货套期保值
cat("【II-4】股指期货套期保值\n")

hedge_ratio <- function(portfolio_beta, contract_value, index_value) {
  optimal_hedge_ratio <- portfolio_beta
  futures_needed <- (portfolio_beta * contract_value) / index_value

  cat("═══ 套期保值计算 ═══\n")
  cat("组合β系数: ", portfolio_beta, "\n")
  cat("合约乘数: ", index_value, "\n")
  cat("需要期货手数: ", round(futures_needed, 2), "手\n")

  return(futures_needed)
}

hedge_ratio(portfolio_beta = 1.2, contract_value = 150000, index_value = 300)
cat("\n")

# II-5: 利率互换定价
cat("【II-5】利率互换定价\n")

irs_pricing <- function(face_value, fixed_rate, float_rates, time_points) {
  discount_factors <- exp(-0.05 * time_points)
  fixed_leg <- face_value * fixed_rate * sum(discount_factors)
  fixed_leg <- fixed_leg + face_value * discount_factors[length(time_points)]
  float_leg <- face_value * sum(float_rates * discount_factors)

  cat("═══ 利率互换定价 ═══\n")
  cat("名义本金: ", face_value, "\n")
  cat("固定利率: ", fixed_rate * 100, "%\n")
  cat("固定端价值: ", round(fixed_leg, 2), "\n")
  cat("浮动端价值: ", round(float_leg, 2), "\n")
  cat("互换价值: ", round(float_leg - fixed_leg, 2), "\n")

  return(float_leg - fixed_leg)
}

irs_pricing(face_value = 1000000, fixed_rate = 0.035,
           float_rates = c(0.03, 0.032, 0.034, 0.036),
           time_points = c(0.25, 0.5, 0.75, 1))
cat("\n")

# II-6: 外汇远期定价
cat("【II-6】外汇远期定价\n")

fx_forward <- function(spot, r_domestic, r_foreign, T) {
  forward <- spot * (1 + r_domestic) / (1 + r_foreign)^T
  forward_points <- forward - spot
  premium_discount <- ifelse(forward_points > 0, "远期升水", "远期贴水")

  cat("═══ 外汇远期定价 ═══\n")
  cat("即期汇率: ", spot, "\n")
  cat("本币利率: ", r_domestic * 100, "%\n")
  cat("外币利率: ", r_foreign * 100, "%\n")
  cat("期限: ", T, "年\n")
  cat("远期汇率: ", round(forward, 4), "\n")
  cat("远期点数: ", round(forward_points, 2), "\n")
  cat("升贴水: ", premium_discount, "\n")

  return(forward)
}

fx_forward(spot = 7.25, r_domestic = 0.025, r_foreign = 0.05, T = 1)
cat("\n")


# ============================================================
# FDA III级: 高级内容
# ============================================================
cat("━━━ FDA III级: 高级课程 ━━━\n\n")

# III-2: CDS定价(简化)
cat("【III-2】信用违约互换(CDS)定价\n")

cds_pricing <- function(face_value, spread, default_prob, recovery = 0.4) {
  expected_loss <- default_prob * (1 - recovery)
  cds_value <- face_value * (spread - expected_loss)

  cat("═══ CDS定价 ═══\n")
  cat("名义本金: ", face_value, "\n")
  cat("CDS利差: ", spread * 10000, "bps\n")
  cat("违约概率: ", default_prob * 100, "%\n")
  cat("回收率: ", recovery * 100, "%\n")
  cat("期望损失: ", round(expected_loss, 4), "\n")
  cat("CDS价值: ", round(cds_value, 2), "\n")

  return(cds_value)
}

cds_pricing(face_value = 10000000, spread = 0.005, default_prob = 0.05)
cat("\n")

# III-3: 结构化产品定价
cat("【III-3】结构化产品定价\n")

structured_pricing <- function(principal, maturity, r, participation, underlying_price, strike) {
  bond_value <- principal * exp(-r * maturity)
  call_value <- pmax(underlying_price - strike, 0) * exp(-r * maturity)
  participation_rate <- (principal - bond_value) / call_value

  cat("═══ 结构化产品定价 ═══\n")
  cat("本金: ", principal, "\n")
  cat("期限: ", maturity, "年\n")
  cat("债券价值: ", round(bond_value, 2), "\n")
  cat("期权价值: ", round(call_value, 2), "\n")
  cat("参与率: ", round(participation_rate * 100, 1), "%\n")
  cat("实际参与率(用户设定): ", participation * 100, "%\n")

  return(list(bond = bond_value, option = call_value, participation = participation_rate))
}

structured_pricing(principal = 100000, maturity = 1, r = 0.03, participation = 0.8,
                underlying_price = 100, strike = 95)
cat("\n")

# III-4: 波动率指标计算
cat("【III-4】波动率与VIX计算\n")

volatility_metrics <- function(returns) {
  realized_vol <- sd(returns) * sqrt(252)
  variance_realized <- var(returns) * 252

  cat("═══ 波动率指标 ═══\n")
  cat("日收益率标准差: ", round(sd(returns), 6), "\n")
  cat("年化波动率: ", round(realized_vol * 100, 2), "%\n")
  cat("年化方差: ", round(variance_realized, 6), "\n")

  return(list(realized_vol = realized_vol, variance = variance_realized))
}

set.seed(42)
returns <- rnorm(252, 0.001, 0.015)
volatility_metrics(returns)
cat("\n")

# III-5: Vasicek利率模型模拟
cat("【III-5】Vasicek利率模型模拟\n")

vasicek_simulation <- function(r0, a, b, sigma, T, n_paths = 1000) {
  dt <- T / 252
  r <- matrix(0, n_paths, 252)
  r[, 1] <- r0

  for (t in 2:252) {
    dr <- a * (b - r[, t - 1]) * dt + sigma * sqrt(dt) * rnorm(n_paths)
    r[, t] <- r[, t - 1] + dr
  }

  final_rates <- r[, 252]
  cat("═══ Vasicek模型模拟 ═══\n")
  cat("初始利率: ", r0 * 100, "%\n")
  cat("均值回归速度: ", a, "\n")
  cat("长期均值: ", b * 100, "%\n")
  cat("波动率: ", sigma * 100, "%\n")
  cat("─────────────────────\n")
  cat("最终利率均值: ", round(mean(final_rates) * 100, 2), "%\n")
  cat("最终利率标准差: ", round(sd(final_rates) * 100, 2), "%\n")

  return(list(rates = r, final = final_rates))
}

vasicek_simulation(r0 = 0.03, a = 0.2, b = 0.04, sigma = 0.01)
cat("\n")

# III-5: Copula相关性
cat("【III-5】Copula相关性分析\n")

copula_correlation <- function(n, rho) {
  # 生成二元正态分布
  library(MASS)
  Sigma <- matrix(c(1, rho, rho, 1), 2, 2)
  data <- mvrnorm(n, c(0, 0), Sigma)

  # 转换为均匀分布
  u <- pnorm(data[, 1])
  v <- pnorm(data[, 2])

  # 计算尾部相关系数(简化)
  tail_dep <- mean(u < 0.05 & v < 0.05) / 0.05

  cat("═══ Copula相关性 ═══\n")
  cat("设定相关系数: ", rho, "\n")
  cat("样本量: ", n, "\n")
  cat("下尾相关系��: ", round(tail_dep, 4), "\n")

  return(list(u = u, v = v, tail_dep = tail_dep))
}

copula_correlation(n = 5000, rho = 0.7)
cat("\n")


# ============================================================
# 总结
# ============================================================
cat("═══════════════════════════════════════════\n")
cat("  全部模块执行完毕！\n")
cat("  FDA I级: 微观经济学/DDM/统计基础\n")
cat("  FDA II级: 回归分析/BS期权/套保/利率互换\n")
cat("  FDA III级: CDS/结构化产品/波动率/Vasicek/Copula\n")
cat("═══════════════════════════════════════════\n")