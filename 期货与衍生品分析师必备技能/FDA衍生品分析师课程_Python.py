# ============================================================
#  FDA衍生品分析师技能课程 — Python实现
#  基于中国期货业协会FDA I/II/III级培训课程
# ============================================================

import warnings
warnings.filterwarnings('ignore')

import numpy as np
import pandas as pd
from scipy import stats
from scipy.optimize import brentq

print("═══════════════════════════════════════════")
print("  FDA衍生品分析师技能课程 — Python实现")
print("═══════════════════════════════════════════\n")


# ============================================================
# FDA I级: 微观经济学基础
# ============================================================
print("━━━ FDA I级: 微观经济学基础 ━━━\n")

# I-1: 需求供给均衡模型
print("【I-1】需求供给均衡模型\n")

def equilibrium_model(a, b, c, d):
    """
    求均衡价格和数量
    需求: Qd = a - bP
    供给: Qs = c + dP
    """
    P_star = (a - c) / (b + d)
    Q_star = a - b * P_star
    demand_elasticity = b * P_star / Q_star
    supply_elasticity = d * P_star / Q_star

    print("═══ 均衡分析 ═══")
    print(f"均衡价格: {P_star:.2f}")
    print(f"均衡数量: {Q_star:.2f}")
    print(f"需求弹性: {demand_elasticity:.4f}")
    print(f"供给弹性: {supply_elasticity:.4f}")
    return {'P': P_star, 'Q': Q_star}

equilibrium_model(a=100, b=2, c=10, d=3)
print()

# I-2: 股息折现模型(DDM)
print("【I-2】股息折现模型(DDM)\n")

def ddm_valuation(D1, r, g, years=5):
    """DDM股息折现模型"""
    dividends = [D1 * (1 + g)**i for i in range(years)]
    pv = [d / (1 + r)**(i+1) for i, d in enumerate(dividends)]
    terminal = D1 * (1 + g)**years / (r - g)
    pv_terminal = terminal / (1 + r)**years
    intrinsic_value = sum(pv) + pv_terminal

    print("═══ DDM估值 ═══")
    print(f"当前股息: {D1}")
    print(f"折现率: {r*100}%")
    print(f"增长率: {g*100}%")
    print(f"内在价值: {intrinsic_value:.2f}")
    return intrinsic_value

ddm_valuation(D1=2, r=0.08, g=0.03)
print()


# ============================================================
# FDA II级: 进阶内容
# ============================================================
print("━━━ FDA II级: 进阶课程 ━━━\n")

# II-2: 回归分析
print("【II-2】OLS回归分析\n")

def ols_regression():
    np.random.seed(42)
    n = 200
    X1 = np.random.normal(50, 15, n)
    X2 = np.random.normal(30, 10, n)
    Y = 10 + 1.5 * X1 + 0.8 * X2 + np.random.normal(0, 5, n)

    # 使用numpy做OLS
    X = np.column_stack([np.ones(n), X1, X2])
    beta = np.linalg.lstsq(X, Y, rcond=None)[0]

    y_pred = X @ beta
    ss_res = np.sum((Y - y_pred)**2)
    ss_tot = np.sum((Y - np.mean(Y))**2)
    r_squared = 1 - ss_res / ss_tot

    print("═══ OLS回归结果 ═══")
    print(f"截距: {beta[0]:.4f}")
    print(f"X1系数: {beta[1]:.4f}")
    print(f"X2系数: {beta[2]:.4f}")
    print(f"R²: {r_squared:.4f}")

    # 预测
    new_X = np.array([1, 55, 32])
    pred = new_X @ beta
    print(f"预测值(X1=55,X2=32): {pred:.2f}")
    return beta

ols_regression()
print()

# II-3: Black-Scholes期权定价
print("【II-3】Black-Scholes期权定价\n")

def bs_pricing(S, K, T, r, sigma, option_type='call'):
    """Black-Scholes期权定价"""
    d1 = (np.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * np.sqrt(T))
    d2 = d1 - sigma * np.sqrt(T)

    if option_type == 'call':
        price = S * stats.norm.cdf(d1) - K * np.exp(-r * T) * stats.norm.cdf(d2)
        delta = stats.norm.cdf(d1)
    else:
        price = K * np.exp(-r * T) * stats.norm.cdf(-d2) - S * stats.norm.cdf(-d1)
        delta = stats.norm.cdf(d1) - 1

    gamma = stats.norm.pdf(d1) / (S * sigma * np.sqrt(T))
    vega = S * stats.norm.pdf(d1) * np.sqrt(T) / 100
    theta = -(S * stats.norm.pdf(d1) * sigma) / (2 * np.sqrt(T)) / 365

    type_cn = "看涨期权" if option_type == 'call' else "看跌期权"
    print("═══ Black-Scholes定价 ═══")
    print(f"类型: {type_cn}")
    print(f"标的价: {S}")
    print(f"行权价: {K}")
    print(f"到期时间: {T*365:.0f}天")
    print(f"波动率: {sigma*100}%")
    print("─────────────────────")
    print(f"期权价格: {price:.2f}")
    print(f"Delta: {delta:.4f}")
    print(f"Gamma: {gamma:.6f}")
    print(f"Vega: {vega:.4f}")
    print(f"Theta: {theta:.4f}")

    return {'price': price, 'delta': delta, 'gamma': gamma, 'vega': vega, 'theta': theta}

bs_pricing(S=72000, K=74000, T=90/365, r=0.025, sigma=0.25, option_type='call')
print()

# II-4: 股指期货套期保值
print("【II-4】股指期货套期保值\n")

def hedge_ratio(portfolio_beta, contract_value, index_value):
    """计算最优套保比和期货手数"""
    optimal_hedge_ratio = portfolio_beta
    futures_needed = (portfolio_beta * contract_value) / index_value

    print("═══ 套期保值计算 ═══")
    print(f"组合β系数: {portfolio_beta}")
    print(f"合约乘数: {index_value}")
    print(f"需要期货手数: {futures_needed:.2f}手")
    return futures_needed

hedge_ratio(portfolio_beta=1.2, contract_value=150000, index_value=300)
print()

# II-5: 利率互换定价
print("【II-5】利率互换定价\n")

def irs_pricing(face_value, fixed_rate, float_rates, time_points):
    """利率互换定价"""
    discount_factors = np.exp(-0.05 * time_points)
    fixed_leg = face_value * fixed_rate * np.sum(discount_factors)
    fixed_leg = fixed_leg + face_value * discount_factors[-1]
    float_leg = face_value * np.sum(float_rates * discount_factors)

    print("═══ 利率互换定价 ═══")
    print(f"名义本金: {face_value}")
    print(f"固定利率: {fixed_rate*100}%")
    print(f"固定端价值: {fixed_leg:.2f}")
    print(f"浮动端价值: {float_leg:.2f}")
    print(f"互换价值: {float_leg - fixed_leg:.2f}")
    return float_leg - fixed_leg

irs_pricing(face_value=1000000, fixed_rate=0.035,
           float_rates=[0.03, 0.032, 0.034, 0.036],
           time_points=[0.25, 0.5, 0.75, 1])
print()

# II-6: 外汇远期定价
print("【II-6】外汇远期定价\n")

def fx_forward(spot, r_domestic, r_foreign, T):
    """外汇远期定价 - 利率平价"""
    forward = spot * (1 + r_domestic) / (1 + r_foreign)**T
    forward_points = forward - spot
    premium_discount = "远期升水" if forward_points > 0 else "远期贴水"

    print("═══ 外汇远期定价 ═══")
    print(f"即期汇率: {spot}")
    print(f"本币利率: {r_domestic*100}%")
    print(f"外币利率: {r_foreign*100}%")
    print(f"期限: {T}年")
    print(f"远期汇率: {forward:.4f}")
    print(f"远期点数: {forward_points:.2f}")
    print(f"升贴水: {premium_discount}")
    return forward

fx_forward(spot=7.25, r_domestic=0.025, r_foreign=0.05, T=1)
print()


# ============================================================
# FDA III级: 高级内容
# ============================================================
print("━━━ FDA III级: 高级课程 ━━━\n")

# III-2: CDS定价(简化)
print("【III-2】信用违约互换(CDS)定价\n")

def cds_pricing(face_value, spread, default_prob, recovery=0.4):
    """CDS定价 - 简化模型"""
    expected_loss = default_prob * (1 - recovery)
    cds_value = face_value * (spread - expected_loss)

    print("═══ CDS定价 ═══")
    print(f"名义本金: {face_value}")
    print(f"CDS利差: {spread*10000}bps")
    print(f"违约概率: {default_prob*100}%")
    print(f"回收率: {recovery*100}%")
    print(f"期望损失: {expected_loss:.4f}")
    print(f"CDS价值: {cds_value:.2f}")
    return cds_value

cds_pricing(face_value=10000000, spread=0.005, default_prob=0.05)
print()

# III-3: 结构化产品定价
print("【III-3】结构化产品定价\n")

def structured_pricing(principal, maturity, r, participation, underlying_price, strike):
    """结构化产品定价"""
    bond_value = principal * np.exp(-r * maturity)
    call_value = max(underlying_price - strike, 0) * np.exp(-r * maturity)

    if call_value > 0:
        participation_rate = (principal - bond_value) / call_value
    else:
        participation_rate = 0

    print("═══ 结构化产品定价 ═══")
    print(f"本金: {principal}")
    print(f"期限: {maturity}年")
    print(f"债券价值: {bond_value:.2f}")
    print(f"期权价值: {call_value:.2f}")
    print(f"参与率: {participation_rate*100:.1f}%")
    print(f"实际参与率(用户设定): {participation*100}%")
    return {'bond': bond_value, 'option': call_value, 'participation': participation_rate}

structured_pricing(principal=100000, maturity=1, r=0.03, participation=0.8,
                underlying_price=100, strike=95)
print()

# III-4: 波动率指标计算
print("【III-4】波动率与VIX计算\n")

def volatility_metrics(returns):
    """波动率指标计算"""
    realized_vol = np.std(returns) * np.sqrt(252)
    variance_realized = np.var(returns) * 252

    print("═══ 波动率指标 ═══")
    print(f"日收益率标准差: {np.std(returns):.6f}")
    print(f"年化波动率: {realized_vol*100:.2f}%")
    print(f"年化方差: {variance_realized:.6f}")
    return {'realized_vol': realized_vol, 'variance': variance_realized}

np.random.seed(42)
returns = np.random.normal(0.001, 0.015, 252)
volatility_metrics(returns)
print()

# III-5: Vasicek利率模型模拟
print("【III-5】Vasicek利率模型模拟\n")

def vasicek_simulation(r0, a, b, sigma, T, n_paths=1000):
    """Vasicek利率模型模拟"""
    dt = T / 252
    r = np.zeros((n_paths, 252))
    r[:, 0] = r0

    for t in range(1, 252):
        dr = a * (b - r[:, t-1]) * dt + sigma * np.sqrt(dt) * np.random.normal(0, 1, n_paths)
        r[:, t] = r[:, t-1] + dr

    final_rates = r[:, -1]

    print("═══ Vasicek模型模拟 ═══")
    print(f"初始利率: {r0*100}%")
    print(f"均值回归速度: {a}")
    print(f"长期均值: {b*100}%")
    print(f"波动率: {sigma*100}%")
    print("─────────────────────")
    print(f"最终利率均值: {np.mean(final_rates)*100:.2f}%")
    print(f"最终利率标准差: {np.std(final_rates)*100:.2f}%")

    return {'rates': r, 'final': final_rates}

vasicek_simulation(r0=0.03, a=0.2, b=0.04, sigma=0.01)
print()

# III-5: Copula相关性
print("【III-5】Copula相关性分析\n")

def copula_correlation(n, rho):
    """Copula相关性分析"""
    # 生成二元正态分布
    mean = [0, 0]
    cov = [[1, rho], [rho, 1]]
    data = np.random.multivariate_normal(mean, cov, n)

    # 转换为均匀分布
    u = stats.norm.cdf(data[:, 0])
    v = stats.norm.cdf(data[:, 1])

    # 计算下尾相关系数
    tail_dep = np.mean((u < 0.05) & (v < 0.05)) / 0.05

    print("═══ Copula相关性 ═══")
    print(f"设定相关系数: {rho}")
    print(f"样本量: {n}")
    print(f"下尾相关系数: {tail_dep:.4f}")

    return {'u': u, 'v': v, 'tail_dep': tail_dep}

copula_correlation(n=5000, rho=0.7)
print()


# ============================================================
# 总结
# ============================================================
print("═══════════════════════════════════════════")
print("  全部模块执行完毕！")
print("  FDA I级: 微观经济学/DDM/统计基础")
print("  FDA II级: 回归分析/BS期权/套保/利率互换")
print("  FDA III级: CDS/结构化产品/波动率/Vasicek/Copula")
print("═══════════════════════════════════════════")
