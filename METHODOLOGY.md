# Methodology & Theoretical Framework

## Analysis 1: 2004 Market Leaders (Prospective Analysis)

### Research Question

How did portfolio optimization strategies perform using the five largest US companies by market capitalization in 2004? This analysis compares two theoretical optimization approaches (minimum variance and mean-variance) against the passive S&P 500 benchmark over a 21-year out-of-sample period.

### Stock Selection Methodology

The portfolio consists of the five largest US market cap companies as of November 2004 (at the start of the analysis period):
- **GE** (General Electric)
- **MSFT** (Microsoft)
- **XOM** (ExxonMobil)
- **PFE** (Pfizer)
- **C** (Citigroup)

Note: Each ticker symbol is a standard stock identifier used by the Yahoo Finance API via the quantmod package to automatically retrieve historical price data.

### Analysis Period

- **Total Duration:** November 2004 - November 2025 (21 years)
- **Training Data:** First 48 months (4 years) for initial portfolio construction
- **Out-of-Sample Testing:** 17 annual rebalancing cycles (2005-2025)

### Methodologies Employed

#### 1. Capital Asset Pricing Model (CAPM)

Estimates expected returns based on systematic risk (beta):

**Formula:** E(R) = Risk-Free Rate + Beta × (Market Return - Risk-Free Rate)

- Benchmark: S&P 500 index
- Calculates beta coefficient for each stock relative to market

#### 2. Global Minimum Variance Portfolio (GMVP)

Minimizes portfolio volatility regardless of expected returns:

**Closed-form solution:** w = Σ⁻¹·1 / (1ᵀ·Σ⁻¹·1)

- Optimal for risk-averse investors
- Focuses on downside protection

#### 3. Mean-Variance (MV) Efficient Portfolio

Maximizes portfolio return for a given risk level using Markowitz optimization. 
Target return = arithmetic mean of estimated individual stock returns (CAPM-based)

#### 4. Rolling-Window Backtesting

- **Expanding window:** Start with 48 months, grow by 12 months annually
- **Rebalancing:** Annual (each November)
- **No look-ahead bias:** Uses only historical data available at rebalancing date
- **Out-of-sample testing:** Ensures realistic, implementable strategy

#### 5. Transaction Cost Modeling

- **Market friction:** 10 basis points (0.1%) per rebalancing
- **Calculation:** Transaction costs = Portfolio Turnover × 0.1%
- **Comparison:** Gross returns (before costs) vs net returns (after costs)

#### 6. Inflation Adjustment

- **Real returns:** Deflated by Consumer Price Index (CPI)
- **Baseline:** November 2004 purchasing power
- **Coverage:** November 2004 - November 2024 (latest CPI available)
- **Purpose:** Assesses true purchasing power gains

### Data Sources

- **Stock prices:** Yahoo Finance (via quantmod::getSymbols)
- **S&P 500 benchmark:** Yahoo Finance ticker SPY
- **Risk-free rate:** Federal Reserve Economic Data (FRED) - 3M Treasury yield (DGS3MO)
- **Inflation data:** FRED - Consumer Price Index for All Urban Consumers (CPIAUCSL)

---

## Analysis 2: 2025 Market Leaders (Retrospective Analysis)

### Research Question

Could an investor in 2004 have predicted which companies would dominate the 2025 market? Probably not. This what-if analysis conducts a retrospective examination: we identify the five largest US companies by market cap as of November 2025, then backtest portfolio optimization strategies using only these five stocks over the entire 21-year period (2004-2025), comparing against the S&P 500 benchmark.

**Why this matters:** In November 2004, the future was uncertain. This analysis asks: "If you had perfect hindsight, how would you perform compared to the general US stock market?"

### Important Disclaimer

**This is retrospective analysis with perfect hindsight.** It does NOT demonstrate predictive ability or investment skill. Real investors in 2004 faced genuine uncertainty about these companies. The exceptional returns reflect market outcomes, not forecasting talent.

### Stock Selection Methodology

The five largest US market cap companies as of November 2025 (all trading continuously since November 2004):
- **MSFT** (Microsoft)
- **NVDA** (NVIDIA)
- **AAPL** (Apple)
- **AMZN** (Amazon)
- **BRK-A** (Berkshire Hathaway)

Each ticker is a standard stock identifier used by Yahoo Finance API (via quantmod package) to retrieve historical monthly price data.

### Analysis Period

- **Total Duration:** November 2004 - November 2025 (21 years)
- **Training period:** First 48 months (4 years) for initial optimization
- **Backtesting:** 17 annual rebalancing cycles
- **Total rebalancing events:** 21 annual portfolio adjustments

### Methodologies Employed

#### 1. Capital Asset Pricing Model (CAPM)

Estimates expected returns based on systematic risk (beta):

**Formula:** E(R) = Risk-Free Rate + Beta × (Market Return - Risk-Free Rate)

- Benchmark: S&P 500 index (SPY)
- Captures systematic risk of each stock

#### 2. Global Minimum Variance Portfolio (GMVP)

Minimizes portfolio volatility independent of returns:

**Closed-form solution:** w = Σ⁻¹·1 / (1ᵀ·Σ⁻¹·1)

- Optimal for risk-averse investors focused on downside protection

#### 3. Mean-Variance (MV) Efficient Portfolio

Maximizes portfolio return for a given risk level using Markowitz optimization. 
Target return = arithmetic mean of estimated individual stock returns (CAPM-based)

#### 4. Rolling-Window Backtesting

- **Expanding window:** Start with 48 months, grow by 12 months annually
- **Annual rebalancing:** Portfolio weights recalculated each November
- **Out-of-sample testing:** Uses only data available at rebalancing date
- **No look-ahead bias:** Ensures realistic, implementable strategy

#### 5. Transaction Cost Modeling

- **Market friction:** 10 basis points (0.1%) per rebalancing
- **Calculation:** Transaction costs = Portfolio Turnover × 0.1%
- **Comparison:** Gross returns (before costs) vs net returns (after costs)

#### 6. Inflation Adjustment

- **Real returns:** Deflated by Consumer Price Index (CPI)
- **Baseline:** November 2004 purchasing power
- **Coverage:** November 2004 - November 2024 (latest CPI available)
- **Purpose:** True economic gains in constant dollars

### Data Sources

- **Stock prices:** Yahoo Finance (via quantmod::getSymbols)
- **S&P 500 benchmark:** Yahoo Finance ticker SPY
- **Risk-free rate:** Federal Reserve (FRED code DGS3MO) - 3-Month Treasury
- **Inflation data:** Federal Reserve (FRED code CPIAUCSL) - Consumer Price Index

---

## Common Framework for Both Analyses

Both analyses employ identical methodological frameworks for optimal comparability. The key distinction is the cohort selection: 2004's market leaders (Analysis 1) versus 2025's market leaders applied retrospectively (Analysis 2).

### Capital Asset Pricing Model (CAPM)

The CAPM framework is used to estimate required returns based on each stock's systematic risk:

- **Alpha:** Intercept of regression (excess return not explained by beta)
- **Beta:** Slope coefficient measuring systematic risk relative to S&P 500
- **Risk-Free Rate:** Average 3-month Treasury yield over the period
- **Market Risk Premium:** Difference between S&P 500 return and risk-free rate

## Global Minimum Variance Portfolio (GMVP)

The GMVP solves the constrained optimization problem:

Minimize: w^T Σ w (portfolio variance)
Subject to: 
- Σ w_i = 1 (fully invested)
- 0 ≤ w_i ≤ 1 (no short selling, no leverage)

The closed-form solution is: **w = Σ⁻¹·1 / (1ᵀ·Σ⁻¹·1)**

Where:
- Σ = Covariance matrix of returns
- 1 = Vector of ones
- w_i = Weight of asset i in portfolio

### Mean-Variance Efficient Portfolio (MV)

The MV portfolio solves a constrained optimization problem:

Maximize: w^T μ - λ/2 · w^T Σ w (return minus risk penalty)
Subject to: 
- Σ w_i = 1 (fully invested)
- 0 ≤ w_i ≤ 1 (no short selling, no leverage)

Where:
- μ = Vector of expected returns (arithmetic mean of CAPM-derived individual stock returns)
- Σ = Covariance matrix of returns
- λ = Risk aversion parameter (implicit in setting target return = mean CAPM return)
- w_i = Weight of asset i in portfolio

### Rolling-Window Backtesting Framework

The backtesting procedure is designed to eliminate look-ahead bias and test implementable strategies:

1. **Window Expansion:** Begin with 48 months of historical data (Nov 2000 - Oct 2004)
2. **Initial Optimization:** Calculate GMVP and MV weights using first window
3. **Out-of-Sample Period:** Implement optimized weights from Nov 2004 to Oct 2005
4. **Annual Rebalancing:** Each November, expand historical window by 12 months and recalculate weights
5. **Continue:** Repeat steps 3-4 through November 2025

This ensures no future information is used in past decisions.

### Performance Metrics

**Annualized Return:** Geometric mean monthly return compounded to annual basis

**Annualized Volatility:** Standard deviation of monthly returns, scaled by √12

**Sharpe Ratio:** (Annualized Return - Risk-Free Rate) / Annualized Volatility

**Portfolio Turnover:** Σ|w_new_i - w_old_i| / 2 (sum of absolute weight changes)

### Expected Outputs

Both analyses generate:

- **CAPM Summary:** Beta coefficients and expected returns for each stock
- **Annual Performance:** Returns, volatility, Sharpe ratios, turnover metrics for each year
- **Wealth Evolution:** Dollar value of $1 invested, tracked annually (nominal and real)
- **Summary Statistics:** 21-year annualized metrics and performance comparison
- **Visualizations:** Two plots per analysis (nominal and inflation-adjusted wealth evolution)
- **CSV Exports:** Four detailed data files for reproducibility and external analysis
