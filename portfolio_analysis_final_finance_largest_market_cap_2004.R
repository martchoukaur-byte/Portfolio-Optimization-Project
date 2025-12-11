rm(list = ls())
# ============================================================
# INSTALLATION AND LOADING OF PACKAGES
# ============================================================
install.packages("quantmod")
library(quantmod)

install.packages("dplyr")
library(dplyr)

install.packages("tidyr")
library(tidyr)

install.packages("zoo")
library(zoo)

# ============================================================
# QUESTION 1: COMPUTE MEAN, STANDARD DEVIATION, AND SHARPE RATIO
# ============================================================

# ============================================================
# STOCK 1: GE General Electric	
# ============================================================
getSymbols("GE", from = "1999-11-01", to = "2025-11-02", periodicity = "monthly")
data <- as.data.frame(GE)
colnames(data) <- c("open", "high", "low", "close", "volume", "adjusted_close")
# Ajouter la colonne date
data$date <- rownames(data)
rownames(data) <- NULL
# Convertir date en format Date
data$date <- as.Date(data$date)
# Réorganiser pour avoir date en premier
data <- data[, c("date", "open", "high", "low", "close", "adjusted_close", "volume")]

data <- data %>%
  mutate(
    returns = (adjusted_close / lag(adjusted_close) - 1) * 100,
    compound_growth = adjusted_close / adjusted_close[1]
  )

prices <- na.omit(data$adjusted_close)
returns <- na.omit(data$returns)
std_dev <- sd(returns)

View(data)

# ============================================================
# STOCK 2: XOM Exxon Mobil
# ============================================================
getSymbols("XOM", from = "1999-11-01", to = "2025-11-02", periodicity = "monthly")
data2 <- as.data.frame(XOM)
colnames(data2) <- c("open", "high", "low", "close", "volume", "adjusted_close")
# Ajouter la colonne date
data2$date <- rownames(data2)
rownames(data2) <- NULL
# Convertir date en format Date
data2$date <- as.Date(data2$date)
# Réorganiser pour avoir date en premier
data2 <- data2[, c("date", "open", "high", "low", "close", "adjusted_close", "volume")]

data2 <- data2 %>%
  mutate(
    returns = (adjusted_close / lag(adjusted_close) - 1) * 100,
    compound_growth = adjusted_close / adjusted_close[1]
  )

prices2 <- data2$adjusted_close
returns2 <- na.omit(data2$returns)
std_dev2 <- sd(returns2)

View(data2)

# ============================================================
# STOCK 3: MSFT Microsoft
# ============================================================
getSymbols("MSFT", from = "1999-11-01", to = "2025-11-02", periodicity = "monthly")
data3 <- as.data.frame(MSFT)
colnames(data3) <- c("open", "high", "low", "close", "volume", "adjusted_close")
# Ajouter la colonne date
data3$date <- rownames(data3)
rownames(data3) <- NULL
# Convertir date en format Date
data3$date <- as.Date(data3$date)
# Réorganiser pour avoir date en premier
data3 <- data3[, c("date", "open", "high", "low", "close", "adjusted_close", "volume")]

data3 <- data3 %>%
  mutate(
    returns = (adjusted_close / lag(adjusted_close) - 1) * 100,
    compound_growth = adjusted_close / adjusted_close[1]
  )

prices3 <- data3$adjusted_close
returns3 <- na.omit(data3$returns)
std_dev3 <- sd(returns3)

View(data3)

# ============================================================
# STOCK 4: C Citigroup
# ============================================================
getSymbols("C", from = "1999-11-01", to = "2025-11-02", periodicity = "monthly")
data4 <- as.data.frame(C)
colnames(data4) <- c("open", "high", "low", "close", "volume", "adjusted_close")
# Ajouter la colonne date
data4$date <- rownames(data4)
rownames(data4) <- NULL
# Convertir date en format Date
data4$date <- as.Date(data4$date)
# Réorganiser pour avoir date en premier
data4 <- data4[, c("date", "open", "high", "low", "close", "adjusted_close", "volume")]

data4 <- data4 %>%
  mutate(
    returns = (adjusted_close / lag(adjusted_close) - 1) * 100,
    compound_growth = adjusted_close / adjusted_close[1]
  )

prices4 <- data4$adjusted_close
returns4 <- na.omit(data4$returns)
std_dev4 <- sd(returns4)

View(data4)

# ============================================================
# STOCK 5: PFE Pfizer
# ============================================================
getSymbols("PFE", from = "1999-11-01", to = "2025-11-02", periodicity = "monthly")
data5 <- as.data.frame(PFE)
colnames(data5) <- c("open", "high", "low", "close", "volume", "adjusted_close")
# Ajouter la colonne date
data5$date <- rownames(data5)
rownames(data5) <- NULL
# Convertir date en format Date
data5$date <- as.Date(data5$date)
# Réorganiser pour avoir date en premier
data5 <- data5[, c("date", "open", "high", "low", "close", "adjusted_close", "volume")]


data5 <- data5 %>%
  mutate(
    returns = (adjusted_close / lag(adjusted_close) - 1) * 100,
    compound_growth = adjusted_close / adjusted_close[1]
  )

prices5 <- data5$adjusted_close
returns5 <- na.omit(data5$returns)
std_dev5 <- sd(returns5)

View(data5)

# ============================================================
# Computing standard deviations for all stocks
# ============================================================
sd1 <- sd(data$returns, na.rm = TRUE)
sd2 <- sd(data2$returns, na.rm = TRUE)
sd3 <- sd(data3$returns, na.rm = TRUE)
sd4 <- sd(data4$returns, na.rm = TRUE)
sd5 <- sd(data5$returns, na.rm = TRUE)

# ============================================================
# Add stock identifier and merge all datasets
# ============================================================
data <- data %>% mutate(stock = "Stock1")
data2 <- data2 %>% mutate(stock = "Stock2")
data3 <- data3 %>% mutate(stock = "Stock3")
data4 <- data4 %>% mutate(stock = "Stock4")
data5 <- data5 %>% mutate(stock = "Stock5")

data_all <- bind_rows(data, data2, data3, data4, data5)

# ============================================================
# Compute annualized monthly returns
# ============================================================
mean_return1 <- (1 + (data$adjusted_close[313] - data$adjusted_close[1]) / data$adjusted_close[1])^(1/313)
mean_return2 <- (1 + (data2$adjusted_close[313] - data2$adjusted_close[1]) / data2$adjusted_close[1])^(1/313)
mean_return3 <- (1 + (data3$adjusted_close[313] - data3$adjusted_close[1]) / data3$adjusted_close[1])^(1/313)
mean_return4 <- (1 + (data4$adjusted_close[313] - data4$adjusted_close[1]) / data4$adjusted_close[1])^(1/313)
mean_return5 <- (1 + (data5$adjusted_close[313] - data5$adjusted_close[1]) / data5$adjusted_close[1])^(1/313)

# ============================================================
# Risk-free rate (3-month Treasury)
# ============================================================
getSymbols("DGS3MO", src = "FRED", from = "1999-11-01", to = "2025-11-02")
rf_3m <- as.data.frame(DGS3MO)
rf_3m$date <- rownames(rf_3m)
rownames(rf_3m) <- NULL
colnames(rf_3m) <- c("value", "date")
rf_3m$date <- as.Date(rf_3m$date)
View(rf_3m)
rf_3m <- rf_3m %>%
  mutate(ym = format(date, "%Y-%m"))

rf_3m <- rf_3m %>%
  group_by(ym) %>%
  filter(date == min(date)) %>%
  ungroup()

# ============================================================
# S&P 500 Index (via SPY)
# ============================================================
getSymbols("SPY", from = "1999-11-01", to = "2025-11-02", periodicity = "monthly")
# Convertir en dataframe
sp500_monthly <- as.data.frame(SPY)
# Ajouter et renommer les colonnes
sp500_monthly$date <- rownames(sp500_monthly)
rownames(sp500_monthly) <- NULL
colnames(sp500_monthly) <- c("open", "high", "low", "close", "volume", "adjusted_close", "date")
# Réorganiser pour avoir date en premier
sp500_monthly <- sp500_monthly[, c("date", "open", "high", "low", "close", "adjusted_close", "volume")]
# Convertir date en Date
sp500_monthly$date <- as.Date(sp500_monthly$date)

sp500_monthly <- sp500_monthly %>%
  mutate(
    returns = (adjusted_close / lag(adjusted_close) - 1) * 100,
    compound_growth = adjusted_close / adjusted_close[1]
  )

prices_sp500 <- sp500_monthly$adjusted_close
returns_sp500 <- na.omit(sp500_monthly$returns)
std_dev_sp500 <- sd(returns_sp500)

# ============================================================
# Compute covariances and betas (full dataset)
# ============================================================
cov_stock1_sp500 <- cov(returns, returns_sp500)
cov_stock2_sp500 <- cov(returns2, returns_sp500)
cov_stock3_sp500 <- cov(returns3, returns_sp500)
cov_stock4_sp500 <- cov(returns4, returns_sp500)
cov_stock5_sp500 <- cov(returns5, returns_sp500)

variance_sp500 <- var(returns_sp500, na.rm = TRUE)

beta1 <- cov(returns, returns_sp500) / var(returns_sp500, na.rm = TRUE)
beta2 <- cov(returns2, returns_sp500) / var(returns_sp500, na.rm = TRUE)
beta3 <- cov(returns3, returns_sp500) / var(returns_sp500, na.rm = TRUE)
beta4 <- cov(returns4, returns_sp500) / var(returns_sp500, na.rm = TRUE)
beta5 <- cov(returns5, returns_sp500) / var(returns_sp500, na.rm = TRUE)

# ============================================================
# Extract first 150 observations
# ============================================================
View(sp500_monthly)

data_150_obs <- data[1:150, ]
data2_150_obs <- data2[1:150, ]
data3_150_obs <- data3[1:150, ]
data4_150_obs <- data4[1:150, ]
data5_150_obs <- data5[1:150, ]
sp500_monthly_150_obs <- sp500_monthly[1:150, ]

returns_150_obs <- na.omit(data_150_obs$returns)
returns2_150_obs <- na.omit(data2_150_obs$returns)
returns3_150_obs <- na.omit(data3_150_obs$returns)
returns4_150_obs <- na.omit(data4_150_obs$returns)
returns5_150_obs <- na.omit(data5_150_obs$returns)
returns_sp500_150_obs <- na.omit(sp500_monthly_150_obs$returns)

# ============================================================
# Compute covariances and betas (first 150 observations)
# ============================================================
cov_stock1_sp500_150 <- cov(returns_150_obs, returns_sp500_150_obs)
cov_stock2_sp500_150 <- cov(returns2_150_obs, returns_sp500_150_obs)
cov_stock3_sp500_150 <- cov(returns3_150_obs, returns_sp500_150_obs)
cov_stock4_sp500_150 <- cov(returns4_150_obs, returns_sp500_150_obs)
cov_stock5_sp500_150 <- cov(returns5_150_obs, returns_sp500_150_obs)

variance_sp500_150 <- var(returns_sp500_150_obs, na.rm = TRUE)

beta1_150 <- cov(returns_150_obs, returns_sp500_150_obs) / var(returns_sp500_150_obs, na.rm = TRUE)
beta2_150 <- cov(returns2_150_obs, returns_sp500_150_obs) / var(returns_sp500_150_obs, na.rm = TRUE)
beta3_150 <- cov(returns3_150_obs, returns_sp500_150_obs) / var(returns_sp500_150_obs, na.rm = TRUE)
beta4_150 <- cov(returns4_150_obs, returns_sp500_150_obs) / var(returns_sp500_150_obs, na.rm = TRUE)
beta5_150 <- cov(returns5_150_obs, returns_sp500_150_obs) / var(returns_sp500_150_obs, na.rm = TRUE)

returns_sp500_150_obs <- as.numeric(na.omit(sp500_monthly_150_obs$returns))
View(returns_sp500_150_obs)

# ============================================================
# QUESTION 3: CAPM EXPECTED RETURNS
# ============================================================

# ============================================================
# Function: compute geometric mean of returns
# ============================================================
geometric_mean_returns <- function(x) {
  ((prod(1 + x/100, na.rm = TRUE))^(1/length(na.omit(x))) - 1)
}

# ============================================================
# S&P 500 mean returns (first 150 observations)
# ============================================================
mean_return_sp500_150_monthly <- geometric_mean_returns(returns_sp500_150_obs)

mean_return_sp500_150 <- ((1 + mean_return_sp500_150_monthly)^12) - 1

mean_return_sp500_150_monthly_pct <- mean_return_sp500_150_monthly*100
mean_return_sp500_150_pct <- mean_return_sp500_150*100

# ============================================================
# Risk-free rate (first 150 observations)
# ============================================================
rf_capm_150 <- rf_3m[1:150, ] %>% select(-ym)
rf_capm_150

rf_capm_150$value <- as.numeric(as.character(rf_capm_150$value))
mean_rf_capm_150 <- mean(rf_capm_150$value, na.rm = TRUE)
mean_rf_capm_150

# ============================================================
# CAPM expected returns (first 150 observations)
# ============================================================
# Formula: E(Ri) = Rf + βi × (E(Rm) - Rf)
expected_return_1_150 <- mean_rf_capm_150 + beta1_150 * (mean_return_sp500_150_pct - mean_rf_capm_150)
expected_return_2_150 <- mean_rf_capm_150 + beta2_150 * (mean_return_sp500_150_pct - mean_rf_capm_150)
expected_return_3_150 <- mean_rf_capm_150 + beta3_150 * (mean_return_sp500_150_pct - mean_rf_capm_150)
expected_return_4_150 <- mean_rf_capm_150 + beta4_150 * (mean_return_sp500_150_pct - mean_rf_capm_150)
expected_return_5_150 <- mean_rf_capm_150 + beta5_150 * (mean_return_sp500_150_pct - mean_rf_capm_150)

# ============================================================
# S&P 500 mean returns (full dataset)
# ============================================================
mean_return_sp500_monthly <- geometric_mean_returns(returns_sp500)
mean_return_sp500_monthly

mean_return_sp500 <- ((1 + mean_return_sp500_monthly)^12) - 1
mean_return_sp500

mean_return_sp500_monthly_pct <- mean_return_sp500_monthly*100
mean_return_sp500_pct <- mean_return_sp500*100

# ============================================================
# Risk-free rate (full dataset)
# ============================================================
rf_capm_all <- rf_3m %>% select(-date, -ym)
rf_capm_all

rf_capm_all$value <- as.numeric(as.character(rf_capm_all$value))
mean_rf_capm_all <- mean(rf_capm_all$value, na.rm = TRUE)
mean_rf_capm_all

# ============================================================
# CAPM expected returns (full dataset)
# ============================================================
# Formula: E(Ri) = Rf + βi × (E(Rm) - Rf)
expected_return_1 <- mean_rf_capm_all + beta1 * (mean_return_sp500_pct - mean_rf_capm_all)
expected_return_2 <- mean_rf_capm_all + beta2 * (mean_return_sp500_pct - mean_rf_capm_all)
expected_return_3 <- mean_rf_capm_all + beta3 * (mean_return_sp500_pct - mean_rf_capm_all)
expected_return_4 <- mean_rf_capm_all + beta4 * (mean_return_sp500_pct - mean_rf_capm_all)
expected_return_5 <- mean_rf_capm_all + beta5 * (mean_return_sp500_pct - mean_rf_capm_all)

# ============================================================
# Scatterplot: CAPM expected returns (first 150 vs full dataset)
# ============================================================
expected_returns_150 <- c(expected_return_1_150, expected_return_2_150, expected_return_3_150, expected_return_4_150, expected_return_5_150)
expected_returns_all <- c(expected_return_1, expected_return_2, expected_return_3, expected_return_4, expected_return_5)
stock_names <- c("Stock 1", "Stock 2", "Stock 3", "Stock 4", "Stock 5")

scatter_data <- data.frame(
  Stock = stock_names,
  Expected_Return_150 = expected_returns_150,
  Expected_Return_All = expected_returns_all
)

install.packages("ggplot2")
library(ggplot2)

ggplot(scatter_data, aes(x = Expected_Return_150, y = Expected_Return_All, label = Stock)) +
  geom_point(size = 3, color = "blue") +
  geom_text(vjust = -0.7, hjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  xlab("Expected Return (First 150 Observations, %)") +
  ylab("Expected Return (All Observations, %)") +
  ggtitle("CAPM Expected Returns: First 150 vs All Observations") +
  theme_minimal()

# ============================================================
# QUESTION 2: PLOT EVOLUTION OF INVESTMENT VALUE
# ============================================================

# ============================================================
# Prepare data for returns matrix
# ============================================================
data_all_date_stock_return <- data_all %>% 
  select(-open, -high, -low, -close, -adjusted_close, -volume,
        -compound_growth)
# ============================================================
# Build returns matrix (long to wide format)
# ============================================================
data_all_date_stock_return <- data_all_date_stock_return %>%
  mutate(yearmonth = format(as.Date(date), "%Y-%m")) %>%
  select(-date)

data_wide <- data_all_date_stock_return %>%
  pivot_wider(
    names_from = stock,
    values_from = returns
  )

data_wide_numeric <- data_wide %>% select(-yearmonth)
View(data_wide_numeric)
View(data_all_date_stock_return)

data_wide <- data_wide[-1, ]
data_wide_numeric <- data_wide_numeric[-1, ]

anyNA(data_wide)

# ============================================================
# QUESTIONS 4-6: PORTFOLIO OPTIMIZATION AND OUT-OF-SAMPLE ANALYSIS
# ============================================================

# ============================================================
# Function: compute global minimum variance portfolio (GMVP) weights
# ============================================================
calculate_gmvp_weights <- function(window_data) {
  
  # Compute covariance matrix
  sigma_mat <- cov(window_data)
  
  # Create vector of ones
  n_assets <- ncol(window_data)
  one_vec <- rep(1, n_assets)
  
  # Invert covariance matrix
  sigma_inv <- solve(sigma_mat)
  
  # Compute numerator: (Σ^-1) × 1
  top <- sigma_inv %*% one_vec
  
  # Compute denominator: 1^T × (Σ^-1) × 1
  bottom <- as.numeric(t(one_vec) %*% sigma_inv %*% one_vec)
  
  # Compute GMVP weights: w = (Σ^-1 × 1) / (1^T × Σ^-1 × 1)
  gmvp_weights <- as.vector(top) / bottom
  
  # Compute portfolio variance: w^T × Σ × w
  gmvp_var <- as.numeric(t(gmvp_weights) %*% sigma_mat %*% gmvp_weights)
  
  # Compute portfolio volatility: sqrt(variance)
  gmvp_vol <- sqrt(gmvp_var)
  
  return(list(weights = gmvp_weights, volatility = gmvp_vol))
}

# ============================================================
# Test GMVP function
# ============================================================
results_list <- calculate_gmvp_weights(data_wide_numeric)
print(results_list$weights)
print(results_list$volatility)

# ============================================================
# Function: compute mean-variance portfolio with target return
# ============================================================
calculate_mv_portfolio <- function(window_data, target_return = NULL) {
  # Covariance matrix and mean vector
  sigma_mat <- cov(window_data)
  mu_vec <- colMeans(window_data)
  n_assets <- length(mu_vec)
  one_vec <- rep(1, n_assets)
  
  sigma_inv <- solve(sigma_mat)
  
  # Default target = mean of expected returns
  if (is.null(target_return)) {
    target_return <- mean(mu_vec)
  }
  
  # Compute Markowitz scalars
  A <- as.numeric(t(one_vec) %*% sigma_inv %*% one_vec)
  B <- as.numeric(t(mu_vec) %*% sigma_inv %*% one_vec)
  C <- as.numeric(t(mu_vec) %*% sigma_inv %*% mu_vec)
  D <- A * C - B^2
  
  # Analytic formula for weights
  w_mv <- ((C - B * target_return) * (sigma_inv %*% one_vec) + (A * target_return - B) * (sigma_inv %*% mu_vec)) / D
  
  # Risk, volatility, expected return
  mv_var <- as.numeric(t(w_mv) %*% sigma_mat %*% w_mv)
  mv_vol <- sqrt(mv_var)
  mv_ret <- as.numeric(t(w_mv) %*% mu_vec)
  
  return(list(weights = as.vector(w_mv), volatility = mv_vol, expected_return = mv_ret, target = target_return))
}

# ============================================================
# Setup rolling window parameters
# ============================================================
initial_periods <- 48
periods_per_year <- 12
n_years <- 22
returns_matrix <- data_wide %>% select(-yearmonth) %>% as.matrix()
results_list <- list()
# ============================================================
# Main loop: annual portfolio optimization
# ============================================================
for (year in 1:n_years) {
  end_period <- initial_periods + (year * periods_per_year)
  window_data <- returns_matrix[1:end_period, ]
  
  # Compute GMVP
  gmvp_result <- calculate_gmvp_weights(window_data)
  # Compute mean-variance portfolio
  mv_result <- calculate_mv_portfolio(window_data)
  
  # Store results
  results_list[[year]] <- list(
    Year = year,
    Periods = end_period,
    GMVP_weights = gmvp_result$weights,
    GMVP_vol = gmvp_result$volatility,
    MV_weights = mv_result$weights,
    MV_vol = mv_result$volatility
  )
}

# ============================================================
# Annual growth calculations & performance indicators
# ============================================================
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# Transaction cost rate (e.g., 0.1% per transaction)
transaction_cost_rate <- 0.001

results_mean_list <- list()

for (year in 1:(length(results_list)-1)) {
  
  # Define periods
  initial_period <- initial_periods + year * periods_per_year
  end_period <- initial_periods + ((year+1) * periods_per_year) - 1
  
  # Extract data for the year
  monthly_returns_by_stock <- returns_matrix[initial_period:end_period, ]
  rf_vector_mean <- mean(rf_capm_all[initial_period:end_period, ]$value, na.rm = TRUE)
  
  # Compute turnover (portfolio weight changes)
  if (year == 1) {
    # First year: assume initially empty portfolio (turnover = sum of absolute weights)
    turnover_gmvp <- sum(abs(results_list[[year]]$GMVP_weights))
    turnover_mv <- sum(abs(results_list[[year]]$MV_weights))
  } else {
    # Subsequent years: sum of absolute weight changes
    turnover_gmvp <- sum(abs(results_list[[year]]$GMVP_weights - results_list[[year-1]]$GMVP_weights))
    turnover_mv <- sum(abs(results_list[[year]]$MV_weights - results_list[[year-1]]$MV_weights))
  }
  
  # Transaction costs
  transaction_costs_gmvp <- turnover_gmvp * transaction_cost_rate
  transaction_costs_mv <- turnover_mv * transaction_cost_rate
  
  # Weighted portfolio returns
  returns_by_month_gmvp <- sweep(monthly_returns_by_stock, 2, results_list[[year]]$GMVP_weights, "*")
  returns_by_month_mv <- sweep(monthly_returns_by_stock, 2, results_list[[year]]$MV_weights, "*")
  
  # Total portfolio return each month
  total_monthly_return_gmvp <- as.numeric(rowSums(returns_by_month_gmvp, na.rm = TRUE))
  total_monthly_return_mv <- as.numeric(rowSums(returns_by_month_mv, na.rm = TRUE))
  
  # Annual compound return (gross, before transaction costs)
  annual_return_gmvp_gross <- (prod(1 + total_monthly_return_gmvp/100) - 1) * 100
  annual_return_mv_gross <- (prod(1 + total_monthly_return_mv/100) - 1) * 100
  
  # Annual return net (after transaction costs)
  annual_return_gmvp <- annual_return_gmvp_gross - (transaction_costs_gmvp * 100)
  annual_return_mv <- annual_return_mv_gross - (transaction_costs_mv * 100)
  
  # Monthly volatility
  sd_gmvp <- sd(total_monthly_return_gmvp, na.rm = TRUE)
  sd_mv <- sd(total_monthly_return_mv, na.rm = TRUE)
  
  # Excess returns and Sharpe ratio (using net returns)
  excess_returns_gmvp <- annual_return_gmvp - rf_vector_mean
  excess_returns_mv <- annual_return_mv - rf_vector_mean
  sharpe_gmvp <- excess_returns_gmvp / sd_gmvp
  sharpe_mv <- excess_returns_mv / sd_mv
  
  # Skewness and kurtosis
  skewness_gmvp <- skewness(total_monthly_return_gmvp)
  skewness_mv <- skewness(total_monthly_return_mv)
  kurtosis_gmvp <- kurtosis(total_monthly_return_gmvp)
  kurtosis_mv <- kurtosis(total_monthly_return_mv)
  
  # Store results
  results_mean_list[[year]] <- list(
    Year = year,
    first_observation = initial_period,
    last_observation = end_period,
    rf_mean = rf_vector_mean,
    # Transaction costs
    turnover_gmvp = turnover_gmvp,
    turnover_mv = turnover_mv,
    transaction_costs_gmvp = transaction_costs_gmvp,
    transaction_costs_mv = transaction_costs_mv,
    # Monthly returns
    monthly_returns_gmvp = total_monthly_return_gmvp,
    monthly_returns_mv = total_monthly_return_mv,
    # Annual returns
    annual_return_gmvp_gross = annual_return_gmvp_gross,
    annual_return_mv_gross = annual_return_mv_gross,
    annual_return_gmvp = annual_return_gmvp,
    annual_return_mv = annual_return_mv,
    # Volatility
    volatility_gmvp = sd_gmvp,
    volatility_mv = sd_mv,
    # Sharpe ratio
    sharpe_gmvp = sharpe_gmvp,
    sharpe_mv = sharpe_mv,
    # Additional statistics
    skewness_gmvp = skewness_gmvp,
    skewness_mv = skewness_mv,
    kurtosis_gmvp = kurtosis_gmvp,
    kurtosis_mv = kurtosis_mv
  )
}

# ============================================================
# QUESTION 6: WEALTH EVOLUTION OVER TIME
# ============================================================

# ============================================================
# Cumulative wealth trajectory
# ============================================================
# Initialize wealth (starting with 1 unit)
wealth_gmvp_net <- 1
wealth_gmvp_gross <- 1
wealth_mv_net <- 1
wealth_mv_gross <- 1
wealth_sp500 <- 1

# Initialize dataframe
wealth_data <- data.frame(
  Year = 0,
  Wealth_GMVP_Net = wealth_gmvp_net,
  Wealth_GMVP_Gross = wealth_gmvp_gross,
  Wealth_MV_Net = wealth_mv_net,
  Wealth_MV_Gross = wealth_mv_gross,
  Wealth_sp500= wealth_sp500
)

for (year in 1:length(results_mean_list)) {
  # Returns with transaction costs
  annual_return_gmvp_net <- results_mean_list[[year]]$annual_return_gmvp
  annual_return_mv_net <- results_mean_list[[year]]$annual_return_mv
  
  # Returns without transaction costs (gross)
  annual_return_gmvp_gross <- results_mean_list[[year]]$annual_return_gmvp_gross
  annual_return_mv_gross <- results_mean_list[[year]]$annual_return_mv_gross
  
  # Wealth with transaction costs
  wealth_gmvp_net <- wealth_gmvp_net * (1 + annual_return_gmvp_net / 100)
  wealth_mv_net <- wealth_mv_net * (1 + annual_return_mv_net / 100)
  
  # Wealth without transaction costs
  wealth_gmvp_gross <- wealth_gmvp_gross * (1 + annual_return_gmvp_gross / 100)
  wealth_mv_gross <- wealth_mv_gross * (1 + annual_return_mv_gross / 100)
  
  # Wealth with investing in SP500
  wealth_sp500 <- sp500_monthly$compound_growth[61+year*periods_per_year]/sp500_monthly$compound_growth[61]
  
  wealth_data <- rbind(wealth_data, data.frame(
    Year = year,
    Wealth_sp500 = wealth_sp500,
    Wealth_GMVP_Net = wealth_gmvp_net,
    Wealth_GMVP_Gross = wealth_gmvp_gross,
    Wealth_MV_Net = wealth_mv_net,
    Wealth_MV_Gross = wealth_mv_gross
  ))
}
# ============================================================
# Prepare data and create visualization
# ============================================================
# Convert to long format
library(lubridate)
start_date <- ymd("2004-11-01")  # novembre 2004

wealth_long <- wealth_data %>%
  mutate(
    Date = start_date + years(Year)  # 0 → 2004-11-01, 1 → 2005-11-01, etc.
  ) %>%
  pivot_longer(
    cols = c("Wealth_GMVP_Gross", "Wealth_GMVP_Net", "Wealth_MV_Gross", "Wealth_MV_Net", "Wealth_sp500"),
    names_to = "Portfolio",
    values_to = "Wealth"
  ) %>%
  mutate(
    Type = case_when(
      grepl("_Net", Portfolio) ~ "Net",
      grepl("_Gross", Portfolio) ~ "Gross",
      grepl("sp500", Portfolio, ignore.case = TRUE) ~ "Benchmark"
    ),
    Group = case_when(
      grepl("GMVP", Portfolio) ~ "GMVP",
      grepl("MV", Portfolio) ~ "MV",
      grepl("sp500", Portfolio, ignore.case = TRUE) ~ "SP500"
    )
  )

gmvp_colors <- c("Gross" = "red", "Net" = "deeppink")
mv_colors   <- c("Gross" = "blue", "Net" = "purple")

# GMVP : rouge et rose
ggplot(subset(wealth_long, Group == "GMVP"),
       aes(x = Date, y = Wealth, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = gmvp_colors,
                     labels = c("Gross" = "GMVP gross", "Net" = "GMVP net")) +
  scale_linetype_manual(values = c("Gross" = "solid", "Net" = "dashed")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Wealth GMVP: Gross vs Net",
    x = "Year (November of each year)",
    y = "Wealth (initial = 1)",
    color = "Type",
    linetype = "Type"
  ) +
  theme_minimal()

# MV : bleu et violet
ggplot(subset(wealth_long, Group == "MV"),
       aes(x = Date, y = Wealth, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = mv_colors,
                     labels = c("Gross" = "MV gross", "Net" = "MV net")) +
  scale_linetype_manual(values = c("Gross" = "solid", "Net" = "dashed")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Wealth MV: Gross vs Net",
    x = "Year (November of each year)",
    y = "Wealth (initial = 1)",
    color = "Type",
    linetype = "Type"
  ) +
  theme_minimal()
# ============================================================
# Combined plot: GMVP, MV, and S&P 500 on same graph NOMINAL VALUE
# ============================================================
wealth_long <- wealth_data %>%
  mutate(
    Date = start_date + lubridate::years(Year)
  ) %>%
  pivot_longer(
    cols = c("Wealth_GMVP_Gross", "Wealth_GMVP_Net", "Wealth_MV_Gross", "Wealth_MV_Net", "Wealth_sp500"),
    names_to = "Portfolio",
    values_to = "Wealth"
  ) %>%
  mutate(
    Type = case_when(
      grepl("_Net", Portfolio) ~ "Net",
      grepl("_Gross", Portfolio) ~ "Gross",
      grepl("sp500", Portfolio, ignore.case = TRUE) ~ "Benchmark"
    ),
    Group = case_when(
      grepl("GMVP", Portfolio) ~ "GMVP",
      grepl("MV", Portfolio) ~ "MV",
      grepl("sp500", Portfolio, ignore.case = TRUE) ~ "SP500"
    )
  )

# Rename for display
wealth_long$Portfolio_Display <- with(wealth_long, paste(Group, Type, sep = "_"))

# Linetype mapping
wealth_linetypes <- c("Gross" = "solid", "Net" = "dashed", "Benchmark" = "dotted")

# Color mapping
wealth_colors <- c(
  "GMVP_Gross" = "red", 
  "GMVP_Net" = "deeppink", 
  "MV_Gross" = "blue", 
  "MV_Net" = "purple",
  "SP500_Benchmark" = "darkgreen"
)

# Combined plot avec dates réelles
ggplot(wealth_long, aes(x = Date, y = Wealth, color = Portfolio_Display, linetype = Type)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = wealth_colors,
    labels = c(
      "GMVP_Gross" = "GMVP gross",
      "GMVP_Net" = "GMVP net",
      "MV_Gross" = "MV gross",
      "MV_Net" = "MV net",
      "SP500_Benchmark" = "S&P 500"
    )
  ) +
  scale_linetype_manual(
    values = wealth_linetypes, 
    labels = c("Gross" = "Gross", "Net" = "Net", "Benchmark" = "Benchmark")
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y-%m",
    limits = c(as.Date("2004-11-01"), as.Date("2025-11-01"))
  ) +
  labs(
    title = "Wealth Evolution: Gross vs Net (GMVP, MV, and S&P 500)",
    x = "Date (November of each year)",
    y = "Wealth (initial = 1)",
    color = "Portfolio",
    linetype = "Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================
# Now we take into account inflation REALVALUE
# ============================================================

getSymbols("CPIAUCSL", src = "FRED", from = "2004-11-01", to = "2024-11-01", auto.assign = TRUE)
cpi_data <- as.data.frame(CPIAUCSL)
cpi_data$Date <- rownames(cpi_data)
colnames(cpi_data) <- c("CPI", "Date")

# Initialize wealth (starting with 1 unit)
wealth_gmvp_net <- 1
wealth_gmvp_gross <- 1
wealth_mv_net <- 1
wealth_mv_gross <- 1
wealth_sp500 <- 1

# Initialize dataframe avec TOUTES les colonnes
wealth_data <- data.frame(
  Year = 0,
  Wealth_GMVP_Net = wealth_gmvp_net,
  Wealth_GMVP_Gross = wealth_gmvp_gross,
  Wealth_MV_Net = wealth_mv_net,
  Wealth_MV_Gross = wealth_mv_gross,
  Wealth_sp500 = wealth_sp500,
  Wealth_MV_Net_Real = wealth_mv_net,
  Wealth_GMVP_Net_Real = wealth_gmvp_net,
  Wealth_MV_Gross_Real = wealth_mv_gross,
  Wealth_GMVP_Gross_Real = wealth_gmvp_gross,
  Wealth_sp500_Real = wealth_sp500
)

for (year in 1:(length(results_mean_list) - 1)) {
  # Returns with transaction costs
  annual_return_gmvp_net <- results_mean_list[[year]]$annual_return_gmvp
  annual_return_mv_net <- results_mean_list[[year]]$annual_return_mv
  
  # Returns without transaction costs (gross)
  annual_return_gmvp_gross <- results_mean_list[[year]]$annual_return_gmvp_gross
  annual_return_mv_gross <- results_mean_list[[year]]$annual_return_mv_gross
  
  # Wealth with transaction costs
  wealth_gmvp_net <- wealth_gmvp_net * (1 + annual_return_gmvp_net / 100)
  wealth_mv_net <- wealth_mv_net * (1 + annual_return_mv_net / 100)
  
  # Wealth without transaction costs
  wealth_gmvp_gross <- wealth_gmvp_gross * (1 + annual_return_gmvp_gross / 100)
  wealth_mv_gross <- wealth_mv_gross * (1 + annual_return_mv_gross / 100)
  
  # Wealth with investing in SP500
  wealth_sp500 <- sp500_monthly$compound_growth[61 + year * periods_per_year] / sp500_monthly$compound_growth[61]
  
  # Taking into account inflation and CPI
  cpi_indicator <- cpi_data$CPI[(year-1) * periods_per_year + 1] / cpi_data$CPI[1]
  
  # Considering real wealth value with transaction costs
  real_wealth_mv_net <- wealth_mv_net / cpi_indicator
  real_wealth_gmvp_net <- wealth_gmvp_net / cpi_indicator
  
  # Considering real wealth value without transaction costs
  real_wealth_mv_gross <- wealth_mv_gross / cpi_indicator
  real_wealth_gmvp_gross <- wealth_gmvp_gross / cpi_indicator
  real_wealth_sp500 <- wealth_sp500 / cpi_indicator
    
  wealth_data <- rbind(wealth_data, data.frame(
    Year = year,
    Wealth_sp500 = wealth_sp500,
    Wealth_GMVP_Net = wealth_gmvp_net,
    Wealth_GMVP_Gross = wealth_gmvp_gross,
    Wealth_MV_Net = wealth_mv_net,
    Wealth_MV_Gross = wealth_mv_gross,
    Wealth_MV_Net_Real = real_wealth_mv_net,
    Wealth_GMVP_Net_Real = real_wealth_gmvp_net,
    Wealth_MV_Gross_Real = real_wealth_mv_gross,
    Wealth_GMVP_Gross_Real = real_wealth_gmvp_gross,
    Wealth_sp500_Real = real_wealth_sp500
  ))
}

# ============================================================
# REAL VALUES (Inflation-Adjusted)
# ============================================================

# Plot GMVP: red and pink (REAL)
ggplot(subset(wealth_long_real, Group == "GMVP"), aes(x = Year, y = Wealth, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = gmvp_colors, labels = c("Gross" = "GMVP gross", "Net" = "GMVP net")) +
  scale_linetype_manual(values = c("Gross" = "solid", "Net" = "dashed")) +
  labs(title = "Wealth GMVP: Gross vs Net (Real - Inflation-Adjusted)", x = "Year", y = "Wealth (2004 dollars, initial = 1)", color = "Type", linetype = "Type") +
  theme_minimal()

# Plot MV: blue and purple (REAL)
ggplot(subset(wealth_long_real, Group == "MV"), aes(x = Year, y = Wealth, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = mv_colors, labels = c("Gross" = "MV gross", "Net" = "MV net")) +
  scale_linetype_manual(values = c("Gross" = "solid", "Net" = "dashed")) +
  labs(title = "Wealth MV: Gross vs Net (Real - Inflation-Adjusted)", x = "Year", y = "Wealth (2004 dollars, initial = 1)", color = "Type", linetype = "Type") +
  theme_minimal()

# ============================================================
# REAL VALUES - Combined plot (Inflation-Adjusted)
# ============================================================

wealth_long_real_with_sp500 <- wealth_data %>%
  mutate(
    Date = start_date + lubridate::years(Year)
  ) %>%
  pivot_longer(cols = c("Wealth_GMVP_Gross_Real", "Wealth_GMVP_Net_Real", "Wealth_MV_Gross_Real", "Wealth_MV_Net_Real", "Wealth_sp500_Real"),
               names_to = "Portfolio",
               values_to = "Wealth") %>%
  mutate(
    Portfolio = gsub("_Real", "", Portfolio),
    Type = case_when(
      grepl("_Net", Portfolio) ~ "Net",
      grepl("_Gross", Portfolio) ~ "Gross",
      grepl("sp500", Portfolio, ignore.case = TRUE) ~ "Benchmark"
    ),
    Group = case_when(
      grepl("GMVP", Portfolio) ~ "GMVP",
      grepl("MV", Portfolio) ~ "MV",
      grepl("sp500", Portfolio, ignore.case = TRUE) ~ "SP500"
    )
  )

# Rename for display
wealth_long_real_with_sp500$Portfolio_Display <- with(wealth_long_real_with_sp500, paste(Group, Type, sep = "_"))

# Color mapping (avec SP500)
wealth_colors_real <- c(
  "GMVP_Gross" = "red", 
  "GMVP_Net" = "deeppink", 
  "MV_Gross" = "blue", 
  "MV_Net" = "purple",
  "SP500_Benchmark" = "darkgreen"
)

# Linetype mapping
wealth_linetypes_real <- c("Gross" = "solid", "Net" = "dashed", "Benchmark" = "dotted")

# Combined plot - REAL avec S&P 500
ggplot(wealth_long_real_with_sp500, aes(x = Date, y = Wealth, color = Portfolio_Display, linetype = Type)) +
  geom_line(size = 1.2) + scale_color_manual(
    values = wealth_colors_real,
    labels = c(
      "GMVP_Gross" = "GMVP gross",
      "GMVP_Net" = "GMVP net",
      "MV_Gross" = "MV gross",
      "MV_Net" = "MV net",
      "SP500_Benchmark" = "S&P 500"
    )
  ) +
  scale_linetype_manual(
    values = wealth_linetypes_real,
    labels = c("Gross" = "Gross", "Net" = "Net", "Benchmark" = "Benchmark")
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y-%m",
    limits = c(as.Date("2004-11-01"), as.Date("2025-11-01"))
  ) +
  labs(
    title = "Wealth Evolution: Gross vs Net (GMVP, MV, and S&P 500) - Real (Inflation-Adjusted)",
    x = "Date (November of each year)",
    y = "Wealth (2004 dollars, initial = 1)",
    color = "Portfolio",
    linetype = "Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

