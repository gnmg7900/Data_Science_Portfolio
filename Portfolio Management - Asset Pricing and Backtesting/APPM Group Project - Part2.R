# -------------------------------------------------------------------------------------------
# Importing Packages
# -------------------------------------------------------------------------------------------

rm(list=ls(all.names = TRUE))
graphics.off()
close.screen(all.screens = TRUE)
erase.screen()
windows.options(record=TRUE)
setwd('C:/Users/User/Desktop/Nova IMS/Asset Pricing and Portfolio Management/Group Project')

library(pacman)
p_load(portfolioBacktest)     # portfolio Backtesting
p_load(quantmod)              # to download stock data
p_load(PerformanceAnalytics)  # to compute performance measures
p_load(CVXR)                  # Convex Optimization in R
p_load(DT)
p_load(xts)                   # to manipulate time series of stock data
p_load(riskParityPortfolio)   # RPP
p_load(ggplot2)               # plot


# -------------------------------------------------------------------------------------------
# Loading Data
# -------------------------------------------------------------------------------------------

# Define the list of custom tickers
custom_tickers <- c("AAPL", "MSFT", "TSLA", "NVDA", "QQQ", "GBTC", "BNO", "GLD")

# Download data from the internet for the custom tickers
stock_data_custom <- stockDataDownload(
  stock_symbols = custom_tickers,
  index_symbol = "SPY",
  from = "2010-01-01",
  to = "2024-09-27"
)

# Save the downloaded data to a file
save(stock_data_custom, file = "custom_stockdata_2010_to_2024.Rdata")

# Load the data from the saved file
load("custom_stockdata_2010_to_2024.Rdata")

# Resample the data 100 times with 2-year rolling windows
set.seed(123)  # Set seed for reproducibility

# Using financialDataResample to create the dataset list
my_custom_dataset_list <- financialDataResample(
  stock_data_custom,       # Custom dataset
  N_sample = 5,            # Number of instruments in each resample
  T_sample = 252*2,        # Length of each resample (2 years)
  num_datasets = 100       # Number of resampled datasets
)

# Check for NA values
sapply(my_custom_dataset_list, function(dataset) {
  sum(is.na(dataset$adjusted))
})

# Check the structure of one of the resampled datasets
head(my_custom_dataset_list$`dataset 1`$adjusted, 3)
tail(my_custom_dataset_list$`dataset 1`$adjusted, 3)

# -------------------------------------------------------------------------------------------
# Portfolio definitions
# -------------------------------------------------------------------------------------------

# 1.) Equally Weighted Portfolio

Equally_weighted_portfolio_fun <- function(dataset, ...) {
  # Computing the number of assets in the dataset
  num_assets <- ncol(dataset$adjusted)
  # Creating an equally weighted vector
  weights <- rep(1 / num_assets, num_assets)
  return(as.vector(weights))
}

# 2.) Markowitz mean-variance portfolio

Markowitz_portfolio_fun <- function(dataset, lambda=0.5, ...) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  mu    <- colMeans(X)  # compute mean vector
  Sigma <- cov(X)       # compute the SCM
  # design mean-variance portfolio
  w <- Variable(nrow(Sigma))
  prob <- Problem(Maximize(t(mu) %*% w - lambda*quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w)))
}

# 3.) GMVP (with heuristic not to allow short-selling)

GMVP_portfolio_fun <- function(dataset, ...) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- abs(w)/sum(abs(w))
  return(w)
}

# 4.) Maximum Sharpe Ratio Portfolio

MSRP_portfolio_fun <- function(dataset, risk_free_rate = 0, ...) {
  # Compute log returns
  X <- diff(log(dataset$adjusted))[-1]  # Compute log returns
  Sigma <- cov(X)  # Compute the covariance matrix
  mu <- colMeans(X) - risk_free_rate  # Compute mean excess return vector
  # Define the optimization problem to minimize variance subject to expected return constraint
  w_ <- Variable(nrow(Sigma))
  prob <- Problem(Minimize(quad_form(w_, Sigma)),
                  constraints = list(w_ >= 0, t(mu) %*% w_ == 1))
  # Solve the optimization problem
  result <- CVXR::solve(prob)
  # Check if the solution is successful
  if (result$status != "optimal") {
    warning("Optimization did not converge to an optimal solution. Returning equal weights.")
    # Return equal weights if the solution is not optimal
    return(rep(1 / nrow(Sigma), nrow(Sigma)))
  }
  # Extracting and normalize the weights
  w <- as.vector(result$getValue(w_) / sum(result$getValue(w_)))
  names(w) <- colnames(Sigma)
  return(w)
}

# 5.) Inverse Volatility Portfolio

IVP_portfolio_fun <- function(dataset, ...) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design IVP
  w <- riskParityPortfolio(Sigma, formulation='diag')$w
  return(w)
}

# 6.) Risk Parity Portfolio

RPP_portfolio_fun <- function(dataset, ...) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  Sigma <- cov(X) 
  # design RPP
  w <- riskParityPortfolio(Sigma)$w
  return(w)
}

# 7.) Most Diversified Portfolio

MSRP <- function(mu, Sigma) {
  w_ <- Variable(nrow(Sigma))
  prob <- Problem(Minimize(quad_form(w_, Sigma)),
                  constraints = list(w_ >= 0, t(mu) %*% w_ == 1))
  result <- CVXR::solve(prob)
  w <- as.vector(result$getValue(w_)/sum(result$getValue(w_)))
  names(w) <- colnames(Sigma)
  return(w)
}

MDP_portfolio_fun <- function(dataset, ...) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  Sigma <- cov(X)    
  mu = sqrt(diag(Sigma))
  w <- MSRP(mu = sqrt(diag(Sigma)), Sigma)
  return(w)
}

# 8.) Maximum Decorrelation Portfolio

MDC_portfolio_fun <- function(dataset, ...) {
  X <- diff(log(dataset$adjusted))[-1]  # Compute log returns
  sigma <- cov(X)  # Compute the covariance matrix
  corr_matrix <- cov2cor(sigma)  # Correlation matrix
  
  # Solve for minimum variance using correlation matrix
  w <- solve(corr_matrix, rep(1, nrow(corr_matrix)))
  w <- abs(w) / sum(abs(w))  # Normalize weights
  
  return(as.vector(w))
}

# -------------------------------------------------------------------------------------------
# Backtesting and Plotting
# -------------------------------------------------------------------------------------------

# We will use dataset 8 as an example in the report!!!

portfolios <- list("GMVP" = GMVP_portfolio_fun,
                   "Markowitz" = Markowitz_portfolio_fun,
                   "MSRP" = MSRP_portfolio_fun,
                   "IVP" = IVP_portfolio_fun,
                   "RPP" = RPP_portfolio_fun,
                   "MDP" = MDP_portfolio_fun,
                   "MDC" = MDC_portfolio_fun)
  
bt <- portfolioBacktest(portfolios, my_custom_dataset_list, 
                        benchmark = c("1/N", "index"),   # benchmark portfolios
                        rebalance_every = 21*3,
                        optimize_every = 21*3,
                        lookback = 252*0.5,         # Length of the lookback rolling window in periods
                        shortselling = F,
                        cost = list(buy = 0e-4, sell = 0e-4, short = 0e-4, long_leverage = 0e-4))

names(bt)

# Checking portfolio weights for the dataset 2 of the Global Minimum Variance portfolio as an example
bt$IVP$`dataset 2`$w_bop[1,]     

# -------------------------------------------------------------------------------------------
# # Selecting several performance measures for the set of portfolios
# -------------------------------------------------------------------------------------------

# Printing the selected performance criteria for each portfolio

#backtestSelector(bt, portfolio_name = "EWP", 
                 #measures = c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", "Sterling ratio", "Omega ratio"))

backtestSelector(bt, portfolio_name = "GMVP", 
                 measures = c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", "Sterling ratio", "Omega ratio"))

backtestSelector(bt, portfolio_name = "Markowitz", 
                 measures = c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", "Sterling ratio", "Omega ratio"))

backtestSelector(bt, portfolio_name = "MSRP", 
                 measures = c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", "Sterling ratio", "Omega ratio"))

backtestSelector(bt, portfolio_name = "IVP", 
                 measures = c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", "Sterling ratio", "Omega ratio"))

backtestSelector(bt, portfolio_name = "RPP", 
                 measures = c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", "Sterling ratio", "Omega ratio"))

backtestSelector(bt, portfolio_name = "MDP", 
                 measures = c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", "Sterling ratio", "Omega ratio"))

backtestSelector(bt, portfolio_name = "MDC", 
                 measures = c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", "Sterling ratio", "Omega ratio"))

# Tables of several performance measures of the portfolios (classified by performance criteria):

# Portfolio's performance in tables 
ppt <- backtestTable(bt, measures = c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", "Sterling ratio", "Omega ratio"))
ppt

# Summary of performance measures:
res_sum <- backtestSummary(bt, summary_fun = median, show_benchmark = TRUE)
names(res_sum)

res_sum$performance_summary

# -------------------------------------------------------------------------------------------
# Visualizing Results
# -------------------------------------------------------------------------------------------

# Performance table:
summaryTable(res_sum, type = "DT", 
             order_col = "Sharpe ratio",   # performance measure to be used to sort the rows
             order_dir = "desc")

# Barplot (summaryTable() information in a visual way):
summaryBarPlot(res_sum, measures = c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", "Sterling ratio", "Omega ratio"))

# BoxPlot:
backtestBoxPlot(bt, measure = "Sharpe ratio", type = c("ggplot2"))

# Cumulative return or wealth plot of a single backtest:

# Chart of the cumulative returns or wealth for a single backtest
backtestChartCumReturn(bt, c("MSRP", "GMVP", "1/N","Markowitz","index"),
                       dataset_num=8)

backtestChartDrawdown(bt, c("MSRP", "GMVP", "1/N","Markowitz","index"), 
                      type = c("ggplot2"),
                      dataset_num=8)





