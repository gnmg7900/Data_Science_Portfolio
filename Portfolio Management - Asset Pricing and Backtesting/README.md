# Portfolio Management - Asset Pricing and Backtesting

*Problem type:* Empirical investigation of financial return properties and backtesting portfolio management strategies using historical market data.

## General Context
This project focuses on asset pricing and portfolio management through the empirical analysis of financial market returns and the evaluation of multiple investment strategies. Using daily, weekly, and monthly log-return data for a diversified portfolio of listed securities (stocks, ETFs, indexes, and cryptoassets), the project investigates key stylized facts of financial returns such as volatility clustering, leverage effects, fat tails, and conditional non-normality.
The study further implements a rolling-window backtesting framework to assess portfolio allocation strategies across randomized subsets of assets and time periods. By generating multiple datasets, the robustness of each strategy is analyzed under different market conditions.

## Project Overview
1. Collect and preprocess historical market data (2010–present) for a diversified portfolio of securities.
2. Compute log-returns at different frequencies (daily, weekly, monthly) and empirically analyze financial stylized facts:
* Absence of autocorrelation
* Fat tails and non-normal distribution of returns
* Asymmetric or negatively skewed returns
* Volatility clustering
* Leverage effects
* Conditional non-normality
4. Implement a rolling-window backtesting approach with 100 randomized 2-year datasets.
5. Evaluate the performance of multiple portfolio allocation strategies, including:
* Equally Weighted Portfolio (EWP)
* Markowitz’s Mean-Variance Portfolio (MVP)
* Global Minimum Variance Portfolio (GMVP)
* Maximum Sharpe Ratio Portfolio (MSRP)
* Inverse Volatility Portfolio (IVP)
* Risk Parity Portfolio (RPP)
* Most Diversified Portfolio (MDP)
* Maximum Decorrelation Portfolio (MDC)
6. Compare strategies using performance metrics such as return, volatility, Sharpe ratio, and maximum drawdown.
