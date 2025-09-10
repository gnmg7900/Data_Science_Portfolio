# Yield Curve Modeling - Computational Finance

*Problem type:* Time series modeling and curve fitting for bond yields using the Nelson-Siegel-Svensson (NSS) model.


## General Context
This project explores fixed income analytics by modeling and analyzing government bond yield curves. 

Using historical bond yield data, the objective is to fit the Nelson-Siegel-Svensson (NSS) model, extract parameters, and evaluate curve dynamics over time. In addition, the project implements key financial functions such as day count conventions and present value (PV) calculations, providing a complete framework for yield curve modeling and valuation in computational finance.

## Project Overview
1. Construct a pandas DataFrame with bond yield data, using dates as columns and year fractions of tenors as index.
2. Plot yield curves for each date and estimate NSS parameters.
3. Fit and visualize NSS yield curves for each input date.
4. Analyze tenor-specific evolution (9M, 3.5Y, 12.5Y, 25Y) over time.
5. Compare market data points against NSS fits using multiple subplots.
6. Implement a day count function (Act/360, Act/365) to compute year fractions between two dates.
7. Develop a present value function to calculate discounted cash flows at a given rate and maturity, with custom string output.
