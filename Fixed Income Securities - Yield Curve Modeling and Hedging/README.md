# Fixed Income Securities - Yield Curve Modeling and Hedging

*Problem type:* Term structure estimation, bond pricing, and portfolio hedging using yield curve models.


## General Context
This project explores fixed income markets through yield curve construction, bond pricing, and risk management. Using government bond data, the study applies bootstrapping techniques to estimate the spot rate curve, analyzes yield volatilities, and prices corporate bonds under credit spreads. In addition, the project implements the Nelson-Siegel and Nelson-Siegel-Svensson (NSS) models to fit yield curves and evaluate their dynamics. The second part focuses on hedging strategies, where a bond portfolio is immunized against yield curve shifts by computing level, slope, and curvature durations and constructing a self-financing hedging portfolio.


## Project Overview
1. Yield Curve Construction
* Bootstrap the spot rate curve from government bond data with semi-annual coupons.
* Estimate yield volatilities across maturities and analyze their implications.
* Price a 5-year corporate bond under a credit spread assumption, computing fair value and risk measures.
2. Nelson-Siegel Modeling
* Fit the Nelson-Siegel (NS) model to market yield data, estimate parameters, and compute mean squared error (MSE).
* Visualize yield curve dynamics over time and analyze parameter evolution.
3. Portfolio Hedging
* Compute level, slope, curvature, and total durations (Δdurations) of the target bond portfolio.
* Calculate corresponding sensitivities for candidate hedging assets.
* Construct a hedging portfolio under a self-financing constraint to immunize against yield curve shifts.
* Assess the performance of the hedging strategy before and after a yield curve shock, using updated NSS parameters.
