# Fixed Income Securities - Yield Curve Analysis with PCA

*Problem type:* Dimensionality reduction and risk management using Principal Component Analysis (PCA) on yield curves.


## General Context
This project investigates the dynamics of the term structure of interest rates by applying Principal Component Analysis (PCA) to spot rate data. PCA is widely used in fixed income to capture the main sources of yield curve variation, often summarized as level, slope, and curvature. The study explores the correlation structure of the yield curve, identifies the dominant components, and interprets their economic meaning. Finally, the project applies PCA-based factors to simulate a hedging strategy for bond portfolios against yield curve shifts.


## Project Overview
1. Analyze the correlation structure of the spot rate matrix.
2. Perform PCA on the covariance matrix of spot rates and report variance explained by each component.
3. Identify the dominant factors and interpret their meaning in the context of yield curve movements.
4. Plot factor loadings and analyze the three most significant principal components.
5. Track the evolution of the three main factors over time, interpreting them as level, slope, and curvature.
6. Simulate bond portfolio hedging against yield curve shifts using PCA-derived factors.
