# Insurance Analytics - Claims Modeling and Pricing

*Problem type:* Statistical modeling and machine learning for claim frequency, claim severity, and insurance pricing.


## General Context
This project applies data science techniques to automobile insurance data, with the dual objectives of analyzing claim behavior and constructing pricing models. The analysis begins with exploratory data analysis and distribution fitting for claim counts and claim severity, identifying statistical patterns and risk factors. Building on these insights, the project develops models for claim frequency and severity using Generalized Linear Models (GLMs) and integrates machine learning methods to address large claims. The results are combined into a pricing structure proposal for the insurance portfolio, highlighting both technical rigor and practical business application.


## Project Overview

### Part I – Claims Data Analysis
1. Exploratory Data Analysis (EDA) of claim counts and severity, highlighting trends, distributions, and outliers.
2. Statistical analysis of claim severity using descriptive metrics and distribution fitting.
3. Graphical analysis of variable interactions (scatter plots, boxplots, heatmaps) to explore claim drivers.
4. Fit probability distributions for claims, applying thresholds for outlier removal and exponential family fitting.

### Part II – Pricing Structure Development
1. Model claim frequency with GLMs, estimating claim probabilities for different insured profiles.
2. Model claim severity for “common” claims with GLMs, refining assumptions and identifying high- and low-risk groups.
3. Propose a pricing structure for common claims, linking profiles to corresponding premium levels.
4. Extend the analysis to large claims using machine learning models (e.g., logistic regression, decision trees, random forests, gradient boosting, neural networks).
5. Critically evaluate model performance and propose how outputs from large claim models can be integrated into the pricing structure.
