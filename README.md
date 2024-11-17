# FTSE 100 Stock Analysis with ARMA Models

## Project Overview

This project analyzes time-series stock return behavior for selected FTSE 100 constituents, focusing on three sectors:

1. **Food & Drug Retailers**
2. **Real Estate Investment Trusts (REITs)**
3. **Mining**

Using data sourced from the Bloomberg terminal, the analysis applies **ARMA (Autoregressive Moving Average) models** to examine stationarity, evaluate sector-specific dynamics, and assess the predictive power of past returns and shocks.

The findings reveal unique patterns in volatility, sensitivity to external shocks, and model fit quality across the analyzed sectors.

## Data Source

The dataset includes daily closing prices of FTSE 100 stocks from **January 2, 2019** to **March 21, 2024**, retrieved via the Bloomberg Data History (BDH) function. Stocks are grouped into sectors:

- **Food & Drug Retailers**: Ocado (OCDO), Marks & Spencer (MKS), Sainsbury’s (SBRY), Tesco (TSCO)
- **Mining**: Anglo American (AAL), Antofagasta (ANTO), Fresnillo (FRES), Glencore (GLEN), Rio Tinto (RIO)
- **REITs**: Land Securities (LAND), Segro (SGRO), Unite Group (UTG)

## Methodology

### 1. Dataset Transformation
- **Logarithmic transformation** stabilizes variance, a key assumption of ARMA models.
- Differencing ensures **stationarity**, confirmed using the **Augmented Dickey-Fuller test**.

### 2. ARMA Modeling
ARMA models combine:
- **Autoregressive (AR)** terms to incorporate past returns.
- **Moving Average (MA)** terms to model the impact of shocks.

The models are selected based on the **Akaike Information Criterion (AIC)** and validated through:
- Residual diagnostics using the **Ljung-Box test** for white noise.
- **Inverse roots** analysis to confirm model stationarity.

### 3. Statistical Evaluation
Model performance is assessed using:
- **Mean Squared Error (MSE)** and **Mean Absolute Error (MAE)** for residuals.
- **Autocorrelation (ACF)** and **Partial Autocorrelation (PACF)** plots for identifying patterns.

## Key Findings

### Food & Drug Retailers
- **High volatility** with significant AR and MA coefficients.
- **Ocado** (ARMA(3,2)) demonstrated strong reversion tendencies and the lasting influence of shocks.
- **Tesco** (ARMA(0,0)) showed minimal predictive patterns, indicative of random walk behavior.

### Mining
- **Most volatile sector** with higher-order ARMA models reflecting complex dynamics.
- **Anglo American** (ARMA(2,2)) and **Glencore** (ARMA(2,3)) exhibited significant negative AR coefficients (reversion) and positive MA coefficients (shock sensitivity).

### REITs
- **Cyclic trends** with lower volatility compared to other sectors.
- **Land Securities** (ARMA(3,2)) showed mixed responses to past returns and shocks.
- **Segro** (ARMA(0,0)) had no significant predictive patterns.

### Across Sectors
- Positive MA coefficients in all sectors highlight the persistent influence of external shocks.
- The REIT sector exhibited the **lowest MSE**, suggesting ARMA models better captured cyclical trends compared to the volatile behavior of Mining and Food & Retail sectors.
