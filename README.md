# Machine Learning–ARIMA Framework for Stock Selection and Forecasting in the SL20 Index

### Machine Learning | Financial Analytics | Time Series Forecasting

## Project Overview

This project develops an integrated Machine Learning–ARIMA framework for stock selection and price forecasting in the Colombo Stock Exchange (CSE), focusing on ten selected companies from the S&P SL20 Index.

The study combines fundamental and technical indicators to evaluate stock performance using machine learning classification, regression, and ranking models. The highest-ranked stock is then analyzed using ARIMA time series forecasting to predict its short-term price movements.

## Research Objectives

- Identify the best-performing stock among selected SL20 companies using machine learning.
- Compare fundamental, technical, and combined feature sets.
- Evaluate classification, regression, and ranking models.
- Forecast the selected stock's future prices using an ARIMA model.

## Methodology

**Stage 1: Machine Learning-Based Stock Selection**

- Data collection and preprocessing
- Fundamental and technical feature analysis
- Classification and regression modeling
- Stock ranking using XGBoost and LightGBM
- Model performance evaluation and comparison

**Stage 2: Time Series Forecasting**

- Historical stock price analysis
- Stationarity testing and differencing
- ACF and PACF analysis
- ARIMA model identification and evaluation
- 30-day stock price forecasting

## Key Findings

- Ranking models demonstrated strong performance in stock selection.
- Combining fundamental and technical indicators improved overall model performance.
- XGBoost Ranker achieved a Rank Information Coefficient of 0.7936 in the fundamental analysis.
- Dialog Axiata PLC (DIAL) was identified as the highest-ranked stock.
- ARIMA(3,1,1) was selected for forecasting DIAL's stock prices.
- The 30-day forecast indicated an upward trend with forecast uncertainty.

## Tools and Technologies

- Python – Machine learning and financial data analysis
- R Programming – Time series analysis and ARIMA forecasting
- Machine Learning – Random Forest, XGBoost, LightGBM, and Support Vector Methods
- Statistical Modeling – Model evaluation, stationarity testing, and forecasting
- Financial Analytics – Fundamental and technical indicator analysis

## Data Sources

- Colombo Stock Exchange (CSE)
- TradingView

## Academic Information

**Author:** B. M. Sachini Chamodya

**Degree:** BSc in Financial Mathematics and Industrial Statistics

**University:** University of Ruhuna, Sri Lanka

**Course:** MFM3151 – Case Study II

**Supervisor:** Dr. L. T. Wedage

**Year:** 2026

---

*An academic research project demonstrating applied machine learning, financial data analytics, stock ranking, and time series forecasting.*

**Disclaimer:** This project is for academic and educational purposes and does not constitute investment advice.
