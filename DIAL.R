# ========================================================
# STEP 1: INSTALL AND LOAD PACKAGES
# ========================================================
# If you haven't installed these yet, run the install lines once:
if(!require(quantmod)) install.packages("quantmod")
if(!require(tseries)) install.packages("tseries")
if(!require(forecast)) install.packages("forecast")

library(quantmod) # For downloading stock data
library(tseries)  # For ADF Stationarity Test
library(forecast) # For ARIMA modeling

# ========================================================
# STEP 2: DOWNLOAD DATA (2015 - 2025)
# ========================================================
# We fetch Unilever (UL) directly from Yahoo Finance
# This ensures the dates are formatted perfectly
getSymbols("DIAL", src = "yahoo", from = "2023-10-01", to ="2025-09-30")
head(DIAL)
# Select the 'Adjusted Close' column (Column 6)
# We use Adjusted Close to account for dividends
stock_data <- DIAL[,6]
head(stock_data)
# Check if data loaded correctly
head(stock_data)

plot(stock_data, main="DIALOG Daily Stock Price (2023-2025)", 
     ylab="Price (USD)", col="darkblue")

par(mfrow=c(2,1))
acf(stock_data,
    lag.max = 100,
    main = "ACF of DIALOG Daily Stock Price (2023–2025)")
acf(stock_data, main="ACF plot of DIALOG Daily Stock Price (2023-2025)")
#not stationary
pacf(stock_data,main="PACF plot of DIALOG Daily Stock Price (2023-2025)")

# ========================================================
# STEP 3: STATIONARITY TEST (KPSS TEST)
# ========================================================
print("--- KPSS TEST RESULTS (RAW DATA) ---")
# Null Hypothesis: Data is Stationary
kpss.test(stock_data)
# If p-value < 0.05, we reject H0
adf.test(stock_data,alternative = "stationary")


stock_diff1 <- diff(stock_data)
stock_diff1 <- na.omit(stock_diff1) # Remove the first NA value created by differencing

plot(stock_diff1, main="Dialog Returns (Differenced Data)", 
     ylab="Change in Price", col="darkgreen",lwd=1.5)

par(mfrow=c(2,1))
acf(stock_diff1, main="ACF plot of Dialog Returns (Differenced Data)")
pacf(stock_diff1, main="PACF plot of Dialog Returns (Differenced Data)")


print("--- KPSS TEST RESULTS (DIFFERENCED DATA) ---")
kpss.test(stock_diff1)

adf.test(na.omit(stock_diff1), alternative = "stationary")

#p-value = 0.1 > 0.05----we cannot reject Ho----stationary

checkresiduals(stock_diff1)

#####------Models------#####

Model1<-arima(stock_diff1, order = c(0,1,0))
Model2<-arima(stock_diff1, order = c(1,1,0))
Model3<-arima(stock_diff1, order = c(0,1,1))
Model4<-arima(stock_diff1, order = c(1,1,1))
Model5<-arima(stock_diff1, order = c(5,1,0))
Model6<-auto.arima(stock_data)
fit <- auto.arima(stock_data)
accuracy(fit)
resid<- fit$residuals
plot(resid,ylab="residuals",col="darkblue")

checkresiduals(fit)

######Compare the models###############

# Model names
model_names <- c("Model1","Model2","Model3","Model4","Model5","Model6")

# Extract AIC and BIC values
aic_vals <- AIC(Model1,Model2,Model3,Model4,Model5,Model6)$AIC
bic_vals <- BIC(Model1,Model2,Model3,Model4,Model5,Model6)$BIC

# Create final data frame
Model_compare <- data.frame(
  Model = model_names,
  AIC_Value = aic_vals,
  BIC_Value = bic_vals
)

Model_compare

##############Select the best model######################

Model_compare$Model[which.min(Model_compare$AIC_Value)]
Model_compare$Model[which.min(Model_compare$BIC_Value)]

#For forecasting (prediction) → AIC is preferred
#For model explanation / simplicity → BIC is preferred

#So we get best model as Model6

Best_model<-Model6
Best_model

##############---------FORECASTING-------#########################

forecast_Model6 <- forecast(Model6, h = 30)
forecast_Model6

plot(forecast_Model6)


####Residuals are follow WhiteNoice##########

forecast_Model6$fitted
plot(forecast_Model6$fitted)

checkresiduals(forecast_Model6$residuals)


####### Forecasted values ##########

forecast_Model6 <- forecast(Model6, h = 30)
par(mfrow=c(2,1))

plot(forecast_Model6)

plot(forecast_Model6,
     include = 100,   # shows only last 100 observations
     main = " Forecast for Next 30 Days")


forecast_Model6$mean
as.numeric(forecast_Model6$mean)


data.frame(
  Day = 1:30,
  Forecast = forecast_Model6$mean,
  Lo_80 = forecast_Model6$lower[,1],
  Hi_80 = forecast_Model6$upper[,1],
  Lo_95 = forecast_Model6$lower[,2],
  Hi_95 = forecast_Model6$upper[,2]
)


data.frame(
  Day = 1:30,
  Forecast = forecast_Model6$mean,
  Lo_95 = forecast_Model6$lower[,2],
  Hi_95 = forecast_Model6$upper[,2]
)

### Model Adequacy #####

library(TSA)

# Ljung-Box test
Box.test(residuals(fit), lag = 20, type = "Ljung-Box")


#----------------------------------------------------
