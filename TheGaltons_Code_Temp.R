# Clean the environment ####
rm(list=ls())

### LOAD LIBRARIES ####
# Load necessary packages
library(readxl)
library(xts)
library(dplyr)  
library(ggplot2)
library(forecast) 
library(lubridate)
library(scales)
library(timeSeries)
library(zoo)
library(urca)
library(uroot)
library(readr)
library(writexl)

### Import and Check data ####

df_m <- read_excel("TheGaltons_Data.xlsx", sheet = "Temp")

head(df_m)
summary(df_m)

### CREATE DTG OBJECT IN POSIXCT FORMAT ####
df_m$DTG <- as.POSIXct(df_m$DTG, format = "%d/%m/%Y %H:%M", tz = "UTC")

# remove duplicate data due to Daylight Saving Time
df_m <- df_m[!duplicated(df_m$DTG), ]

# Create zoo time series object (for better visualization of the plot)
ts_zoo <-zoo(df_m$mean_temperature, order.by = df_m$DTG)

# Full plot of the time series (the plot is not clear)
plot(ts_zoo, ylab="Avg Temperature")

### TEMPORAL AGGREGATIONS FOR PLOT #####

# Monthly aggregation
data_monthly <- df_m %>% 
  mutate(YearMonth = format(DTG, "%Y-%m")) %>% 
  group_by(YearMonth) %>% 
  summarise(mean_temperature = mean(mean_temperature,na.rm = TRUE))

# Daily aggregation
data_daily <- df_m %>% 
  mutate(Day = format(DTG, "%Y-%m-%d")) %>% 
  group_by(Day) %>% 
  summarise(mean_temperature = mean(mean_temperature, na.rm = TRUE))

# Hourly aggregation (create Date column with hours)
data_hourly <- df_m %>% 
  mutate(Date = format(DTG, "%Y-%m-%d %H")) %>% 
  group_by(Date) %>% 
  summarise(mean_temperature = mean(mean_temperature,na.rm = TRUE))

# Convert column Date in POSIXct format
data_hourly <- data_hourly %>%
  mutate(
    Date = as.POSIXct(Date, format = "%Y-%m-%d %H"),
  )

### CREATE TIME SERIES WITH DIFFERENT FREQ  #####
# Conversion and Creation of Time Series with different frequency
data_monthly$YearMonth <- as.Date(paste0(data_monthly$YearMonth, "-01"), format="%Y-%m-%d")
ts_monthly <- ts(data_monthly$mean_temperature, start = c(2020, 1), frequency = 12)
ts_daily <- ts(data_daily$mean_temperature, start = c(2020, 1), frequency = 365)

# ts Hourly - Freq = 8759
ts_hourly <- ts(data_hourly$mean_temperature, start = c(2020, 1), frequency = 8759)
# ts Hourly - Freq = 24
ts_hourly_24 <-ts(data_hourly$mean_temperature, start = c(2020, 1), frequency = 24)
### CREATE VARIABLES FOR MODELLING ####

#Trend
data_hourly$Trend <- 1:nrow(data_hourly)

# Create column Month

data_hourly$Month <- as.numeric(as.character(format(data_hourly$Date, "%m")))
data_hourly$sin_month <- sin(2 * pi * data_hourly$Month / 24)
data_hourly$cos_month <- cos(2 * pi * data_hourly$Month / 24)
data_hourly$sin_month_2 <- sin(4 * pi * data_hourly$Month / 24)
data_hourly$cos_month_2 <- cos(4 * pi * data_hourly$Month / 24)
data_hourly$sin_month_4 <- sin(6 * pi * data_hourly$Month / 24)
data_hourly$cos_month_4 <- cos(6 * pi * data_hourly$Month / 24)
data_hourly$Month_D <- as.factor(as.character(format(data_hourly$Date, "%m")))

#Hour

data_hourly$Hour <- as.numeric(as.character(format(data_hourly$Date, "%H")))
data_hourly$sin_hour <- sin(2 * pi * data_hourly$Hour / 24)
data_hourly$cos_hour <- cos(2 * pi * data_hourly$Hour / 24)
data_hourly$sin_hour_2 <- sin(4 * pi * data_hourly$Hour / 24)
data_hourly$cos_hour_2 <- cos(4 * pi * data_hourly$Hour / 24)
data_hourly$Hour_D <- as.factor(as.character(format(data_hourly$Date, "%H")))

# ---  EDA #######

# 1) VISUALIZE DATA in different timeframe ####

## Basic time series plots
# Full dataset plot - Monthly
plot(ts_monthly, xlim = c(2020, 2025), main = "Monthly Data", ylab = "Temperature C°")
# Last year Daily plot
plot(tail(ts_daily, 365), main = "Last Year - Daily Data", ylab = "Temperature C°")
# Plot to show both Weekly and Daily seasonality
plot(tail(ts_hourly, 24*7), main = "Last 7 Days - Hourly Energy Demand",ylab = "Temperature C°")

# 2) VISUALIZE SEASONALITY ####
# MONTHLY  ####

## Monthly Seasonal plot
ggseasonplot(ts_monthly, main="Monthly Seasonal Pattern")

# HOURLY  ####

# Hourly seasonal plot (one year)
ts_hourly_plot <- ts(ts_hourly, frequency = 24)
last_weeks <- tail(ts_hourly_plot, 24*365)
last_weeks_ts <- ts(last_weeks, frequency = 24)
ggseasonplot(last_weeks_ts, main = "Daily Seasonal Pattern")+
  scale_color_discrete(name = "day")+
  theme(legend.position = "none")

# 3) VISUALIZE TIME SERIES DISTRIBUTION, ACF & PACF #### 

mean_ts <- mean(ts_hourly)
plot(ts_hourly)
abline( h= mean_ts ,col = "red", lwd = 2)
# time series seems to move around the mean over time

# the data seems to have approximately a normal distribution, since n is very large we can approximate to a Normal distribution
hist(ts_hourly, breaks = 30)

ts_hourly_plot_acf <- ts(ts_hourly, frequency = 1)
# ACF
acf_result <- acf(ts_hourly_plot_acf, lag.max=30)
# strong seasonality (daily)
#ACF
pacf(ts_hourly_plot_acf, lag.max=30)
#the pacf shows different significative lags after lag 2

# 4) TIME SERIES DECOMPOSITION ####

# STL Hourly (full dataset) decomposition
decomp <- stl(ts_hourly, s.window = "periodic")
plot(decomp)
title("STL Decomposition of Hourly Data")

# Daily moving averages
ma12 <- ma(ts_monthly, 12)
ma24 <- ma(ts_monthly, 24) # Twice the length of the seasonality (12*2)

# Plot original series with moving averages
par(mfrow=c(1,1))
plot(ts_monthly,
     main = "Daily Temperature with Moving Averages",
     ylab = "Temperature C°")
lines(ma12, col = "red", lwd = 2)
lines(ma24, col = "blue", lwd = 2)
legend("topright", 
       legend = c("Original", "MA(12)", "MA(24)"), 
       col = c("black", "red", "blue"), 
       lty = 1,
       cex = 0.6)


# --- TEST : STOCASTIC TREND & SEASONALITY ####

# 1) ADF TEST #####

# Augmented Dickey Fuller test 

# No intercept, no trend
summary(ur.df(ts_hourly, type = "none", lags = 1, selectlags = "BIC"))

# With intercept only
summary(ur.df(ts_hourly, type = "drift", lags = 1, selectlags = "BIC"))

# With intercept and linear trend
summary(ur.df(ts_hourly, type = "trend", lags = 1, selectlags = "BIC"))

# Test statistic shows : no unit root present 

# 2) CH TEST ####
# Perform CH test for seasonal variation
ch.test(ts_monthly)
# constant deterministic seasonal pattern

# --- MODEL ESTIMATION AND EVALUATION #####

# 1) TRAIN VALID & TEST SPLIT #####
train_start <-c(2020,1)
train_end <- c((3905-30), 19) 
valid_start<- c((3905-30),20)
valid_end <- c((3905-15), 19) 
train <- window(ts_hourly_24, start =train_start, end = train_end)
valid <- window(ts_hourly_24, start = valid_start, end= valid_end)
n.valid <- length(valid)
n.train<-length(train)
df_m_train <- head(data_hourly, (nrow(data_hourly) - 720))
df_m_valid <- head(tail(data_hourly,720),360)
valid_date <- df_m_valid$Date
train_full <- window(ts_hourly_24, start =train_start, end = valid_end)
df_train_full <- head(data_hourly, (n.train + n.valid))
df_test<-tail(data_hourly,72)
test_date<-df_test$Date
test_start<-c((3905-15), 20)
test_end<-c((3905-3), 19)
test<-window(ts_hourly_24, start=test_start, end=test_end)
n.test<-length(test)

# 2) FIT LINEAR MODEL ####
##### MODEL 1 - Fit the linear model with Trend


lm.1.1 <- lm(mean_temperature ~ Trend + sin_month_2 + cos_month_2 + sin_hour + cos_hour , data= df_m_train) 

lm.1.2 <- lm(mean_temperature ~ Trend + sin_month_2 + cos_month_2 + sin_hour + cos_hour + sin_hour*sin_month_2 + sin_hour*cos_month_2 +cos_hour*sin_month_2 + cos_hour*cos_month_2    , data= df_m_train) 

lm.1.3 <- lm(mean_temperature ~ Trend + Month_D + sin_hour + cos_hour + Month_D*sin_hour + Month_D*cos_hour   , data= df_m_train)

lm.1.4 <- lm(mean_temperature ~ Trend + Month_D + sin_hour + cos_hour , data= df_m_train) 

lm.1.5 <- lm(mean_temperature ~ Trend + sin_hour + cos_hour + Month_D*cos_hour, data= df_m_train) 

lm.1.6 <- lm(mean_temperature ~ Trend  + I(Trend^2)+ Month_D + sin_hour + cos_hour , data= df_m_train)

lm.1.7 <- lm(mean_temperature ~ Trend  + I(Trend^2)+ Month_D + sin_hour + cos_hour + Trend*sin_hour + Trend*cos_hour, data= df_m_train)

lm.1.8 <- lm(mean_temperature ~ Trend + Month_D + Hour_D , data= df_m_train) 

lm.1.9 <- lm(mean_temperature ~ Trend+ I(Trend^2) + Hour_D + Month_D  , data= df_m_train) 

lm.1.10 <- lm(mean_temperature ~ Trend + Hour_D + Month_D + Hour_D*Month_D , data= df_m_train) 

lm.1.11<- lm(mean_temperature ~ Trend + I(Trend^2) + Hour_D + Month_D + Hour_D*Month_D  , data= df_m_train) 

lm.1.12<- lm(mean_temperature ~ Trend + I(Trend^2) + Hour_D + Month_D + Hour_D*Month_D + Hour_D*Trend , data= df_m_train) 

k<-12
AIC <- numeric(k)
BIC <- numeric(k)

for (i in 1:k){
  model_name <- paste0("lm.1.", i)
  model_obj <- get(model_name)
  
  AIC[i] <- AIC(model_obj)
  BIC[i] <- BIC(model_obj)
}

AIC(lm.1.1,lm.1.2,lm.1.3,lm.1.4,lm.1.5,lm.1.6,lm.1.7,lm.1.8,lm.1.9,lm.1.10,lm.1.11,lm.1.12)
BIC(lm.1.1,lm.1.2,lm.1.3,lm.1.4,lm.1.5,lm.1.6,lm.1.7,lm.1.8,lm.1.9,lm.1.10,lm.1.11,lm.1.12)

order(AIC)[1:2]
order(BIC)[1:2]
summary(lm.1.9)

# 3) RESIDUALS ANALYSIS ####

reg_residuals1<-lm.1.3$residuals
plot(reg_residuals1)
# hist of residuals
hist(reg_residuals1, breaks = 30)
# acf of residuals
acf(reg_residuals1)
# pacf of residuals
pacf(reg_residuals1)

reg_residuals2<-lm.1.12$residuals
plot(reg_residuals2)
# hist of residuals
hist(reg_residuals2, breaks = 30)
# acf of residuals
acf(reg_residuals2)
# pacf of residuals
pacf(reg_residuals2)

# --- Q-Q plot 
qqnorm(scale(reg_residuals1), 
       main = "Q-Q plot",
       xlab = "Theoretical quantile",
       ylab = "Sample quantile")
qqline(scale(reg_residuals1), col = "red", lwd = 2)

qqnorm(scale(reg_residuals2), 
       main = "Q-Q plot",
       xlab = "Theoretical quantile",
       ylab = "Sample quantile")
qqline(scale(reg_residuals2), col = "red", lwd = 2)

# 4) FIT ARIMAX/SARIMAX MODEL ####
for (i in 1:2) {
  
  residual_name <- paste0("reg_residuals", i)
  reg_residuals <- get(residual_name)
  
  arimax_model_100 <- Arima(reg_residuals, order = c(1, 0, 0))
  arimax_model_101 <- Arima(reg_residuals, order = c(1, 0, 1))
  arimax_model_102 <- Arima(reg_residuals, order = c(1, 0, 2))
  arimax_model_103 <- Arima(reg_residuals, order = c(1, 0, 3))
  arimax_model_104 <- Arima(reg_residuals, order = c(1, 0, 4))
  arimax_model_202 <- Arima(reg_residuals, order = c(2, 0, 2))
  arimax_model_302 <- Arima(reg_residuals, order = c(3, 0, 2))
  arimax_model_303 <- Arima(reg_residuals, order = c(3, 0, 3))
  arimax_model_403 <- Arima(reg_residuals, order = c(4, 0, 3))
  sarimax_model_103_001 <- Arima(reg_residuals, 
                                order = c(1, 0, 3),
                                seasonal = list(order = c(0, 0, 1), period = 24))
  sarimax_model_103_100 <- Arima(reg_residuals, 
                                order = c(1, 0, 3),
                                seasonal = list(order = c(1, 0, 0), period = 24))
  sarimax_model_103_200 <- Arima(reg_residuals, 
                                order = c(1, 0, 3),
                                seasonal = list(order = c(2, 0, 0), period = 24))
  
  # Compare AIC/BIC of ARIMAX/SARIMAX Models
  print(AIC(arimax_model_100, arimax_model_101, arimax_model_102, arimax_model_103,
            arimax_model_104, arimax_model_202, arimax_model_302,arimax_model_303,
            sarimax_model_103_001,sarimax_model_103_100,sarimax_model_103_200))
  
  print(BIC(arimax_model_100, arimax_model_101, arimax_model_102, arimax_model_103,
            arimax_model_104, arimax_model_202, arimax_model_302,arimax_model_303,
            sarimax_model_103_001,sarimax_model_103_100,sarimax_model_103_200))
}

# plot residual
plot(residuals(sarimax_model_103_200), main = "", ylab = "")
abline(h = 0, col ="red")
#qqplot
qqnorm(scale(residuals(sarimax_model_103_200)), 
       main = "",
       xlab = "Theoretical quantile",
       ylab = "Sample quantile")
qqline(scale(residuals(sarimax_model_103_200)), col = "red", lwd = 2)
#ACF plot
acf(residuals(sarimax_model_103_200))
#PACF plot
pacf(residuals(sarimax_model_103_200),ylim=c(0,1))


# --- MODELS EVALUATION BY EXPANDING #####

h <- 72
n.forecasts <- n.valid - (h*2) + 1

# Containers
seasonal_naive_forecast <- seasonal_mean_forecast <- 
  sarimax_lm_forecast <- actual_matrix <- matrix(NA, n.forecasts, h)

# Seasonalities
month_vector <- as.numeric(format(valid_date, "%m"))
hour_vector <- as.numeric(format(valid_date, "%H"))

lm_fit <- NULL
sarimax_fit <- NULL
retrain_every <- 10

# === EXPANDING WINDOW ===
for (i in 1:n.forecasts) {
  
  #Train and test
  df_m_train <- data_hourly[1:(n.train + i - 1), ]
  df_m_test <- data_hourly[(n.train + i):(n.train + i + h - 1), ]
  y_train <- df_m_train$mean_temperature
  y_test <- df_m_test$mean_temperature
  actual_matrix[i, ] <- y_test
  
  # Seasonal Mean
  smean_table <- df_m_train %>%
    mutate(
      Month = as.numeric(format(Date, "%m")),
      Hour = as.numeric(format(Date, "%H"))
    ) %>%
    group_by(Month, Hour) %>%
    summarise(Avg = mean(mean_temperature)
    )
  
  for (s in 1:h) {
    current_month <- month_vector[i + s - 1]
    current_hour <- hour_vector[i + s - 1]
    match_row <- smean_table[smean_table$Month == current_month & smean_table$Hour == current_hour, ]
    
    seasonal_mean_forecast[i, s] <- match_row$Avg[1]
    
    seasonal_naive_forecast[i,s] <- y_train[length(y_train) - 8759 + s - 1]
  }

  #Data for Sarimax
  df_m_train <- df_m_train %>%
    mutate(Month = month(Date), Hour = hour(Date), Trend = 1:nrow(df_m_train))
  
  # SARIMAX: every 10 i
  if (i %% retrain_every == 0 || i == 1) {
    lm_fit <- lm(mean_temperature ~ Trend + I(Trend^2) + Hour + Month + Hour*Month + Hour*Trend,
                 data = df_m_train)
    residuals_lm <- residuals(lm_fit)
    sarimax_fit <- Arima(residuals_lm, order = c(1, 0, 3), seasonal = list(order = c(2, 0, 0), period = 24))
  }
  
  # Forecast
  future_trend <- (nrow(df_m_train) + 1):(nrow(df_m_train) + h)
  future_data <- data.frame(
    Trend = future_trend,
    Month = month_vector[i:(i + h - 1)],
    Hour = hour_vector[i:(i + h - 1)]
  )

  pred_lm <- predict(lm_fit, newdata = future_data)
  pred_sarimax <- forecast(sarimax_fit, h = h)$mean
  sarimax_lm_forecast[i, ] <- pred_lm + pred_sarimax
}

# MSE
mses_snaive <- colMeans((seasonal_naive_forecast - actual_matrix)^2, na.rm = TRUE)
mses_seasmean <- colMeans((seasonal_mean_forecast - actual_matrix)^2, na.rm = TRUE)
mses_sarimaxlm <- colMeans((sarimax_lm_forecast - actual_matrix)^2, na.rm = TRUE)

#Diebold Mariano test
dm_pvalues <- numeric(ncol(actual_matrix))

for (j in 1:ncol(actual_matrix)) {
  e_sarimaxlm <- actual_matrix[, j] - sarimax_lm_forecast[, j]
  e_seasnaive <- actual_matrix[, j] - seasonal_naive_forecast[, j]
  dm_test <- dm.test(e_sarimaxlm, e_seasnaive, alternative = "greater", h = 1, power = 2)
  dm_pvalues[j] <- dm_test$p.value
}


# PLOT MSEs

plot(mses_seasmean, type = "o", pch = 16, col = "gray20",
     cex = 0.8,
     ylim = c(0,25),
     lwd = 1,
     xlab = "Forecast step", 
     ylab = "MSE",
     main = "MSE Comparison",
     cex.main = 1.3, cex.lab = 1.2, cex.axis = 1)

lines(mses_sarimaxlm, type = "o",cex = 0.8, pch = 16, col = "gray50", lwd = 1, lty = 1)
lines(mses_snaive, type = "o",cex = 0.8, pch = 16, col = "gray80", lwd = 1, lty = 1)
# grid 
grid(col = "gray90", lty = "dotted")

# Legend
legend("topleft",
       legend = c("Conditional Mean", "Sarimax[24]","Seasonal Naive"), 
       col = c("gray20", "gray50","gray80"), 
       lty = c(1, 1),
       pch = c(16, 16),
       lwd = 1,
       bty = "n",  
       cex = 1,
       text.col = "black")


#Diebold Mariano plot
plot(dm_pvalues, type = "p", col = "gray30", lwd = 0.5,
     xlab = "Forecast step", ylab = "p-value",
     main = "DM test (Seasonal Naive VS Sarimax)")
abline(h = 0.05, col = "red", lty = 2, lwd = 2)

#Conditional mean gives best results, so we will use it to forecast temperature

#we will seasonal mean as conditional mean
# --- RESIDUALS ANALYSIS ####

# Compute fitted values
seasonal_naive_fitted <- df_m_train$mean_temperature[1:(length(df_m_train$mean_temperature) - 8760)]
seasonal_naive_fitted <- c(rep(NA, 8760), seasonal_naive_fitted)
conditional_means_table <- df_m_train %>%
  mutate(
    Month = as.numeric(format(Date, "%m")),
    Weekday = as.numeric(format(Date, "%u")),
    Hour = as.numeric(format(Date, "%H"))
  ) %>%
  group_by(Month, Weekday, Hour) %>%
  summarise(Conditional_Mean = mean(mean_temperature), .groups = "drop")

df_train_fitted <- df_m_train %>%
  mutate(
    Month = as.numeric(format(Date, "%m")),
    Weekday = as.numeric(format(Date, "%u")),
    Hour = as.numeric(format(Date, "%H"))
  ) %>%
  left_join(conditional_means_table, by = c("Month", "Weekday", "Hour"))
conditional_mean_fitted <- df_train_fitted$Conditional_Mean

# Convert fitted values to time series
seasonal_naive_fitted_ts <- ts(seasonal_naive_fitted, start = train_start, frequency = 24)
conditional_mean_fitted_ts <- ts(conditional_mean_fitted, start = train_start, end= train_end, frequency = 24)

# Compute residuals
seasonal_naive_residuals <- df_m_train$mean_temperature - seasonal_naive_fitted
conditional_mean_residuals <- df_m_train$mean_temperature - conditional_mean_fitted

par(mfrow = c(2, 2))

### Seasonal Naive residuals plot

# ACF
acf(seasonal_naive_residuals[!is.na(seasonal_naive_residuals)], main = "ACF - Seasonal Naive Residuals")
# PACF
pacf(seasonal_naive_residuals[!is.na(seasonal_naive_residuals)], main = "PACF - Seasonal Naive Residuals")
# Plot residuals
plot(seasonal_naive_residuals[!is.na(seasonal_naive_residuals)], type = "l", 
     main = "Residuals - Seasonal Naive", 
     ylab = "Residuals")
abline(h = 0, col = "red")
# QQ Plot
qqnorm(scale(seasonal_naive_residuals[!is.na(seasonal_naive_residuals)]), 
       main = "Q-Q Plot - Seasonal Naive Residuals",
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles")
qqline(scale(seasonal_naive_residuals[!is.na(seasonal_naive_residuals)]), col = "red", lwd = 2)

### Conditional Mean Residuals plot

# ACF
acf(conditional_mean_residuals, main = "ACF - Conditional Mean Residuals")
# PACF
pacf(conditional_mean_residuals, main = "PACF - Conditional Mean Residuals")
# Plot residuals
plot(conditional_mean_residuals, type = "l", 
     main = "Residuals - Conditional Mean", 
     ylab = "Residuals")
abline(h = 0, col = "red")
# QQ Plot
qqnorm(scale(conditional_mean_residuals), 
       main = "Q-Q Plot - Conditional Mean Residuals",
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles")
qqline(scale(conditional_mean_residuals), col = "red", lwd = 2)


# --- FORECAST WITH CONDITIONAL MEAN ####


# 1) FORECAST FOR ENERGY DEMAND EVALUATION ####

h <- 288

# Compute conditional mean on train set
conditional_means_table <- df_train_full %>%
  mutate(
    Month = as.numeric(format(Date, "%m")),
    Weekday = as.numeric(format(Date, "%u")),
    Hour = as.numeric(format(Date, "%H"))
  ) %>%
  group_by(Month, Weekday, Hour) %>%
  summarise(Conditional_Mean = mean(mean_temperature), .groups = "drop")

# Forecast 288 step ahead
end_train_date <- tail(df_train_full$Date, 1)
forecast_dates <- seq(from = end_train_date + hours(1), 
                      length.out = h, 
                      by = "hours")

# Create dataframe with future data
df_forecast <- data.frame(
  Date = forecast_dates,
  Month = as.numeric(format(forecast_dates, "%m")),
  Weekday = as.numeric(format(forecast_dates, "%u")),
  Hour = as.numeric(format(forecast_dates, "%H"))
) %>%
  left_join(conditional_means_table, by = c("Month", "Weekday", "Hour"))

# Extract forecast
conditional_mean_forecast <- df_forecast$Conditional_Mean

#Save the forecasts

conditional_mean_forecast<-data.frame(conditional_mean_forecast)
write_xlsx(conditional_mean_forecast, "forecast_smean_for_evaluation.xlsx")

# 2) FINAL FORECAST FOR ENERGY  FORECAST ####

h <- 72
real_df<-data_hourly[-72,]
# Compute conditional mean on train set
conditional_means_table <- real_df %>%
  mutate(
    Month = as.numeric(format(Date, "%m")),
    Weekday = as.numeric(format(Date, "%u")),
    Hour = as.numeric(format(Date, "%H"))
  ) %>%
  group_by(Month, Weekday, Hour) %>%
  summarise(Conditional_Mean = mean(mean_temperature), .groups = "drop")

# Forecast 288 step ahead
end_train_date <- tail(real_df$Date, 1)
forecast_dates <- seq(from = end_train_date + hours(1), 
                      length.out = h, 
                      by = "hours")

# Create dataframe with future data
df_forecast <- data.frame(
  Date = forecast_dates,
  Month = as.numeric(format(forecast_dates, "%m")),
  Weekday = as.numeric(format(forecast_dates, "%u")),
  Hour = as.numeric(format(forecast_dates, "%H"))
) %>%
  left_join(conditional_means_table, by = c("Month", "Weekday", "Hour"))

# Extract forecast
conditional_mean_forecast <- df_forecast$Conditional_Mean

#Save forecast in excel  
df_forecast<-data.frame(conditional_mean_forecast)
write_xlsx(df_forecast, "forecast_smean.xlsx")



# 3) FORECAST PLOT ####

# Index and zoo objects

real_temperatures<-tail(data_hourly$mean_temperature, 72)
time_index <- tail(data_hourly$Date, 72)
forecast_smean_zoo <- zoo(df_forecast, order.by = time_index)
real_temperatures_zoo<-zoo(real_temperatures, order.by=time_index)

#plot

plot(real_temperatures_zoo,
     main = "Conditional Mean Forecast vs Real average Temperatures",
     ylim = range(c(real_temperatures, df_forecast), na.rm = TRUE),
     ylab = "Energy Demand (MW)", 
     xlab = "",
     lwd = 2, 
     col = "gray10")

grid(col = "lightgray", lty = "dotted")

lines(forecast_smean_zoo, col = "blue", lwd = 2)

legend("bottomright",
       legend = c("Real average temperature", "Average temperature Forecast"), 
       col = c("gray10", "blue"), 
       lty = 1,
       cex = 0.9)










