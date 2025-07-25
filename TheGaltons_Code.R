# Clean the environment ####
rm(list=ls())
# LIBRARIES ####
# Load necessary packages
library(readxl)
library(xts)
library(dplyr)  
library(ggplot2)
library(forecast) 
library(lubridate)
library(scales)
library(timeSeries)
library(urca)
library(uroot)
library(zoo)

########## CLEANING & SET UP DATASET ####
# --- IMPORT & CLEANUP ENERGY DATASET WITH TEMP FORECAST ####
# Read data
df <- read_excel("Hourly_Load.xlsx", sheet = "Load_Temp")
# Create a copy of the dataset to preserve the original data
data_hourly <- df

# Check data
head(data_hourly)
summary(data_hourly)

# Convert Date column into POSIXct objects
Date <- as.POSIXct(data_hourly$Date, format="%Y-%m-%d %H:%M:%S")
data_hourly$Date <- Date

# Create zoo hourly time series object (for plot visualization)
zoo_hourly <- zoo(data_hourly$Total_load, order.by = Date)

# Full plot of the time series
plot(zoo_hourly,ylab="Total Load",xlab="",main="Sardinia Energy Demand")

# --- TEMPORAL AGGREGATIONS #####

# Monthly aggregation 
data_monthly <- df %>% 
  mutate(YearMonth = format(Date, "%Y-%m")) %>% 
  group_by(YearMonth) %>% 
  summarise(Total_load = mean(Total_load))
# Create n. of Year & Month
data_monthly$YearMonth <- as.Date(paste0(data_monthly$YearMonth, "-01"), format="%Y-%m-%d")

# Daily aggregation
data_daily <- df %>% 
  mutate(Day = format(Date, "%Y-%m-%d")) %>% 
  group_by(Day) %>% 
  summarise(Total_load = mean(Total_load))

# --- CREATE USEFUL VARIABLES ####
# Create column Month 
data_hourly$Month <- format(data_hourly$Date, "%m")
# factorize
data_hourly$Month <- as.factor(data_hourly$Month)

# create the column number day of the week 
data_hourly$Weekday <- format(data_hourly$Date, "%u")
data_hourly$Weekday <- as.factor(data_hourly$Weekday)

# Hour
data_hourly$Hour <- format(data_hourly$Date, "%H")
data_hourly$Hour <- as.factor(data_hourly$Hour)

# Dummy Festivity
data_hourly <- data_hourly %>%
  mutate(
    Festivity = if_else(
      format(Date, "%m-%d") %in% c(
        "01-01",  # Capodanno
        "01-06",  # Epifania
        "04-25",  # Liberazione
        "05-01",  # Festa del lavoro
        "06-02",  # Festa della Repubblica
        "08-15",  # Ferragosto
        "11-01",  # Ognissanti
        "12-08",  # Immacolata Concezione
        "12-25",  # Natale
        "12-26"   # Santo Stefano
      ),
      1, 0
    )
  )

# Create Trend variable
Trend <- 1:nrow(data_hourly)
data_hourly$Trend <- Trend

# sin() & cos() 
data_hourly$sin_month <- sin(2 * pi * as.numeric(data_hourly$Month) / 12)
data_hourly$cos_month <- cos(2 * pi * as.numeric(data_hourly$Month) / 12)

data_hourly$sin_hour <- sin(2 * pi * as.numeric(data_hourly$Hour) / 24)
data_hourly$cos_hour <- cos(2 * pi * as.numeric(data_hourly$Hour) / 24)

# create dummy weekend
data_hourly$Weekend <- ifelse(data_hourly$Weekday %in% c(6, 7), 1, 0)

# --- CREATE TS OBJECT WITH DIFFERENT FREQ  #####

# ts Monthly
ts_monthly <- ts(data_monthly$Total_load, start = c(2020, 1), frequency = 12)
# ts Daily
ts_daily <- ts(data_daily$Total_load, start = c(2020, 1), frequency = 365)
# ts Hourly - Freq = 8759
ts_hourly <- ts(data_hourly$Total_load, start = c(2020, 1), frequency = 8759)
# ts Hourly - Freq = 24
ts_hourly_24 <-ts(data_hourly$Total_load, start = c(2020, 1), frequency = 24)
# Zoo - Hourly
zoo_hourly <- zoo(data_hourly$Total_load, order.by = data_hourly$Date)


########## EDA ####
# --- 1) Monthly plot ####
# Calculate monthly dates
start_year <- start(ts_monthly)[1]
start_month <- start(ts_monthly)[2]
end_year <- end(ts_monthly)[1]
end_month <- end(ts_monthly)[2]

# Create monthly date sequence
start_date <- as.Date(paste(start_year, start_month, "01", sep = "-"))
end_date <- as.Date(paste(end_year, end_month, "01", sep = "-"))
all_dates <- seq(from = start_date, to = end_date, by = "month")

# Simple base R plot without x-axis labels
plot(all_dates, as.numeric(ts_monthly), 
     type = "l",
     main = "Monthly Data", 
     ylab = "LOAD [MW]",
     xlab = "Time",
     xaxt = "n")

# Add custom labels 
years <- seq(start_year, end_year, by = 1)
year_dates <- as.Date(paste0(years, "-01-01"))
axis(1, at = year_dates, labels = years)

# --- 2) Daily plot ####
# Calculate actual dates directly
# Calculate dates for daily data
end_date <- as.Date("2025-01-01") + 62  # 63rd day of 2025
start_date <- end_date - (length(ts_daily) - 1)  # Starting date

# Create complete date sequence
all_dates <- seq(from = start_date, to = end_date, by = "day")

# Take last 2000 days and create simple base R plot
plot(tail(all_dates, 2000), as.numeric(tail(ts_daily, 2000)), 
     type = "l",
     main = "Daily Data", 
     ylab = "LOAD [MW]",
     xlab = "Time")

# --- 3) Weekly plot ####
# Calculate actual dates for last 7 days of hourly data
# Calculate dates for last 7 days of hourly data
# Simple base R plot
plot(zoo_hourly[(45259-336):45259], 
     type = "l",
     main = "Last 7 Days - Hourly Energy Demand", 
     ylab = "LOAD [MW]",
     xlab = "Time")

##### VISUALIZE SEASONALITY ####
# --- Create variable to visualize seasonality ####
df_short <- df
df$Day <- as.Date(df$Date)
df_short$Day <- as.Date(df_short$Date)
# remove last 48 obs to correctly plot Monthly Seasonality
df_short<-head(df,nrow(df)-2*24)
df_short$Year <- as.numeric(format(df_short$Date, "%Y"))
df_short$Month <- as.numeric(format(df_short$Date, "%m"))             # Extract month
df$Weekday <- as.numeric(format(df$Date, "%u"))           # Extract weekday (1 = Monday, 7 = Sunday)
df$Hour <- as.numeric(format(df$Date, "%H"))              # Extract hour of the day (0-23)
df$Weekend <- as.numeric(ifelse(df$Weekday %in% c(6, 7), 1, 0))  # Weekends: 1 if Saturday or Sunday, else 0
df$weeknum <- as.numeric(format(df$Date, "%U"))           # Week number of the year (starting Sunday as week 1)
# --- MONTHLY ####
# Monthly aggregation
monthly_avg <- df_short %>%
  group_by(Year, Month) %>%
  summarise(Avg_Load = mean(Total_load, na.rm = TRUE), .groups = 'drop')

# Plot 
ggplot(monthly_avg, aes(x = Month, y = Avg_Load, group = Year)) +
  geom_line(color = "grey10", alpha = 0.9, linewidth = 0.5) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  labs(
    title = "Monthly Seasonality",
    y = "Total Load"
  ) +
  theme_minimal()

# --- WEEKLY ####

# Create variable : hours since start of the week 
df$hour_of_week <- (df$Weekday - 1) * 24 + df$Hour

# Take last 120 weeks 
df_subset <- tail(df, 120 * 24)  
# order by week
df_subset <- df_subset[order(df_subset$weeknum, df_subset$hour_of_week),]  

# Plot
ggplot(df_subset, aes(x = hour_of_week, y = Total_load, group = weeknum)) +
  geom_line(color = "grey10", alpha = 0.8, linewidth = 1) +
  scale_x_continuous(
    breaks = seq(0, 144, 24),
    labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  ) +
  labs(
    title =  "Weekly Seasonality (last 120 weeks)",
    x="",
    y = "Total Load"
  ) +
  theme_minimal()

# --- DAILY ####
# Plot 5 Month
ggplot(tail(df,150*24), aes(x = Hour, y = Total_load, group = Day, color = factor(Weekend))) +
  geom_line(alpha = 0.7, linewidth = 1) +
  scale_x_continuous(
    breaks = seq(0, 23, 2),
    labels = paste0(seq(0, 23, 2), ":00")
  ) +
  scale_color_manual(
    values = c("0" = "grey10", "1" = "gold"),
    labels = c("Weekdays", "Weekend")
  ) +
  labs(
    title = "Daily Seasonality (last 5 month)",
    x = "",
    y = "Total Load",
    color = "Day type"
  ) +
  theme_minimal()

# --- CHECK THE TIME SERIES DISTRIBUTION and ACF & PACF #### 
# Hist of the energy hourly series
hist(zoo_hourly, breaks = 30,
     col = "lightblue",
     probability = TRUE,
     ylim = c(0,0.0025),
     main = "Histogram with theoretical normal curve",
     xlab = "Values")
devstd <- sd(zoo_hourly, na.rm = TRUE)
# Add Gaussian curve on top of the hist
mean_ts <- mean(zoo_hourly)
curve(dnorm(x, mean = mean_ts, sd = devstd), 
      col = "red", lwd = 2, add = TRUE)
par(mfrow=c(1,2))

# Compute ACF of the hourly series
ts_hourly_plot_acf <- ts(ts_hourly_24, frequency = 1)
acf_result <- acf(ts_hourly_plot_acf, lag.max=24*4, main='')
pacf(ts_hourly_plot_acf, lag.max=24*4, main='')

########## Comment ###########
# the data seems to have approximately a normal distribution, since n is very large we can approximate to a Normal distribution
# the acf plot shows a persistent seasonality (the time series is probably not stationary)
# seems to be very persistent deterministic seasonality since slowly decrease to zero
# the pacf shows different significative lags after lag 1

# --- TIME SERIES DECOMPOSITION ####

# STL Hourly (full dataset) decomposition
decomp <- stl(ts_hourly, s.window = "periodic")
plot(decomp)
title("STL Decomposition of Hourly Data")

# --- TREND ANALYSIS ####
### Daily moving averages
ma_4m <- ma(ts_hourly_24, 720*4)
ma_12m <- ma(ts_hourly_24, 720*12) # Twice the length of the seasonality (12*2)

# Plot original series with moving averages
par(mfrow=c(1,1))
plot(ts_hourly_24,
     main = "",
     ylab = "Total Load [MW]",
     xlab = "")  
lines(ma_4m, col = "blue", lwd = 2)
lines(ma_12m, col = "red", lwd = 2)
legend("topright", 
       legend = c("Original", "MA (4 month)", "MA (12 month)"), 
       col = c("black", "blue", "red"), 
       lty = 1,
       cex = 0.6)
######### CHECL FOR STRUCTURAL BREAK
########## TEST ####
# --- 1) CHOW TEST ####
# Check for structural break
# look at the plot decomposition we expect a break in the early 2022
# interval : "2021-12-31 23:00:00 UTC" - "2022-01-31 23:00:00 UTC"  
start_time <- 17518
end_time <- 18262
break_points <- start_time:end_time
F_stat <- numeric(length(break_points))

for(i in seq_along(break_points)) {
  tau <- break_points[i]
  
  # dummy break
  data_hourly$D_t <- ifelse(data_hourly$Trend > tau, 1, 0)
  # iteration trend-dummy
  data_hourly$trend_break <- data_hourly$D_t * data_hourly$Trend             
  
  # Fit model with break point
  model1 <- lm(Total_load ~ D_t + Trend + trend_break + I(Trend^2) + I(log(Trend)) + 
                 Hour + Festivity + Weekday + Month + Temp, data = data_hourly)
  # Compute F stat
  F_stat[i] <- summary(model1)$coefficients["trend_break", "t value"]^2
}

# Plot break point
plot(F_stat, type = "p",
     xlab = "Index", ylab = "F-statistic",
     main = "Plot Dynamic F-statistic")
optimal_index <- which.max(F_stat)
points(optimal_index, F_stat[optimal_index], 
       col = "red", pch = 19, cex = 1.3)

# Date of the break
time_of_break <- which.max(F_stat) + start_time -1
data_hourly$Date[time_of_break]
##### Comment
# The break does not correspond to our hypothesis, so we won't consider it in our model


# --- 2) ADF TEST ####

# Augmented Dickey Fuller test 

# No intercept, no trend
summary(ur.df(zoo_hourly, type = "none", selectlags = "BIC"))

# With intercept only
summary(ur.df(zoo_hourly, type = "drift", selectlags = "BIC"))

# With intercept and linear trend
summary(ur.df(zoo_hourly, type = "trend", selectlags = "BIC"))

##### Comment
# Test statistic shows : no unit root present 

# --- 3) CH TEST ####

# Perform CH test for seasonal variation
ch.test(ts_monthly)

########## CREATE TRAIN VALIDATION & TEST SET ####
# --- 1) Zoo - TRAIN AND TEST SPLIT ####
# Split data for forecasting evaluation
train_start <- as.POSIXct("2020-01-01 00:00:00")
train_end   <- as.POSIXct("2025-01-31 23:00:00")
valid_start <- as.POSIXct("2025-02-01 00:00:00")
valid_end <- as.POSIXct("2025-02-15 23:00:00")
test_start <- as.POSIXct("2025-02-16 00:00:00")
test_end <- as.POSIXct("2025-02-27 23:00:00")
train <- window(zoo_hourly, end = train_end)
valid <- window(zoo_hourly, start = valid_start, end = valid_end )
test <- window(zoo_hourly, start = test_start, end = test_end)
train_full <- window(zoo_hourly, start =train_start , end = valid_end)

# valid & Train length
n.valid <- length(valid)
n.train <- length(train)
n.test <-length(test)
df_train <- head(data_hourly, n.train)
df_valid <- head(tail(data_hourly, (n.valid*2)), n.valid)
df_train_full <- head(data_hourly, (n.train + n.valid+1))
df_full <- head(data_hourly, (n.train + n.valid+289))
# Date vector
valid_date <- df_valid$Date

# --- 2) ts - TRAIN AND TEST SPLIT (freq = 24) #####
train_start_24 <-c(2020,1)
train_end_24 <- c((3905-30), 19) 
valid_start_24 <- c((3905-30),20)
valid_end_24 <- c((3905-15), 19) 
train_24 <- window(ts_hourly_24, start =train_start_24, end = train_end_24)
valid_24 <- window(ts_hourly_24, start = valid_start_24, end= valid_end_24)
n.valid_24 <- length(valid_24)
df_train_24 <- head(data_hourly, (nrow(data_hourly) - n.valid_24*2))
df_valid_24 <- head(tail(data_hourly,n.valid_24*2),n.valid_24)
valid_date_24 <- df_valid_24$Date
train_full_24 <- window(ts_hourly_24, start =train_start_24, end = valid_end_24)

# --- 3) SET-UP ROLLING WINDOWS ####
## chose forecast horizon
days <- 3
h <- 24*days
window_size <- 13140 #1.5 years
# 360-71
n.forecasts <- n.valid - h +1
n.obs <- window_size + h + n.forecasts -1
roll_start <- tail(zoo_hourly,(n.obs+360))
roll_start_24 <- tail(ts_hourly_24,(n.obs+360))
########## SIMPLE ALGORITHM ####
# --- 1) CREATE SEASONAL MEAN'S DUMMY ####

# Monthly seasonality 
month_vector <- as.numeric(format(valid_date, "%m"))

# Weekly seasonality
weekday_vector <- as.numeric(format(valid_date, "%u"))

# Hour seasonality
hour_vector <- as.numeric(format(valid_date, "%H"))
# --- 2) FORECAST {NAIVE, SEASONAL NAIVE, CONDITIONAL MEAN,MEAN} ####
# Initialize matrix for forecasts
valid_rw <- naive_forecast <- seasonal_naive_forecast <- mean_forecast <- conditional_mean_forecast <- matrix(NA, n.forecasts, h)

for (i in 1:n.forecasts) {
  
  # Define start and end time for training window
  train_start <- time(roll_start)[i]
  train_end <- time(roll_start)[i + window_size - 1]
  
  # Extract training window as ts object
  train_window <- window(roll_start, start = train_start, end = train_end)
  
  df_rw <- head(tail(data_hourly, length(roll_start) + 1 - i), window_size )
  
  data_hourly_smean <- df_rw %>%
    mutate(
      Month = as.numeric(format(Date, "%m")),
      Weekday = as.numeric(format(Date, "%u")),
      Hour = as.numeric(format(Date, "%H"))
    ) %>%
    group_by(Month, Weekday, Hour) %>%
    summarise(Total_load = mean(Total_load)
    )
  
  for (s in 1:h) {
    
    valid_rw[i,s] <- valid[(i + s - 1)]
    
    naive_forecast[i,] <- tail(train_window, 1)
    
    seasonal_naive_forecast[i,s] <- train_window[length(train_window) - 8759 + s - 1]
    
    mean_forecast[i,] <- mean(train_window)
    
    current_month <- month_vector[(i+s-1)]
    current_weekday <- weekday_vector[(i+s-1)]
    current_hour <- hour_vector[(i+s-1)]
    
    filtered_row <- data_hourly_smean[
      data_hourly_smean$Month == current_month & 
        data_hourly_smean$Weekday == current_weekday & 
        data_hourly_smean$Hour == current_hour, 
    ]
    
    conditional_mean_forecast[i,s] <- filtered_row$Total_load
  }
}

# --- 3) PLOT : FORECAST ####

# --- Plot last 72 step ahead forecasts
naive_zoo <- zoo(as.numeric(naive_forecast[289,]), order.by = index(tail(valid,72)))
seasonal_naive_zoo <- zoo(as.numeric(seasonal_naive_forecast[289,]), order.by = index(tail(valid,72)))
mean_zoo <- zoo(as.numeric(mean_forecast[289,]), order.by = index(tail(valid,72)))
conditional_mean_zoo <- zoo(as.numeric(conditional_mean_forecast[289,]), order.by = index(tail(valid,72)))

plot(tail(valid,72),
     main = "Simple Forecasting Methods",
     ylim = c(500,1500),
     ylab = "Energy Demand (MW)",lwd = 3,col ='gray20',xlab="")
grid(col = "lightgray", lty = "dotted")
lines(seasonal_naive_zoo, col = "darkgreen", lwd = 3)
lines(conditional_mean_zoo, col = "blue", lwd = 1.5)
lines(mean_zoo, col = "purple", lwd = 1.5)
legend("topleft",
       legend = c("Seasonal Naive", "Conditional Mean", "Mean"), 
       col = c("darkgreen", "blue","purple"), 
       lty = 1,
       cex = 0.8)

# --- Plot all forecasts together (each 72 step)

# Rows to plot:
rows_to_plot <- seq(n.forecasts, 1, by = -72)

# Plot all forecasts together
plot(valid,
     main = "",
     ylim = c(600, 1400),
     ylab = "Energy Demand (MW)",
     type = "l",
     col = "black",
     lwd = 2,
     xaxt = "n",
     xlab = "")

# add x ticks
axis.POSIXct(1, at = seq(start(valid), end(valid), by = "day"), 
             format = "%b %d", 
             tcl = -0.3,
             mgp = c(3, 1, 0))

# Create time series every 72 days
for (i in 1:length(rows_to_plot)) {
  
  # Select row
  r <- rows_to_plot[i]
  
  # Calculate the correct start time for this forecast
  # Assuming valid starts at a certain date and each row represents h=72 hours ahead
  forecast_start <- index(valid)[1] + (r - 1) * 3600  # Add (r-1) hours in seconds
  time_index <- seq(from = forecast_start, by = "hour", length.out = 72)
  
  # Create zoo time series 
  naive_zoo <- zoo(naive_forecast[r, ], order.by = time_index)
  seasonal_naive_zoo <- zoo(seasonal_naive_forecast[r, ], order.by = time_index)
  mean_zoo <- zoo(mean_forecast[r, ], order.by = time_index)
  seasonal_mean_zoo <- zoo(conditional_mean_forecast[r, ], order.by = time_index)
  
  # Add lines to plot
  #lines(naive_zoo, col = "red", lwd = 1)
  lines(seasonal_naive_zoo, col = "darkgreen", lwd = 1)
  lines(mean_zoo, col = "purple", lwd = 1)
  lines(seasonal_mean_zoo, col = "blue", lwd = 1)
}
# Legend
legend("topleft", 
       legend = c( "Seasonal Naive", "Mean", "Conditional Mean"), 
       col = c( "darkgreen","purple", "blue"), 
       lty = 1, 
       cex = 0.7)


# --- 4) COMPARE MSEs ####

dm.pvalues<- MSEs_naive <- MSEs_seas_naive <- MSEs_mean <- MSEs_conditional_mean <- MSEs_arimax <- numeric(h)

for (s in 1:h){
  
  # Errors
  e.naive <- valid_rw - naive_forecast
  e.seas_naive <- valid_rw - seasonal_naive_forecast
  e.mean <- valid_rw - mean_forecast
  e.seas_mean <- valid_rw - conditional_mean_forecast
  
  # MSE
  MSEs_naive[s] <- mean(e.naive[,s]^2)
  MSEs_seas_naive[s] <- mean(e.seas_naive[,s]^2)
  MSEs_mean[s] <- mean(e.mean[,s]^2)
  MSEs_conditional_mean[s] <- mean(e.seas_mean[,s]^2)
  
  # Diebold-Mariano test
  dm.pvalues[s] <- dm.test(as.vector(e.naive[,s]), as.vector(e.seas_mean[,s]),
                           alternative = "greater", h = 1, power = 2)$p.value
}

# --- PLOT MSEs
plot(MSEs_seas_naive, type = "o", pch = 16, col = "darkgreen",
     cex = 0.8,
     ylim = c(0,30000),
     lwd = 1,
     xlab = "Forecast step", 
     ylab = "MSE",
     main = "MSE Comparison",
     cex.main = 1.3, cex.lab = 1.2, cex.axis = 1)

lines(MSEs_conditional_mean, type = "o",cex = 0.8, pch = 16, col = "blue", lwd = 1, lty = 1)
lines(MSEs_mean, type = "o",cex = 0.8, pch = 16, col = "purple", lwd = 1, lty = 1)

# grid
grid(col = "gray90", lty = "dotted")

# Legend
legend("left",
       legend = c("Seasonal naive", "Conditional mean","Mean"), 
       col = c("darkgreen", "blue","purple"), 
       lty = c(1, 1),
       pch = c(16, 16),
       lwd = 1,
       bty = "n",
       cex = 1.1,
       text.col = "black")


# --- PLOT P-VALUE DM TEST

# For alternative="greater", the alternative hypothesis is that method 2 is more accurate than method 1
plot(dm.pvalues, type = "p", col = "gray20", lwd = 2,ylim= c(0,1),
     xlab = "Forecast", ylab = "p-value",
     main = "Diebold-Mariano p-values - Conditiona Mean vs Seasonal naive ")
abline(h = 0.05, col = "red", lty = 2, lwd = 2)

# --- 5) RESIDUALS ANALYSIS ####
# Compute fitted values
naive_fitted <- df_rw$Total_load[-length(df_rw$Total_load)]
naive_fitted <- c(NA, naive_fitted)
seasonal_naive_fitted <- df_rw$Total_load[1:(length(df_rw$Total_load) - 8760)]
seasonal_naive_fitted <- c(rep(NA, 8760), seasonal_naive_fitted)
mean_fitted <- rep(mean(train_window), length.out = length(df_rw$Total_load))
conditional_means_table <- df_rw %>%
  mutate(
    Month = as.numeric(format(Date, "%m")),
    Weekday = as.numeric(format(Date, "%u")),
    Hour = as.numeric(format(Date, "%H"))
  ) %>%
  group_by(Month, Weekday, Hour) %>%
  summarise(Conditional_Mean = mean(Total_load), .groups = "drop")

df_train_fitted <- df_rw %>%
  mutate(
    Month = as.numeric(format(Date, "%m")),
    Weekday = as.numeric(format(Date, "%u")),
    Hour = as.numeric(format(Date, "%H"))
  ) %>%
  left_join(conditional_means_table, by = c("Month", "Weekday", "Hour"))
conditional_mean_fitted <- df_train_fitted$Conditional_Mean

# Convert fitted values to zoo
time_seq <- seq(from = as.POSIXct(train_start), 
                to = as.POSIXct(train_end), 
                by = "hour")

# Create zoo object
naive_fitted_zoo <- zoo(naive_fitted, order.by = time_seq)
seasonal_naive_fitted_zoo <- zoo(seasonal_naive_fitted, order.by = time_seq)
mean_fitted_zoo <- zoo(mean_fitted, order.by = time_seq)
conditional_mean_fitted_zoo <- zoo(conditional_mean_fitted, order.by = time_seq)

# Compute residuals
naive_residuals <- df_rw$Total_load - naive_fitted
seasonal_naive_residuals <- df_rw$Total_load - seasonal_naive_fitted
mean_residuals <- df_rw$Total_load - mean_fitted
conditional_mean_residuals <- df_rw$Total_load - conditional_mean_fitted

par(mfrow = c(2, 2))

### Naive residuals plot
naive_residuals_clean <- na.omit(naive_residuals)

# ACF
acf(naive_residuals_clean, main = "ACF - Naive Residuals")
# PACF
pacf(naive_residuals_clean, main = "PACF - Naive Residuals")
# Plot residuals
plot(naive_residuals_clean, type = "l", 
     main = "Residuals - Naive", 
     ylab = "Residuals")
abline(h = 0, col = "red")
# QQ Plot
qqnorm(scale(naive_residuals_clean), 
       main = "Q-Q Plot - Naive Residuals",
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles")
qqline(scale(naive_residuals_clean), col = "red", lwd = 2)

### Seasonal Naive residuals plot
seasonal_naive_residuals_clean <- na.omit(seasonal_naive_residuals)

# ACF
acf(seasonal_naive_residuals_clean, main = "ACF - Seasonal Naive Residuals")
# PACF
pacf(seasonal_naive_residuals_clean, main = "PACF - Seasonal Naive Residuals")
# Plot residuals
plot(seasonal_naive_residuals_clean, type = "l", 
     main = "Residuals - Seasonal Naive", 
     ylab = "Residuals")
abline(h = 0, col = "red")
# QQ Plot
qqnorm(scale(seasonal_naive_residuals_clean), 
       main = "Q-Q Plot - Seasonal Naive Residuals",
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles")
qqline(scale(seasonal_naive_residuals_clean), col = "red", lwd = 2)

### Mean Residuals plot
mean_residuals_clean <- na.omit(mean_residuals)

# ACF
acf(mean_residuals_clean, main = "ACF - Mean Residuals")
# PACF
pacf(mean_residuals_clean, main = "PACF - Mean Residuals")
# Plot residuals
plot(mean_residuals_clean, type = "l", 
     main = "Residuals - Mean", 
     ylab = "Residuals")
abline(h = 0, col = "red")
# QQ Plot
qqnorm(scale(mean_residuals_clean), 
       main = "Q-Q Plot - Mean Residuals",
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles")
qqline(scale(mean_residuals_clean), col = "red", lwd = 2)

### Conditional Mean Residuals plot
conditional_mean_residuals_clean <- na.omit(conditional_mean_residuals)
# ACF
acf(conditional_mean_residuals_clean, main = "ACF - Conditional Mean Residuals")
# PACF
pacf(conditional_mean_residuals_clean, main = "PACF - Conditional Mean Residuals")
# Plot residuals
plot(conditional_mean_residuals_clean, type = "l", 
     main = "Residuals - Conditional Mean", 
     ylab = "Residuals")
abline(h = 0, col = "red")
# QQ Plot
qqnorm(scale(conditional_mean_residuals_clean), 
       main = "Q-Q Plot - Conditional Mean Residuals",
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles")
qqline(scale(conditional_mean_residuals_clean), col = "red", lwd = 2)

########## SMOOTHING ALGORITHM (ETS) ####
# --- 1) COMPUTE ETS's AIC/BIC ON TRAIN ####
ses <-ets(train_24, model = "ANN")
aic_ses<-AIC(ses)
bic_ses<-BIC(ses)

holt<-ets(train_24, model = "AAN", damped = TRUE)
aic_holt <-AIC(holt)
bic_holt<-BIC(holt)

ets_ana <- ets(train_24, model = "ANA")
aic_ets_ana<-AIC(ets_ana)
bic_ets_ana<-BIC(ets_ana)

hw_add <- ets(train_24, model = "AAA")
aic_hw_add<-AIC(hw_add)
bic_hw_add<-BIC(hw_add)

hw_mul <- ets(train_24, model = "MAM")
aic_hw_mul<-AIC(hw_mul)
bic_hw_mul<-BIC(hw_mul)

ets_mnm <- ets(train_24, model = "MNM")
aic_ets_mnm<-AIC(ets_mnm)
bic_ets_mnm<-BIC(ets_mnm)

# Compare AIC and BIC (for ETS vs Seasonal Mean)
results <- data.frame(
  Model = c("SES", "Holt Damped", "ANA", "AAA","MAM","MNM"),
  AIC = c(aic_ses, aic_holt, aic_ets_ana, aic_hw_add, aic_hw_mul,aic_ets_mnm),
  BIC = c(bic_ses, bic_holt, bic_ets_ana, bic_hw_add, bic_hw_mul,bic_ets_mnm)
)
results[order(results$AIC), ]

# ******** Comment ********
# MNM show the lowest AIC and BIC, but looking at the data we think that an ANA could suite
# the data more, so we take in cosideration both MNM and ANA

########## ROLLING WINDOW ETS (ANA,MNM) ####
# --- 3) SET UP ROLLING WINDOW ####
# Initialize vector for forecasts
window_valid <- point_forecast_ana <- point_forecast_mnm <- matrix(NA, n.forecasts, h)
lower_95_ana <- upper_95_ana <- matrix(NA, n.forecasts, h)
lower_95_mnm <- upper_95_mnm <- matrix(NA, n.forecasts, h)

window_size <- 13140 # 1.5 Years
n.forecasts <- n.valid - h + 1
n.obs <- window_size + h + n.forecasts -1
roll_start <- tail(zoo_hourly,(n.obs+360))
roll_start_24 <- tail(ts_hourly_24,(n.obs+360))

# --- 4) Rolling window : Forecast Model 'ANA' ####
for (i in 1:n.forecasts) {
  train_start <- time(roll_start_24)[i]
  train_end <- time(roll_start_24)[i + window_size - 1]
  # window of the observed values
  window_valid[i,] <- valid_24[i:(i+h-1)]
  
  # Extract training window as ts object
  train_window <- window(roll_start_24, start = train_start, end = train_end)
  # Fit ETS model
  ets_ana <- ets(train_window, model = "ANA")
  
  
  # Compute forecast
  forecast_ets_ana <- forecast(ets_ana, h = h)
  
  # Store point forecast
  point_forecast_ana[i,] <- forecast_ets_ana$mean
  
  # prediction interval
  lower_95_ana[i,] <- forecast_ets_ana$lower[,2]  
  upper_95_ana[i,] <- forecast_ets_ana$upper[,2]
}

# --- 5) Rolling window : Forecast Model 'MNM' ####

for (i in 1:n.forecasts) {
  train_start <- time(roll_start_24)[i]
  train_end <- time(roll_start_24)[i + window_size - 1]
  window_valid[i,] <- valid_24[i:(i+h-1)]
  
  # Extract training window as ts object
  train_window <- window(roll_start_24, start = train_start, end = train_end)
  
  # Fit ETS model
  ets_mnm <- ets(train_window, model = "MNM")
  
  # Compute forecast
  forecast_ets_mnm <- forecast(ets_mnm, h = h)
  
  # Store point forecast
  point_forecast_mnm[i,] <- forecast_ets_mnm$mean
  
  # prediction interval
  lower_95_mnm[i,] <- forecast_ets_mnm$lower[,2]  
  upper_95_mnm[i,] <- forecast_ets_mnm$upper[,2]
}

# --- 6) PLOT : FORECAST ####
# --- Plot last 72 forecast
par(mfrow=c(1,1))

#Convert in zoo for plotting
point_forecast_ana_zoo <- zoo(as.numeric(point_forecast_ana[289,]), order.by = index(tail(valid,72)))
point_forecast_mnm_zoo <- zoo(as.numeric(point_forecast_mnm[289,]), order.by = index(tail(valid,72)))

plot(tail(valid,72),
     main = "ETS Models",
     ylim = c(500, 1500),
     ylab = "Energy Demand (MW)",
     xlab ="",
     col = "gray20",
     lwd = 2,
     cex.main = 1.3,
     cex.lab = 1.2,
     cex.axis = 1,
     lty = 1)
#forecast 'ana'
lines(point_forecast_ana_zoo, col = "blue", lwd = 2, 
      lty = 1,
      type = "l",
      pch = 18,
      cex = 1)
#forecast 'mnm'
lines(point_forecast_mnm_zoo, col = "darkgreen", lwd = 2, lty = 1,
      type = "l",
      pch = 16,
      cex = 1)
#grid
grid(col = "gray90", lty = "dotted")
#legend
legend("topleft",
       legend = c("'ANA'", "'MNM'"), 
       col = c("blue","darkgreen"), 
       lty = 1,
       cex = 0.8)

# --- 7) PLOT Forecast for all Validation set ####

# Rows to plot:
rows_to_plot <- seq(289, 1, by = -72)

# Plot all forecasts together
plot(valid,
     main = "Smoothing Algorithms- ETS",
     ylim = c(600, 1400),
     ylab = "Energy Demand (MW)",
     type = "l",
     col = "black",
     lwd = 2,
     xaxt = "n",
     xlab = "")

# add x ticks
axis.POSIXct(1, at = seq(start(valid), end(valid), by = "day"), 
             format = "%b %d", 
             tcl = -0.3,
             mgp = c(3, 1, 0))

# Create time series every 72 days
for (i in 1:length(rows_to_plot)) {
  
  # Select row
  r <- rows_to_plot[i]
  
  # Calculate the correct start time for this forecast
  # Assuming valid starts at a certain date and each row represents h=72 hours ahead
  forecast_start <- index(valid)[1] + (r - 1) * 3600  # Add (r-1) hours in seconds
  time_index <- seq(from = forecast_start, by = "hour", length.out = 72)
  
  # Create zoo time series 
  ana_zoo <- zoo(point_forecast_ana[r, ], order.by = time_index)
  mnm_zoo <- zoo(point_forecast_mnm[r, ], order.by = time_index)
  
  # Add lines to plot
  lines(ana_zoo, col = "darkgreen", lwd = 1)
  lines(mnm_zoo, col = "blue", lwd = 1)
}
# Legend
legend("topleft", 
       legend = c( "ANA", "MNM"), 
       col = c( "darkgreen", "blue"), 
       lty = 1, 
       cex = 0.7)


# --- 8) COMPARE MSEs ####

# Inizialize MSE evaluation
dm_pv_mse <- dm_pv_mse2 <- MSE_mnm <- MSE_ana <- numeric(h)

# Errors
errors_ets_ana <- window_valid -  point_forecast_ana
errors_ets_mnm <- window_valid - point_forecast_mnm

for (s in 1:h){
  # MSE
  MSE_ana[s] <- mean(errors_ets_ana[,s]^2)
  MSE_mnm[s] <- mean(errors_ets_mnm[,s]^2)
  
  dm_test_mse2 <- dm.test(errors_ets_mnm[,s], errors_ets_ana[,s], alternative = "less", h = 1, power = 2)
  dm_pv_mse2[s] <- dm_test_mse2$p.value
}

# --- PLOT MSEs

# MSE 'ANA'
plot(MSE_ana, type = "o", pch = 16, col = "gray30",
     cex = 0.8,
     ylim = c(0, 6000),
     lwd = 1,
     xlab = "Forecast step", 
     ylab = "MSE",
     main = "MSE Comparison",
     cex.main = 1.3, cex.lab = 1.2, cex.axis = 1)

# MSE 'MNM'
lines(MSE_mnm, type = "o",cex = 0.8, pch = 16, col = "gray60", lwd = 1, lty = 1)

# Grid
grid(col = "gray90", lty = "dotted")

# Legend
legend("topleft",
       legend = c("ANA", "MNM"), 
       col = c("gray30", "gray60"), 
       lty = c(1, 1),
       pch = c(16, 16),
       lwd = 1,
       bty = "n", 
       cex = 1.1,
       text.col = "black")

# --- 9) PLOT P-VALUE DM TEST ####
# Compare p_values of Diebold Mariano Test
plot(dm_pv_mse2, type = "p", col = "gray30", lwd = 0.5,
     xlab = "Forecast step", ylab = "p-value",
     main = "Diebold-Mariano test - 'MNM' vs 'ANA'")
abline(h = 0.05, col = "red", lty = 2, lwd = 2)

########## DYNAMIC REGRESSION ####
# --- 1) FIT LINEAR MODELS  ####
lm.1 <-lm(Total_load ~ Trend + Hour + Weekday + Month + Festivity , data= df_train)
# logarithmic trend
lm.2 <-lm(Total_load ~ Trend + I(log(Trend)) + Hour + Weekday + Month + Festivity , data= df_train)
# quadratic trend
lm.3 <- lm(Total_load ~ Trend + I(Trend^2) + Hour + Weekday + Month + Festivity , data= df_train)  
# interaction : weekday*festivity
lm.4 <-lm(Total_load ~ Trend + Hour + Weekday + Month + Festivity + Hour*Festivity , data= df_train)
# interaction : hour*weekday
lm.5 <-lm(Total_load ~ Trend + Hour + Weekday + Hour*Weekday + Month + Festivity, data= df_train)
# sin & cos month
lm.6 <- lm(Total_load ~ Trend + Hour + Weekday + Festivity + sin_month + cos_month , data= df_train)
# sin & cos hour
lm.7 <- lm(Total_load ~ Trend + Weekday + Month + Festivity + sin_hour + cos_hour, data= df_train)
# weekend
lm.8 <-lm(Total_load ~ Trend + Hour + Weekend + Month + Festivity , data= df_train)
# temperature
lm.9 <-lm(Total_load ~ Trend + Hour + Weekday + Month + Festivity + Temp, data= df_train) 

#number of model
k<-9
AIC_values <- numeric(k)
BIC_values <- numeric(k)

for (i in 1:k){
  model_name <- paste0("lm.", i)
  model_obj <- get(model_name)
  
  AIC_values[i] <- AIC(model_obj)
  BIC_values[i] <- BIC(model_obj)
}

# Create results table
results <- data.frame(
  Model = paste0("LM", 1:k),
  Description = c("Base", "Log Trend", "Quadratic Trend", "Hour*Festivity", 
                  "Hour*Weekday", "Sin/Cos Month", "Sin/Cos Hour", 
                  "Weekend","Temperature"),
  AIC = AIC_values,
  BIC = BIC_values
)

# Order by AIC for better visualization
results <- results[order(results$AIC), ]
results


# models 10,5,3,2 has lowest AIC

lm.1.1 <- lm(Total_load ~   Trend  + I(log(Trend)) + Hour + Weekday + Month + Festivity  + Temp, data= df_train) 

lm.1.2 <- lm(Total_load ~   Trend  + I(Trend^2) + Hour + Weekday + Month + Festivity  + Temp, data= df_train) 

lm.1.3 <- lm(Total_load ~   Trend  + Hour + Weekday + Hour*Weekday + Month + Festivity  + Temp, data= df_train)

lm.1.4 <- lm(Total_load ~   Trend  + Hour + Weekday + Month + Festivity + Hour*Festivity  + Temp, data= df_train) 

j<-4
AIC_values <- numeric(j)
BIC_values <- numeric(j)

for (i in 1:j){
  model_name <- paste0("lm.1.", i)
  model_obj <- get(model_name)
  
  AIC_values[i] <- AIC(model_obj)
  BIC_values[i] <- BIC(model_obj)
}

results <- data.frame(
  Model = paste0("LM", 1:j),
  AIC = AIC_values,
  BIC = BIC_values
)
results <- results[order(results$AIC), ]

results

# lm.1.3 and lm.1.2

lm.2.1 <- lm(Total_load ~ Trend + I(log(Trend)) + Hour + Weekday + Month + Festivity + Hour*Weekday +Hour*Festivity + Temp, data= df_train) 
summary(lm.2.1)
lm.2.2 <- lm(Total_load ~ Trend + I(Trend^2) + I(log(Trend)) + Hour + Hour*Weekday + Hour*Festivity +  Festivity + Weekday + Weekday*Festivity + Month + Temp, data= df_train)
summary(lm.2.2)

aic_2.1 <- AIC(lm.2.1)
aic_2.2 <- AIC(lm.2.2)
bic_2.1 <- BIC(lm.2.1)
bic_2.2 <- BIC(lm.2.2)

# Create results table
results_final <- data.frame(
  Model = c("LM2.1", "LM2.2"),
  AIC = c(aic_2.1, aic_2.2),
  BIC = c(bic_2.1, bic_2.2)
)

# Order by AIC
results_final <- results_final[order(results_final$AIC), ]
results_final

# --- 2) PRELIMINARY ANALYSIS OF RESIDUALS FOR THE BEST LM (2.2) ####
reg_residuals_ts <- lm.2.2$residuals

par(mfrow=c(1,2))
# PLOT RESIDUAL, HIST, ACF, PACF
plot(reg_residuals_ts, ylab = "Residuals")
hist(reg_residuals_ts, main = "", xlab = "Residuals")

acf(as.vector(reg_residuals_ts), lag.max = 200, main = "")
pacf(as.vector(reg_residuals_ts), lag.max = 200,, main = "")

# QQ PLOTS
par(mfrow=c(1,1))
qqnorm(scale(reg_residuals_ts), 
       main = "Q-Q plot of standardized residuals",
       xlab = "Theoretical quantile",
       ylab = "Sample quantile")
qqline(scale(reg_residuals_ts), col = "red", lwd = 2)


# --- 3) FIT ARIMAX MODELS  ####
## 2 MODELS (simple vs complex)

reg_residuals1 <- lm.9$residuals
reg_residuals2 <- lm.2.2$residuals

# list of order to test
orders <- list(
  c(1,0,1),
  c(1,0,2),
  c(1,0,3),
  c(2,0,2),
  c(2,0,1),
  c(3,0,1)
)

# Model's names
model_names <- c("ARIMA(1,0,1)", "ARIMA(1,0,2)", "ARIMA(1,0,3)",
                 "ARIMA(2,0,2)", "ARIMA(2,0,1)", "ARIMA(3,0,1)")

results_list <- list()

for (i in 1:2) {
  residual_name <- paste0("reg_residuals", i)
  reg_residuals <- get(residual_name)
  
  aic_vec <- numeric(length(orders))
  bic_vec <- numeric(length(orders))
  
  for (j in seq_along(orders)) {
    fit <- Arima(reg_residuals, order = orders[[j]])
    aic_vec[j] <- AIC(fit)
    bic_vec[j] <- BIC(fit)
  }
  
  results_list[[i]] <- data.frame(
    Modello = model_names,
    AIC = round(aic_vec, 2),
    BIC = round(bic_vec, 2)
  )
}

# Results lm.9
results_list[[1]]

# Results lm.2.2
results_list[[2]]

####
# --- 4) FIT SARIMAX MODELS  ####
reg_residuals1 <- lm.9$residuals
reg_residuals2 <- lm.2.2$residuals


# list of Sarimax order to test
sarimax_orders <- list(
  list(ar = c(1, 0, 3), seas = c(1, 0, 1)),
  list(ar = c(1, 0, 3), seas = c(1, 0, 2)),
  list(ar = c(1, 0, 3), seas = c(1, 0, 3)),
  list(ar = c(3, 0, 1), seas = c(1, 0, 1)),
  list(ar = c(3, 0, 1), seas = c(1, 0, 2))
)

# Name of the models
model_names <- c("SARIMAX(1,0,3)(1,0,1)[24]",
                 "SARIMAX(1,0,3)(1,0,2)[24]",
                 "SARIMAX(1,0,3)(1,0,3)[24]",
                 "SARIMAX(3,0,1)(1,0,1)[24]",
                 "SARIMAX(3,0,1)(1,0,2)[24]")

sarimax_results_list <- list()
sarimax_models_list <- list()

for (i in 1:2) {
  residual_name <- paste0("reg_residuals", i)
  reg_residuals <- get(residual_name)
  
  aic_vec <- numeric(length(sarimax_orders))
  bic_vec <- numeric(length(sarimax_orders))
  
  model_list_inner <- list()
  
  for (j in seq_along(sarimax_orders)) {
    ord <- sarimax_orders[[j]]
    fit <- Arima(reg_residuals,
                 order = ord$ar,
                 seasonal = list(order = ord$seas, period = 24))
    
    model_list_inner[[j]] <- fit
    
    aic_vec[j] <- AIC(fit)
    bic_vec[j] <- BIC(fit)
  }
  
  sarimax_models_list[[i]] <- model_list_inner
  sarimax_results_list[[i]] <- data.frame(
    Modello = model_names,
    AIC = round(aic_vec, 2),
    BIC = round(bic_vec, 2)
  )
}


# SARIMAX on residual's LM 2.1
sarimax_results_list[[1]][order(sarimax_results_list[[1]]$AIC), ]

# SARIMAX on residual's LM 2.2
sarimax_results_list[[2]][order(sarimax_results_list[[2]]$AIC), ]

# --- 5) RESIDUALS ANALYSIS ####

# check residuals after using SARIMAX
par(mfrow=c(1,2))
Sarimax_simple <- sarimax_models_list[[1]][[5]]
Sarimax_complex <- sarimax_models_list[[2]][[2]]
# SARIMAX on residual for Lm 9
acf(residuals(Sarimax_simple), main = "SARIMAX s(3,0,1)(1,0,2)[24]")
pacf(residuals(Sarimax_simple),main = "SARIMAX s(3,0,1)(1,0,2)[24]")

# plot residual
plot(residuals(Sarimax_simple), main = "", ylab = "")
abline(h = 0, col ="red")
#qqplot
qqnorm(scale(residuals(Sarimax_simple)), 
       main = "",
       xlab = "Theoretical quantile",
       ylab = "Sample quantile")
qqline(scale(residuals(Sarimax_simple)), col = "red", lwd = 2)


# SARIMAX on residual for Lm 2.2
acf(residuals(Sarimax_complex), main = "SARIMAX c(3,0,1)(1,0,2)[24]")
pacf(residuals(Sarimax_complex), main = "SARIMAX c(3,0,1)(1,0,2)[24]")

# plot residual
plot(residuals(Sarimax_complex), main = "", ylab = "")
abline(h = 0, col ="red")
#qqplot
qqnorm(scale(residuals(Sarimax_complex)), 
       main = "",
       xlab = "Theoretical quantile",
       ylab = "Sample quantile")
qqline(scale(residuals(Sarimax_complex)), col = "red", lwd = 2)

# reset par
par(mfrow=c(1,1))
########## ROLLING WINDOW ON VALIDATION USING SARIMAX on two LM : complex vs Linear ####

# --- 1) SETUP ####
window_size <-13140
start_index <- window_size + 1
end_index <- nrow(df_train_full) - h + 1
n.obs_need <- n.forecasts+h-1 #360

# --- 2) ROLLING FORECAST ####

# Inizialize variables
combined_forecast_c <- combined_forecast_s <- matrix(NA, nrow = n.forecasts, ncol = h)

for (i in 1:n.forecasts) {
  
  idx_start <- nrow(df_train_full) - n.obs_need - window_size + i
  idx_end <- idx_start + window_size 
  # 2023-08-14 11:00:00 - 2025-02-12 23:00:00
  train_data <- df_train_full[idx_start:(idx_end-1), ] # always 13140 (window_size)
  # 2025-02-13 00:00:00 - 2025-02-15 23:00:00
  future_data <- df_train_full[(idx_end):(idx_end+h-1), ] #always 72 step
  
  # Update every 25 iteration
  if (i == 1 || i %% 25 == 0) {
    
    # Fit linear model
    lm_simple <- lm(Total_load ~ Trend + Hour + Weekday + Month + Festivity + Temp, 
                    data = train_data) 
    reg_residuals_s <- residuals(lm_simple)
    
    lm_complex <- lm(Total_load ~ Trend + I(Trend^2) + I(log(Trend)) + Hour + Hour*Weekday + Hour*Festivity +  Festivity + Weekday + Weekday*Festivity + Month + Temp, 
                     data = train_data) 
    reg_residuals_c <- residuals(lm_complex)
    
    # Fit Sarimax on residuals
    sarimax_model_s <- Arima(reg_residuals_s, 
                             order = c(3, 0, 1),
                             seasonal = list(order = c(1, 0, 2), period = 24))
    
    sarimax_model_c <- Arima(reg_residuals_c, 
                             order = c(3, 0, 1),
                             seasonal = list(order = c(1, 0, 2), period = 24))
  }
  
  # Forecast regression part 
  reg_forecast_s <- predict(lm_simple, newdata = future_data)
  reg_forecast_c <- predict(lm_complex, newdata = future_data)
  
  # SARIMAX forecast 
  sarimax_forecast_s <- forecast(sarimax_model_s, h = h)
  sarimax_forecast_c <- forecast(sarimax_model_c, h = h)
  
  # Combine regression + ARIMA forecasts
  combined_forecast_s[i,] <- reg_forecast_s + sarimax_forecast_s$mean
  combined_forecast_c[i,] <- reg_forecast_c + sarimax_forecast_c$mean
  
}

# --- 3) PLOT : FORECAST ####
# --- Plot last 72 forecast

combined_forecast_s_zoo <- zoo(as.numeric(combined_forecast_s[289,]), order.by = index(tail(valid,72)))
combined_forecast_c_zoo <- zoo(as.numeric(combined_forecast_c[289,]), order.by = index(tail(valid,72)))

plot(tail(valid,72),
     main = "Sarimax forecast",
     ylim = c(500, 1300),
     ylab = "Energy Demand (MW)",
     col = "gray30",
     lwd = 2,
     cex.main = 1.3,
     cex.lab = 1.2,
     cex.axis = 1,
     lty = 1)

lines(combined_forecast_s_zoo, col = "blue", lwd = 2, 
      lty = 1,
      type = "l",
      pch = 18,
      cex = 1)
lines(combined_forecast_c_zoo, col = "darkgreen", lwd = 2, lty = 1,
      type = "l",
      pch = 16,
      cex = 1)

grid(col = "gray90", lty = "dotted")

legend("topleft",
       legend = c("SARIMAX simple", "SARIMAX complex"),
       col = c("blue","darkgreen"),
       lty = c(1, 1),
       lwd = 2,
       bty = "n",
       cex = 0.9,
       text.col = "black")

# --- 4) PLOT FORECAST ON ALL VALIDATION SET (each 72 step) ####
# Rows to plot:
rows_to_plot <- seq(289, 1, by = -72)

plot(valid,
     main = "Sarimax forecast",
     ylim = c(600, 1400),
     ylab = "Energy Demand (MW)",
     type = "l",
     col = "gray20",
     lwd = 2,
     xaxt = "n",
     xlab = "")

# add x ticks
axis.POSIXct(1, at = seq(start(valid), end(valid), by = "day"), 
             format = "%b %d", 
             tcl = -0.3,  
             mgp = c(3, 1, 0))

# Create time series every 72 days
for (i in 1:length(rows_to_plot)) {
  
  # Select row
  r <- rows_to_plot[i]
  
  # Assuming valid starts at a certain date and each row represents h=72 hours ahead
  hours_from_start <- (n.forecasts - r)
  forecast_start <- index(valid)[1] + hours_from_start * 3600
  time_index <- seq(from = forecast_start, by = "hour", length.out = 72)
  
  # Create zoo time series 
  combined_forecast_s_zoo <- zoo(combined_forecast_s[r, ], order.by = time_index)
  combined_forecast_c_zoo <- zoo(combined_forecast_c[r, ], order.by = time_index)
  
  # Add lines to plot
  lines(combined_forecast_s_zoo, col = "darkgreen", lwd = 1.5)
  lines(combined_forecast_c_zoo, col = "blue", lwd = 1.5)
}

legend("topleft",
       legend = c("SARIMAX simple", "SARIMAX complex"),
       col = c("darkgreen","blue"),
       lty = c(1, 1),
       lwd = 2,
       bty = "n",
       cex = 0.9,
       text.col = "black")

# --- 5) COMPARE MSEs ####
# Initialize result containers
mse_sarimax_s <- mse_sarimax_c <- dm_pv_mse <- numeric(h)

for (j in 1:h) {
  valid_y <- valid[j:(n.forecasts + j - 1)]
  
  # Compute MSE
  mse_sarimax_s[j] <- mean((combined_forecast_s[, j] - valid_y)^2)
  mse_sarimax_c[j] <- mean((combined_forecast_c[, j] - valid_y)^2)
  
  # DM test 
  e_sarimax_s <- valid_y - combined_forecast_s[, j]
  e_sarimax_c <- valid_y - combined_forecast_c[, j]
  dm_test_mse <- dm.test(e_sarimax_c, e_sarimax_s, alternative = "less", h = 1, power = 2)
  dm_pv_mse[j] <- dm_test_mse$p.value
}

plot(mse_sarimax_s, type = "o", pch = 16, col = "gray30",
     cex = 0.8,
     ylim = c(0,8000),
     lwd = 1,
     xlab = "Forecast step", 
     ylab = "MSE",
     main = "MSE Comparison",
     cex.main = 1.3, cex.lab = 1.2, cex.axis = 1)

lines(mse_sarimax_c, type = "o",cex = 0.8, pch = 16, col = "gray60", lwd = 1, lty = 1)

# grid
grid(col = "gray90", lty = "dotted")

# Legend
legend("bottomleft",
       legend = c("Sarimax simple", "Sarimax complex"), 
       col = c("gray30", "gray60"), 
       lty = c(1, 1),
       pch = c(16, 16),
       lwd = 1,
       bty = "n",  # senza bordo
       cex = 1.1,
       text.col = "black")

# --- 6) PLOT P-VALUE DM TEST ####
plot(dm_pv_mse, type = "p", col = "gray30", lwd = 0.5,
     xlab = "Forecast step", ylab = "p-value",
     main = "Diebold-Mariano test - SARIMAXc vs SARIMAXs")
abline(h = 0.05, col = "red", lty = 2, lwd = 2)

# --- 7) RESIDUALS ANALYSIS ####
par(mfrow=c(1,2))
# plot residual
plot(residuals(sarimax_model_c), main = "", ylab = "")
abline(h = 0, col ="red")
#qqplot
qqnorm(scale(residuals(sarimax_model_c)), 
       main = "",
       xlab = "Theoretical quantile",
       ylab = "Sample quantile")
qqline(scale(residuals(sarimax_model_c)), col = "red", lwd = 2)
# acf
acf(residuals(sarimax_model_c), main = "")
# pacf
pacf(residuals(sarimax_model_c),ylim = c(-0.05, 1),main = "")

# --- 8) PREDICTION INTERVAL SARIMAX (SIMPLEX LM) ####
# SET UP ####
# Take last iteration of Rolling window to compute prediction intervals
i<-289 
idx_start <- nrow(df_train_full) - n.obs_need - window_size + i
idx_end <- idx_start + window_size -1
# 2023-08-14 11:00:00 - 2025-02-12 23:00:00
train_data <- df_train_full[(idx_start):idx_end, ] # always 13140 (window_size)
# 2025-02-13 00:00:00 - 2025-02-15 23:00:00
future_data <- df_train_full[(idx_end+1):(idx_end+h), ] #always 72 step

# FIT MODEL ####
# fit complex linear model
lm_simple <- lm(Total_load ~ Trend + Hour + Weekday + Month + Festivity + Temp, 
                data = train_data)
# residuals
reg_residuals_s <- residuals(lm_simple)
# fit sarimax on residuals
sarimax_model_s <- Arima(reg_residuals_c, 
                         order = c(3, 0, 1),
                         seasonal = list(order = c(1, 0, 2), period = 24))
# forecast lm
reg_forecast <- predict(lm_simple, newdata = future_data)
# forecast sarimax
sarimax_forecast_s <- forecast(sarimax_model_s, h = h)

# Combine LM regression + ARIMA residual forecasts
combined_forecast1 <- reg_forecast + sarimax_forecast_s$mean

# BOOTSTRAPPING ####
B <- 1000
y_star <- matrix(NA, B, 72)
for (b in 1:B) {
  sarima_sim <- simulate(sarimax_model_s, nsim = 72)
  y_star[b, ] <- reg_forecast + sarima_sim
}
boot_ub_s <- apply(y_star, 2, quantile, 0.975)
boot_lb_s <- apply(y_star, 2, quantile, 0.025)
# set time of forecast
start_time <- as.POSIXct("2025-02-13 00:00:00")
time_index <- seq(from = start_time, by = "hour", length.out = 72)

# Create zoo objects for all forecast series
combined_forecast_zoo <- zoo(combined_forecast1, order.by = time_index)
boot_ub_zoo <- zoo(boot_ub_s, order.by = time_index)
boot_lb_zoo <- zoo(boot_lb_s, order.by = time_index)

# plot prediction interval
start_pred <- as.POSIXct("2025-02-13 01:00:00")
end_pred <- as.POSIXct("2025-02-16 00:00:00")
step_ahead72 <- window(zoo_hourly, start = start_pred, end = end_pred)

# PLOT ####
plot(step_ahead72, ylim = c(0, 1500), col = "grey10",
     main = "SARIMAX simple (3,0,1)(1,0,3)[24]", ylab = "Total Load", type = "l", lwd = 2,xlab="")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

# Add forecast line
lines(combined_forecast_zoo, col = "green", lwd = 2)

# Add bootstrap confidence intervals as lines
lines(boot_lb_zoo, col = "darkgreen", lty = 3)
lines(boot_ub_zoo, col = "darkgreen", lty = 3)

# Add polygon for shaded area 
time_coords <- as.numeric(time(boot_lb_zoo))
lb_values <- as.numeric(boot_lb_zoo)
ub_values <- as.numeric(boot_ub_zoo)

polygon(c(time_coords, rev(time_coords)), 
        c(lb_values, rev(ub_values)), 
        col = rgb(0, 1, 0, alpha = 0.2), border = NA)
# Legend
legend("topleft", legend = c("Forecast", "Bootstrap PI"), 
       col = c("green", "darkgreen"), lty = c(1, 3), bty = "n")

# --- 9) PREDICTION INTERVAL SARIMAX (COMPLEX LM) ####
# FIT MODEL ####
lm_complex <- lm(Total_load ~ Trend + I(Trend^2) + I(log(Trend)) + Hour + Hour*Weekday + Hour*Festivity +  Festivity + Weekday + Weekday*Festivity + Month + Temp, 
                 data = train_data) 
reg_residuals_c <- residuals(lm_complex)

sarimax_model_c <- Arima(reg_residuals_c, 
                         order = c(3, 0, 1),
                         seasonal = list(order = c(1, 0, 2), period = 24))

reg_forecast2 <- predict(lm_complex, newdata = future_data)

sarimax_forecast_c <- forecast(sarimax_model_c, h = h)

# Combine regression + ARIMA forecasts
combined_forecast2 <- reg_forecast2 + sarimax_forecast_c$mean

# BOOTSTRAPPING ####
B <- 1000
y_star <- matrix(NA, B, 72)
for (b in 1:B) {
  sarima_sim_c <- simulate(sarimax_model_c, nsim = 72)
  y_star[b, ] <- reg_forecast2 + sarima_sim_c
}
boot_ub_c <- apply(y_star, 2, quantile, 0.975)
boot_lb_c <- apply(y_star, 2, quantile, 0.025)
# set time of forecast
start_time <- as.POSIXct("2025-02-13 00:00:00")
time_index <- seq(from = start_time, by = "hour", length.out = 72)

# Create zoo objects for all forecast series
combined_forecast_zoo_c <- zoo(combined_forecast2, order.by = time_index)
boot_ub_zoo_c <- zoo(boot_ub_c, order.by = time_index)
boot_lb_zoo_c <- zoo(boot_lb_c, order.by = time_index)

# plot prediction interval
start_pred <- as.POSIXct("2025-02-13 01:00:00")
end_pred <- as.POSIXct("2025-02-16 00:00:00")
step_ahead72 <- window(zoo_hourly, start = start_pred, end = end_pred)

# PLOT ####
plot(step_ahead72, ylim = c(0, 1500), col = "grey10",
     main = "SARIMAX (3,0,1)(1,0,3)[24]", ylab = "Total Load", type = "l", lwd = 2,xlab="")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

# Add forecast line
lines(combined_forecast_zoo_c, col = "blue", lwd = 2)

# Add bootstrap confidence intervals as lines
lines(boot_lb_zoo_c, col = "darkblue", lty = 3)
lines(boot_ub_zoo_c, col = "darkblue", lty = 3)

# Add polygon for shaded area 
time_coords_c <- as.numeric(time(boot_lb_zoo_c))
lb_values_c <- as.numeric(boot_lb_zoo_c)
ub_values_c <- as.numeric(boot_ub_zoo_c)

polygon(c(time_coords_c, rev(time_coords_c)), 
        c(lb_values_c, rev(ub_values_c)), 
        col = rgb(0, 0, 1, alpha = 0.2), border = NA)
# Legend
legend("topleft", legend = c("Forecast", "Bootstrap PI"), 
       col = c("blue", "darkblue"), lty = c(1, 3), bty = "n")


########## BEST MODELS EVALUATION - PSEUDO OOS #####
# --- COMPARE SARIMAX (complex lm) VS ETS 'MNM'####
# --- 1) SETUP ####

# number of loop
n.forecast_final <- n.forecasts - h 
n.obs_need <- n.forecasts + h - 1

# Initialize matrices for forecasts
window_valid <- point_forecast_mnm <- matrix(NA, nrow = n.forecast_final, ncol = h)
combined_forecast <- combined_forecast_ub <- combined_forecast_lb <- mnm_ub <- mnm_lb <- matrix(NA, nrow = n.forecast_final, ncol = h)

# Initialize time for Seasonal naive
n.obs <- window_size + h + n.forecasts-1
roll_start_24 <- tail(ts_hourly_24,(n.obs))

# Initialize time for Sarimax
initial_idx_start <- nrow(data_hourly) - 2*h -window_size  
initial_idx_end <- initial_idx_start + window_size 
# From '2023-08-26 11:00:00' to '2025-02-24 23:00:00'
initial_train_data <- data_hourly[(initial_idx_start+1):(initial_idx_end), ]

# Sarimax estimation parameters ( outside rolling window )
initial_lm <- lm(Total_load ~ Trend + I(Trend^2) + I(log(Trend)) 
                 + Hour + Hour*Weekday + Hour*Festivity +  Festivity 
                 + Weekday + Weekday*Festivity + Month + Eval_Temp, 
                 data = initial_train_data)
sarimax_param <- Arima(residuals(initial_lm), 
                       order = c(3, 0, 1),
                       seasonal = list(order = c(1, 0, 2), period = 24))

# --- 2) ROLLING FORECAST ####
for (i in 1:n.forecast_final) {
  
  # Create Training window 
  train_start <- time(roll_start_24)[i]
  train_end <- time(roll_start_24)[i + window_size - 1]
  train_window <- window(roll_start_24, start = train_start, end = train_end)
  
  # ETS 'MNM' Forecast 
  ets_mnm <- ets(train_window, model = "MNM")
  forecast_ets <- forecast(ets_mnm, h = h)
  point_forecast_mnm[i,] <- forecast_ets$mean
  
  # prediction interval for ets 'MNM'
  mnm_lb[i,] <- forecast_ets$lower[,2]  
  mnm_ub[i,] <- forecast_ets$upper[,2]
  
  # SARIMAX Forecast 
  idx_start <- nrow(data_hourly) - window_size - n.obs_need + i
  idx_end <- idx_start + window_size
  
  # Generate train and future
  train_data <- data_hourly[idx_start:(idx_end-1), ]
  future_data <- data_hourly[(idx_end):(idx_end + h-1), ]
  
  # Fit linear model using predicted Temperature (see scritp Temperature)
  lm_model <- lm(Total_load ~ Trend + I(Trend^2) + I(log(Trend)) 
                 + Hour + Hour*Weekday + Hour*Festivity +  Festivity 
                 + Weekday + Weekday*Festivity + Month + Eval_Temp, 
                 data = train_data)
  
  # SARIMAX with fixed parameter
  sarimax_model <- Arima(residuals(lm_model), 
                         model = sarimax_param)
  
  # Cobined forecast
  reg_forecast <- predict(lm_model, newdata = future_data)
  sarimax_forecast <- forecast(sarimax_model, h = h)
  combined_forecast[i,] <- reg_forecast + sarimax_forecast$mean
  
  # Prediction Intervals for SARIMAX (assuming Gaussianity)
  # Calculate regression component variance
  X_test <- model.matrix(lm_model, data = future_data)
  reg_var <- diag(X_test %*% vcov(lm_model) %*% t(X_test))
  
  # Extract SARIMAX standard errors
  sarimax_se <- (sarimax_forecast$upper[,"95%"] - sarimax_forecast$mean) / qnorm(0.975)
  
  # Combined standard deviation
  total_sd <- sqrt(reg_var + sarimax_se^2)
  
  # Calculate prediction intervals
  combined_forecast_ub[i,] <- combined_forecast[i,] + qnorm(0.975) * total_sd
  combined_forecast_lb[i,] <- combined_forecast[i,] - qnorm(0.975) * total_sd
  
  print(i)
}

# --- 3) PLOT : FORECAST  ####

# convert in zoo object
combined_forecast_c_zoo <- zoo(as.numeric(combined_forecast[217,]), order.by = index(tail(test,72)))
point_forecast_mnm_zoo <- zoo(as.numeric(point_forecast_mnm[217,]), order.by = index(tail(test,72)))

# Plot for 72 step ahead
plot(tail(test,72),
     main = "Sarimax vs ETS 'ANA' forecast vs Seasonal naive",
     ylim = c(500, 1400),
     ylab = "Energy Demand (MW)",
     col = "gray10",
     lwd = 3,
     cex.main = 1.3,
     cex.lab = 1.2,
     cex.axis = 1,
     lty = 1)

lines(combined_forecast_c_zoo, col = "darkgreen", lwd = 2, lty = 1.5,
      type = "l",
      pch = 16,
      cex = 1)
lines(point_forecast_mnm_zoo, col = "blue", lwd = 2, lty = 1.5,
      type = "l",
      pch = 16,
      cex = 1)

grid(col = "gray90", lty = "dotted")

legend("topleft",
       legend = c("Sarimax [24]", "ETS 'MNM'"),
       col = c("darkgreen","blue"),
       lty = c(1, 1),
       lwd = 2,
       bty = "n",
       cex = 0.9,
       text.col = "black")

# --- 4) PLOT FORECAST ON ALL TEST SET (each 72 step) ####
# Rows to plot:
rows_to_plot <- seq(217, 1, by = -72)

# Plot test set
plot(test,
     main = "Sarimax vs ETS 'ANA'",
     ylim = c(600, 1400),
     ylab = "Energy Demand (MW)",
     type = "l",
     col = "gray10",
     lwd = 2,
     xaxt = "n",
     xlab = "")

# add x labels
axis.POSIXct(1, at = seq(start(test), end(test), by = "day"), 
             format = "%b %d", 
             tcl = -0.3,  
             mgp = c(3, 1, 0))

# Create time series every 72 days
for (i in 1:length(rows_to_plot)) {
  
  # Select row
  r <- rows_to_plot[i]
  
  # Assuming test starts at a certain date and each row represents h=72 hours ahead
  hours_from_start <- (n.forecast_final - r)
  forecast_start <- index(test)[1] + hours_from_start * 3600
  time_index <- seq(from = forecast_start, by = "hour", length.out = 72)
  
  # Create zoo time series 
  point_forecast_mnm_zoo <- zoo(point_forecast_mnm[r, ], order.by = time_index)
  combined_forecast_c_zoo <- zoo(combined_forecast[r, ], order.by = time_index)
  
  # Add lines to plot
  lines(point_forecast_mnm_zoo, col = "blue", lwd = 1.5)
  lines(combined_forecast_c_zoo, col = "darkgreen", lwd = 1.5)
}

legend("topleft",
       legend = c("SARIMAX[24]", "ETS 'ANA'"),
       col = c("darkgreen","blue"),
       lty = c(1, 1, 1),
       lwd = 2,
       bty = "n",
       cex = 0.9,
       text.col = "black")

# --- 5) Create function for MSE , UC, WINKLER ####
mse <- function(x,y) mean((x - y)^2)

uc <- function(l, u, y) mean((y >= l) & (y <= u))

winkler <- function(l, u, y, alpha = 0.05) {
  n <- length(y)
  score <- numeric(n)
  for (i in 1:n) {
    range <- u[i] - l[i]
    if (y[i] < l[i]) {
      score[i] <- range + (2 / alpha) * (l[i] - y[i])
    } else if (y[i] > u[i]) {
      score[i] <- range + (2 / alpha) * (y[i] - u[i])
    } else {
      score[i] <- range
    }
  }
  return(score)
}

dm.winkler.test <- function(e1, e2){
  loss_diff <- e1 - e2
  dm_statistic <- mean(loss_diff) / (sd(loss_diff) / sqrt(length(loss_diff)))
  return(dm_statistic)
}

# --- 6) COMPARE MSEs ####

# Initialize results containers
mse_sarimax <- mse_mnm <- dm_pv_mse1 <- dm_pv_mse2 <- numeric(h)

for (j in 1:h) {
  test_y <- test[j:(n.forecast_final + j - 1)]
  
  # MSE SARIMAX
  mse_sarimax[j] <- mse(combined_forecast[,j], test_y)
  
  # MSE ets 'MNM'
  mse_mnm[j] <- mse(point_forecast_mnm[,j], test_y)
  
  # Compute residuals for : SARIMAX, 'MNM'
  e_sarimax <- test_y - combined_forecast[,j]
  e_mnm <- test_y - point_forecast_mnm[,j]
  
  # Diebold-Mariano Test : Residuals Sarimax vs 'MNM'
  dm_test_mse1 <- dm.test(e_mnm,e_sarimax, alternative = "greater", h = 1, power = 2)
  dm_pv_mse1[j] <- dm_test_mse1$p.value
  
}

# PLOT MSEs

plot(mse_sarimax, type = "o", pch = 16, col = "gray20",
     cex = 0.8,
     ylim = c(0,19000),
     lwd = 1,
     xlab = "Forecast step", 
     ylab = "MSE",
     main = "MSE Comparison",
     cex.main = 1.3, cex.lab = 1.2, cex.axis = 1)

lines(mse_mnm, type = "o",cex = 0.8, pch = 16, col = "gray40", lwd = 1, lty = 1)
# grid
grid(col = "gray90", lty = "dotted")

# Legend
legend("topleft",
       legend = c("Sarimax [24]", "'MNM'"), 
       col = c("gray20", "gray60"), 
       lty = c(1, 1),
       pch = c(16, 16),
       lwd = 1,
       bty = "n",
       cex = 1.1,
       text.col = "black")

# --- 7) PLOT P-VALUE DM TEST ####
plot(dm_pv_mse1, type = "p", col = "gray30", lwd = 0.5,
     xlab = "Forecast step", ylab = "p-value", ylim =c(0,0.06),
     main = "DM test with MSE - SARIMAX vs ets 'MNM' ")
abline(h = 0.05, col = "red", lty = 2, lwd = 2)


# --- 8) RESIDUALS ANALYSIS ####
par(mfrow=c(1,2))
# plot residual
plot(residuals(sarimax_model), main = "", ylab = "")
abline(h = 0, col ="red")
#qqplot
qqnorm(scale(residuals(sarimax_model)), 
       main = "",
       xlab = "Theoretical quantile",
       ylab = "Sample quantile")
qqline(scale(e_sarimax), col = "red", lwd = 2)
# acf
acf(residuals(sarimax_model), main = "")
# pacf
pacf(residuals(sarimax_model),ylim = c(-0.05, 1),main = "")

#reset
par(mfrow=c(1,1))

# --- 9) UNCONDITIONAL COVERAGE & WINKLER FUNCTION ####

# Initialize results containers
uc_sarimax <- uc_mnm <- numeric(h)
uc_pv_sarimax <-uc_pv_mnm <- numeric(h)
winkler_sarimax <- winkler_mnm <- dm_winkler <- numeric(h)

# Loop to compute winkler & unconditional 
for (j in 1:h) {
  test_y <- test[j:(n.forecast_final + j - 1)]
  
  # Unconditional coverage
  uc_sarimax[j] <- uc(combined_forecast_lb[,j], combined_forecast_ub[,j], test_y)
  uc_mnm[j] <- uc(mnm_lb[,j], mnm_ub[,j], test_y)
  
  # binomial test for uc
  uc_pv_sarimax[j] <- binom.test(uc_sarimax[j]*n.forecast_final, n.forecast_final, p = 0.95)$p.value
  uc_pv_mnm[j] <- binom.test(uc_mnm[j]*n.forecast_final, n.forecast_final, p = 0.95)$p.value
  
  # Winkler loss
  w_sarimax <- winkler(combined_forecast_lb[,j], combined_forecast_ub[,j], test_y, 0.05)
  w_mnm <- winkler(mnm_lb[,j],  mnm_ub[,j], test_y, 0.05)
  
  winkler_sarimax[j] <- mean(w_sarimax)
  winkler_mnm[j] <- mean(w_mnm)
  
  # DM test on interval score
  dm_winkler[j] <- dm.winkler.test(w_mnm ,w_sarimax )
  
}

# --- 10) PLOT : unconditional coverage ####
plot(uc_sarimax, type = "o", pch = 16, col = "gray20",
     cex = 0.8,
     ylim = c(0,1),
     lwd = 1,
     xlab = "Forecast step", 
     ylab = "Unconditional Coverage",
     main = "UC Comparison",
     cex.main = 1.3, cex.lab = 1.2, cex.axis = 1)

lines(uc_mnm, type = "o",cex = 0.8, pch = 16, col = "gray60", lwd = 1, lty = 1)

# grid
grid(col = "gray90", lty = "dotted")

# Legend
legend("bottomleft",
       legend = c("Sarimax [24]", "'MNM'"), 
       col = c("gray20", "gray40"), 
       lty = c(1, 1),
       pch = c(16, 16),
       lwd = 1,
       bty = "n",
       cex = 1.1,
       text.col = "black")             


# --- 11) PLOT : Winkler Loss ####

plot(winkler_sarimax, type = "o", pch = 16, col = "gray20",
     cex = 0.8,
     ylim = c(0,1000),
     lwd = 1,
     xlab = "Forecast step", 
     ylab = "Winkler score",
     main = "Winkler Loss function Comparison",
     cex.main = 1.3, cex.lab = 1.2, cex.axis = 1)

lines(winkler_mnm, type = "o",cex = 0.8, pch = 16, col = "gray60", lwd = 1, lty = 1)

# grid
grid(col = "gray90", lty = "dotted")

# Legend
legend("topleft",
       legend = c("Sarimax [24]", "'MNM'"), 
       col = c("gray20", "gray60"), 
       lty = c(1, 1),
       pch = c(16, 16),
       lwd = 1,
       bty = "n",
       cex = 1.1,
       text.col = "black")

# --- 12) DM test on interval score ####
plot(dm_winkler, type = "p", col = "gray30", lwd = 0.5,
     xlab = "Forecast step", ylab = "p-value",
     main = "DM test with Winkler LF - Sarimax vs ets 'MNM'")
abline(h = 0.05, col = "red", lty = 2, lwd = 2)


########## FORECAST OUT OF SAMPLE (using predicted temperature) ####
# --- FORECAST OOS : 28 Feb - 2 Mar ####
# Create future set of data (for forecast)
idx_start <- nrow(df_full) - n.obs_need - window_size + n.forecasts -1
idx_end <- idx_start + window_size 
# From 2025-02-25 00:00:00 to 2025-02-27 23:00:00
future_data <- df_full[(idx_end+1):(idx_end+h), ] # always 72 (step we want to predict)

# Fit best LM
best_lm <- lm(Total_load ~ Trend + I(Trend^2) + I(log(Trend)) + Hour + Hour*Weekday + Hour*Festivity +  Festivity + Weekday + Weekday*Festivity + Month + Temp, 
              data = df_full) 
reg_res <- residuals(best_lm)
# Fit best Sarimax (3,0,1)(1,0,2)[24] on residuals of best LM
best_sarimax <- Arima(reg_res,
                      order = c(3, 0, 1),
                      seasonal = list(order = c(1, 0, 2), period = 24))
# best LM predictions
reg_forecast <- predict(best_lm, newdata = future_data)

# best Sarimax predictions
best_sarimax_forecast <- forecast(best_sarimax, h = h)

# Combine regression + SARIMAX forecasts
best_combined_forecast <- reg_forecast + best_sarimax_forecast$mean                   

# --- PREDICTION INTERVALS ####
B <- 1000
y_star <- matrix(NA, B, 72)
for (b in 1:B) {
  sarima_sim <- simulate(best_sarimax, nsim = 72)
  y_star[b, ] <- reg_forecast + sarima_sim
}
boot_ub <- apply(y_star, 2, quantile, 0.975)
boot_lb <- apply(y_star, 2, quantile, 0.025)
# set time of forecast
start_time <- as.POSIXct("2025-02-28 00:00:00")
time_index <- seq(from = start_time, by = "hour", length.out = 72)

# Create zoo objects for all forecast series
combined_forecast_zoo <- zoo(best_combined_forecast, order.by = time_index)
boot_ub_zoo <- zoo(gauss_ub, order.by = time_index)
boot_lb_zoo <- zoo(gauss_lb, order.by = time_index)

# plot prediction interval
start_pred <- as.POSIXct("2025-02-28 00:00:00")
end_pred <- as.POSIXct("2025-03-02 23:00:00")

step_ahead72 <- window(zoo_hourly, start = start_pred, end = end_pred)
# Plot
plot(step_ahead72, ylim = c(0, 1500), col = "grey10",
     main = "SARIMAX (3,0,1)(1,0,3)[24]", ylab = "Total Load", type = "l", lwd = 2,xlab="")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

# Add forecast line
lines(combined_forecast_zoo, col = "blue", lwd = 2)

# Add bootstrap confidence intervals as lines
lines(boot_lb_zoo, col = "darkblue", lty = 3)
lines(boot_ub_zoo, col = "darkblue", lty = 3)

# Add polygon for shaded area 
time_coords <- as.numeric(time(boot_lb_zoo))
lb_values <- as.numeric(boot_lb_zoo)
ub_values <- as.numeric(boot_ub_zoo)

polygon(c(time_coords, rev(time_coords)), 
        c(lb_values, rev(ub_values)), 
        col = rgb(0, 0, 1, alpha = 0.2), border = NA)
# Legend
legend("topleft", legend = c("Forecast", "BOOTSTRAP PI"), 
       col = c("blue", "darkblue"), lty = c(1, 3), bty = "n")

#### CONCLUSIONS ####
# The lowest value identified in the 72 hous of out of sample forecast is : 653.9332
# Date of minumum demand of energy among 72 forecast
date_min_forecast <-  index(combined_forecast_zoo)[which.min(combined_forecast_zoo)]
print(date_min_forecast)
# last 72 obs
last_72_obs <- tail(zoo_hourly, 72)
# min value of 72 obs
min_value <- min(last_72_obs, na.rm = TRUE)
min_value
# data of min value pred
data_min_pred <- as.POSIXct("2025-02-28 03:00:00")
# value of date with min value pred
value_date <- as.numeric(zoo_hourly[data_min_pred])
value_date
# diff 
diff <- value_date - min_value
diff
# perc diff
percent_diff <- round(((value_date - min_value)/value_date)*100,2)
percent_diff