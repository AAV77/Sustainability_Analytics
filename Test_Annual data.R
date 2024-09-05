# Load necessary libraries
# Load the required packages
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(tseries)
library(rugarch)

# Preparation ------------------------------------------------------------------

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #setting directory

# Load the datasets
energy_production <- read_csv("ogd104_stromproduktion_swissgrid.csv")
energy_prices <- read_csv("ogd106_preise_strom_boerse.csv")

# Reshape the energy production data to have one column per energy source
energy_production <- energy_production %>%
  pivot_wider(names_from = Energietraeger, values_from = Produktion_GWh, values_fill = 0)

# View the first few rows of each dataset
head(energy_production)
head(energy_prices)

# Merge the reshaped energy production data with the energy prices data
d.d <- merge(energy_prices, energy_production, by="Datum", all.x=TRUE)

# Fill any remaining NAs with 0 (in case there were dates in energy_prices not in energy_production)
d.d[is.na(d.d)] <- 0

# View the combined data
head(d.d)

# Time series data prep --------------------------------------------------------

# Create a time series for Baseload prices
ts_baseload_prices <- ts(d.d$Baseload_EUR_MWh, start=c(2017, 1), frequency=365)

# Create a time series for Kernkraft production
ts_kernkraft <- ts(d.d$Kernkraft, start=c(2017, 1), frequency=365)

# Create a time series for Kernkraft production
ts_kernkraft <- ts(d.d$Kernkraft, start=c(2017, 1), frequency=365)

# Create a time series for Photovoltaik production
ts_photovoltaik <- ts(d.d$Photovoltaik, start=c(2017, 1), frequency=365)

# Create a time series for Photovoltaik production
ts_thermische <- ts(d.d$Thermische, start=c(2017, 1), frequency=365)

# Plot ts data -----------------------------------------------------------------

# Plot Baseload prices
plot(ts_baseload_prices, main="Baseload Energy Prices", 
     ylab="EUR/MWh", 
     xlab="Year", 
     col="blue")

# Plot Kernkraft production
plot(ts_kernkraft, main="Kernkraft Production", 
     ylab="GWh", 
     xlab="Year", 
     col="darkgreen")

# Plot Photovoltaik production
plot(ts_photovoltaik, main="Photovoltaik Production", 
     ylab="GWh", 
     xlab="Year", 
     col="orange")

# Plot Thermische production
plot(ts_thermische, main="Thermische Production", 
     ylab="GWh", 
     xlab="Year", 
     col="purple")

# Decomposition ----------------------------------------------------------------

# Decompose Baseload prices time series
decomp_baseload_prices <- decompose(ts_baseload_prices)
plot(decomp_baseload_prices)

# Decompose Photovoltaik
decomp_photovoltaik <- decompose(ts_photovoltaik)
plot(decomp_photovoltaik)

# Decompose Thermische
decomp_thermische <- decompose(ts_thermische)
plot(decomp_thermische)

# 1. Baseload ##################################################################
# 1.1. Stationarity ------------------------------------------------------------

# First difference to remove the trend
diff_baseload_prices <- diff(ts_baseload_prices, differences = 1)

# Plot the differenced series
plot(diff_baseload_prices, main="Differenced Baseload Energy Prices", 
     ylab="Differenced EUR/MWh", xlab="Year", col="blue")
# --> Not successful - let's do a log-transformation

# Apply log transformation to the original series
log_baseload_prices <- log(ts_baseload_prices)

# Difference the log-transformed series
diff_log_baseload_prices <- diff(log_baseload_prices, differences = 1)

# Plot the differenced log-transformed series
plot(diff_log_baseload_prices, main="Differenced Log-Transformed Baseload Energy Prices", 
     ylab="Differenced Log(EUR/MWh)", xlab="Year", col="blue")
# --> Still not successful - let's do a Box-Cox lambda

# Find the optimal Box-Cox lambda
lambda <- BoxCox.lambda(ts_baseload_prices)

# Apply the Box-Cox transformation
boxcox_baseload_prices <- BoxCox(ts_baseload_prices, lambda)

# Difference the Box-Cox transformed series
diff_boxcox_baseload_prices <- diff(boxcox_baseload_prices, differences = 1)

# Plot the differenced Box-Cox transformed series
plot(diff_boxcox_baseload_prices, main="Differenced Box-Cox Transformed Baseload Energy Prices", 
     ylab="Differenced Box-Cox(EUR/MWh)", xlab="Year", col="blue")

# 1.2. GARCH Model for Baseload ------------------------------------------------

# Specify the GARCH model (start with a simple GARCH(1,1))
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 0)),
                   distribution.model = "norm")

# Fit the GARCH model
garch_fit <- ugarchfit(spec = spec, data = diff_log_baseload_prices)

# Summary of the GARCH model
summary(garch_fit)

# Plot the fitted volatility
plot(garch_fit, which = 2)


