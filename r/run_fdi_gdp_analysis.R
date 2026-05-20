library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(vars)

data_path <- file.path("data", "fdi_gdp_sample.csv")
df <- read_csv(data_path, show_col_types = FALSE)

ts_fdi <- ts(df$fdi_billion_usd, start = min(df$year), frequency = 1)
ts_gdp <- ts(df$gdp_billion_usd, start = min(df$year), frequency = 1)

fdi_model <- auto.arima(ts_fdi)
gdp_model <- auto.arima(ts_gdp)

combined <- df %>%
  select(fdi_billion_usd, gdp_billion_usd)

var_model <- VAR(combined, p = 2, type = "const")

cat("FDI ARIMA model:\\n")
print(summary(fdi_model))
cat("\\nGDP ARIMA model:\\n")
print(summary(gdp_model))
cat("\\nVAR model:\\n")
print(summary(var_model))

forecast_fdi <- forecast(fdi_model, h = 5)
forecast_gdp <- forecast(gdp_model, h = 5)

outputs <- data.frame(
  year = (max(df$year) + 1):(max(df$year) + 5),
  fdi_forecast = as.numeric(forecast_fdi$mean),
  gdp_forecast = as.numeric(forecast_gdp$mean)
)

dir.create("outputs", showWarnings = FALSE)
write_csv(outputs, file.path("outputs", "fdi_gdp_forecast.csv"))
