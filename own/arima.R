## Simulating longitudinal data

library(forecast)
library(tidyverse)
library(ggplot2)

arima_model <- simulate(
  Arima(ts(start = 0, end = 18))
)