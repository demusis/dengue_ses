# Bibliotecas
library(tidyr)
library(dplyr)
library(forecast)
library(imputeTS)
library(lubridate)
library(readr)
library(ggplot2)
library(tsibble) # para validação cruzada
library(fUnitRoots)
library(tseries)

# Carregar os dados
dengue_data <- read_csv("dados_semana.csv", na = c("-"))
dengue_long <- dengue_data %>%
  gather(key = "semana_epidemiologica", value = "y", -Ano) %>%
  mutate(semana_epidemiologica = as.numeric(gsub("\\D", "", semana_epidemiologica)),
         primeiro_dia_ano = as.Date(paste(Ano, "-01-01", sep = "")),
         ds = primeiro_dia_ano + days((semana_epidemiologica - 1) * 7)) %>%
  select(ds, y) %>%
  arrange(ds)

# Converter para série temporal
dengue_ts <- ts(dengue_long$y, start = c(year(min(dengue_long$ds)), week(min(dengue_long$ds))), frequency = 52)

# Testes da estacionariedade da série
# KPSS
urkpssTest(dengue_ts, 
           type = c("tau"), 
           lags = c("short"),
           use.lag = NULL, 
           doplot = TRUE)

# Preencher falhas com o Filtro de Kalman
y <- na.kalman(dengue_long$y)
y_ts <- ts(y, start = c(year(min(dengue_long$ds)), week(min(dengue_long$ds))), frequency = 52)

# Dickey-Fuller
adf.test(y_ts, alternative="stationary")

# Ajustar um modelo ARIMA
fit_arima <- auto.arima(dengue_ts,
                        approximation = FALSE,
                        allowdrift = TRUE,
                        allowmean = TRUE,
                        seasonal = TRUE,
                        stepwise = TRUE,
                        lambda="auto", 
                        trace = TRUE,
                        ic = c("aic", "aicc"))
residuos_arima <- residuals(fit_arima)

plot(residuos_arima)

# Diagnóstico dos resíduos do ARIMA
Box.test(residuos_arima, lag = 20, type = "Ljung-Box")

# Previsões para os próximos 3 meses
forecast_future <- forecast(fit_arima, h = 12)
autoplot(forecast_future, xlab = 'Data decimal', ylab = 'Consumo mensal')

# Converter para tsibble
dengue_tbl <- as_tsibble(dengue_long, index = ds)

# Dados para plotagem
observed <- dengue_long %>% filter(ds <= max(dengue_tbl$ds))
fitted_values <- data.frame(ds = dengue_long$ds, y = fitted(fit_arima))
future_forecast <- data.frame(ds = seq(max(dengue_long$ds) + 1, by = "week", length.out = 12),
                              y = forecast_future$mean)

# Gráfico
ggplot() +
  geom_line(data = observed, aes(x = ds, y = y), color = "blue") +
  geom_line(data = fitted_values, aes(x = ds, y = y), color = "red") +
  geom_line(data = future_forecast, aes(x = ds, y = y), color = "green") +
  labs(title = "Dengue: Valores Observados, Estimados e Previsões",
       x = "Data", y = "Casos de Dengue") +
  theme_minimal()

#
# Validação cruzada
#

# Definir a função para o modelo ARIMA
arima_forecast <- function(y, h) {
  forecast(
    fit_arima <- auto.arima(y,
                            approximation = FALSE,
                            allowdrift = TRUE,
                            allowmean = TRUE,
                            seasonal = TRUE,
                            stepwise = TRUE,
                            lambda="auto", 
                            # trace = TRUE,
                            ic = c("aicc")),
    h = h)
}

# Validar cruzadamente o modelo ARIMA (12 semanas)
erro_cv <- tsCV(dengue_ts, arima_forecast, h = 12)
mae_cv <- mean(abs(erro_cv), na.rm = TRUE) # Média do erro absoluto
mae_cv
erro_cv
