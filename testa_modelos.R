library(dplyr)
library(forecast)
library(lubridate)
library(readr)
library(readxl)
library(tidyr)

dados <- read_excel("inmet_cuiaba.xlsx")
dados <- dados[,-2]
p_corte <- 0

# Converte a coluna 'data' para o formato de data
dados$data <- as.Date(dados$data)

# Calcula ano e semana epidemiológica
dados$ano <- year(dados$data)
dados$semana <- sapply(dados$data, function(data) {
  as.integer(format(data, "%W")) + 1
})

# Calcula o percentual de dados válidos
percentual_dados_validos <- dados %>%
  group_by(ano, semana) %>%
  summarise_all(function(x) sum(!is.na(x)) / n() * 100)

# Junta os dataframes para filtrar os dados originais com base no percentual de dados válidos
dados_com_percentual <- inner_join(dados, percentual_dados_validos, by = c("ano", "semana"))

# Filtra para manter apenas os grupos com mais de p_corte de dados válidos
dados_filtrados <- dados_com_percentual %>%
  filter(if_all(starts_with("x"), ~ . > p_corte)) # Substitua "x" pelo prefixo das colunas de percentual no dataframe

# Calcula a média para os dados filtrados
dados_media <- dados_filtrados %>%
  group_by(ano, semana) %>%
  summarise_all(mean, na.rm = TRUE)

# Carregar os dados
dengue_data <- read_csv("dados_semana.csv", na = c("-"))
dengue_long <- dengue_data %>%
  gather(key = "semana_epidemiologica", value = "y", -Ano) %>%
  mutate(semana_epidemiologica = as.numeric(gsub("\\D", "", semana_epidemiologica)),
         primeiro_dia_ano = as.Date(paste(Ano, "-01-01", sep = "")),
         ds = primeiro_dia_ano + days((semana_epidemiologica - 1) * 7)) %>%
  select(ds, y) %>%
  arrange(ds)

# Ano e a semana do dataframe dengue_long
dengue_long$ano <- year(dengue_long$ds)
dengue_long$semana <- sapply(dengue_long$ds, function(data) {
  as.integer(format(data, "%W")) + 1
})

dengue_dados_combinados <- inner_join(dengue_long, dados_media, by = c("ano", "semana"))

df_exogeno <- dengue_dados_combinados[, c(6, 7, 10, 11, 19)]

# Converter para série temporal
dengue_ts <- ts(dengue_dados_combinados$y, 
                start = c(year(min(dengue_dados_combinados$ds)), 
                          week(min(dengue_dados_combinados$ds))), 
                frequency = 52)

# Supondo que dengue_ts é sua série temporal e df_exogeno contém as variáveis exógenas
results <- data.frame(Combination = character(), AIC = numeric(), stringsAsFactors = FALSE)

# Ajustar um modelo ARIMA sem variáveis exógenas
model_puro <- auto.arima(dengue_ts,
                         approximation = FALSE,
                         allowdrift = TRUE,
                         allowmean = TRUE,
                         seasonal = TRUE,
                         stepwise = TRUE,
                         lambda="auto", 
                         # trace = TRUE,
                         ic = c("aic"))

aic_puro <- AIC(model_puro)
results <- rbind(results, data.frame(Combination = "Sem Variáveis Exógenas", AIC = aic_puro))

# Obter todas as combinações possíveis de variáveis exógenas
for (i in 1:ncol(df_exogeno)) {
  comb <- combn(ncol(df_exogeno), i, simplify = FALSE)
  for (vars in comb) {
    exog_data <- as.matrix(df_exogeno[, vars, drop = FALSE])
    # model <- auto.arima(dengue_ts, xreg = exog_data)
    
    model <- auto.arima(dengue_ts,
                        xreg = exog_data,
                        approximation = FALSE,
                        allowdrift = TRUE,
                        allowmean = TRUE,
                        seasonal = TRUE,
                        stepwise = TRUE,
                        lambda="auto", 
                        # trace = TRUE,
                        ic = c("aic"))
    
    aic_value <- AIC(model)
    combination_name <- paste(names(df_exogeno)[vars], collapse = "+")
    results <- rbind(results, data.frame(Combination = combination_name, AIC = aic_value))
  }
}

# Ordenar os resultados pelo AIC
results <- results %>% arrange(AIC)
write.csv(results, "resultados.csv")

# Melhor modelo
df_exogeno <- dengue_dados_combinados[, c(6, 7, 11)]
exog_data <- as.matrix(df_exogeno[, 1:3, drop = FALSE])

modelo <- auto.arima(dengue_ts,
                    xreg = exog_data,
                    approximation = FALSE,
                    allowdrift = TRUE,
                    allowmean = TRUE,
                    seasonal = TRUE,
                    stepwise = TRUE,
                    lambda="auto", 
                    # trace = TRUE,
                    ic = c("aic"))

# Gerar previsões com base no modelo ajustado
previsoes <- forecast(modelo, xreg = exog_data, h = 0)

# Plotar os dados originais
plot(modelo$x, type = "l", col = "blue", xlab = "Tempo", ylab = "Valores", main = "Série Temporal de Dengue")

# Adicionar a série ajustada pelo modelo
lines(modelo$fitted, col = "red")

# Legenda
legend("topright", legend = c("Dados Originais", "Estimativa do Modelo"), col = c("blue", "red"), lty = 1)
