# Carregar as bibliotecas necessárias
library(readr)
library(dplyr)
library(tidyr)

# Definir o diretório de trabalho
setwd("G:/Meu Drive/SES/2024/dengue")

# Ler o arquivo CSV
sinan_dengue <- read_csv("SINAN_DENGUE.csv", 
                         col_types = cols(DiaSemanaNotificacao = col_integer()))

# Filtrar para o município de Cuiabá
sinan_dengue_cuiaba <- filter(sinan_dengue, MunicipioResidencia == "CUIABA")

# Agrupar por Ano e Dia da Semana e contar os casos
dados_agregados <- summarise(group_by(sinan_dengue_cuiaba, AnoNotificacao, DiaSemanaNotificacao),
                             Contagem = n(), 
                             .groups = "drop")

# Completar as semanas que não têm registros com 0
dados_completos <- complete(dados_agregados, 
                            AnoNotificacao, 
                            DiaSemanaNotificacao = 1:53, 
                            fill = list(Contagem = 0))

# Ordenar o resultado por Ano e SemanaNotificacao
dados_completos <- arrange(dados_completos, AnoNotificacao, DiaSemanaNotificacao)

# Converter o dataframe em série temporal
numero_casos_ts <- ts(dados_completos$Contagem, 
                      start = c(min(dados_completos$AnoNotificacao), min(dados_completos$DiaSemanaNotificacao)), 
                      frequency = 53)  # 53 semanas por ano
plot(numero_casos_ts)

modelo <- auto.arima(numero_casos_ts,
                     approximation = FALSE,
                     allowdrift = TRUE,
                     allowmean = TRUE,
                     seasonal = TRUE,
                     stepwise = TRUE,
                     lambda="auto", 
                     # trace = TRUE,
                     ic = c("aic"))
print(modelo)

# Gerar previsões com base no modelo ajustado
previsoes <- forecast(modelo, h = 3)
autoplot(previsoes)

