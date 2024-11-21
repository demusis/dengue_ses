library(readr)
library(tidyverse)
library(factoextra)

# Definir o diretório de trabalho
setwd("G:/Meu Drive/SES/2024/dengue")

# Carregar o arquivo CSV
dados <- read_csv("SINAN_DENGUE.csv", 
                  col_types = cols(DiaSemanaNotificacao = col_integer()))

# Selecionar colunas de interesse
dados_selecionados <- dados %>% select(AnoNotificacao, SexoPaciente, FaixaEtaria, Escolaridade, ZonaResidencia)

# Tratar valores ausentes (substituir por 'Desconhecido')
dados_selecionados[is.na(dados_selecionados)] <- 'Desconhecido'

# Codificar variáveis categóricas para numéricas
dados_selecionados$SexoPaciente <- as.numeric(factor(dados_selecionados$SexoPaciente))
dados_selecionados$FaixaEtaria <- as.numeric(factor(dados_selecionados$FaixaEtaria))
dados_selecionados$Escolaridade <- as.numeric(factor(dados_selecionados$Escolaridade))
dados_selecionados$ZonaResidencia <- as.numeric(factor(dados_selecionados$ZonaResidencia))

# Aplicar PCA (Análise de Componentes Principais)
acp <- prcomp(dados_selecionados, center = TRUE, scale. = TRUE)

# Reduzir para as duas primeiras componentes
dados_acp <- as.data.frame(acp$x[, 1:2])

# Função para calcular a soma dos quadrados dentro dos clusters (WSS) para cada valor de k
wss_values <- function(k) {
  kmeans(dados_acp, k, nstart = 25)$tot.withinss
}

# Calcular WSS para diferentes valores de k
k_values <- 1:10
wss <- map_dbl(k_values, wss_values)

# Encontrar a diferença percentual entre WSS consecutivos
wss_diffs <- diff(wss) / wss[-length(wss)]

# Encontrar o número ótimo de clusters no cotovelo (onde a variação percentual é mínima)
cotovelo <- which.min(wss_diffs) + 2  # <-

# Visualizar o método do cotovelo e marcar o ponto ótimo
fviz_nbclust(dados_acp, kmeans, method = "wss") + 
  geom_vline(xintercept = cotovelo, linetype = 2, color = "red") +
  labs(subtitle = paste("Número ótimo de clusters: ", cotovelo))

# Aplicar K-Means com o número ótimo de clusters
set.seed(42)
resultado_kmeans <- kmeans(dados_acp, centers = cotovelo, nstart = 25)

# Adicionar os clusters ao dataframe
dados_acp$Cluster <- as.factor(resultado_kmeans$cluster)

# Visualizar os clusters em um gráfico hexplot com densidade e maior contraste
ggplot(dados_acp, aes(x = PC1, y = PC2)) +
  geom_hex(bins = 30, aes(fill = ..density..), color = "black", size = 0.3) +  # Cor de borda preta nos hexágonos para maior contraste
  scale_fill_gradient(low = "white", high = "#003366") +  # Gradiente com maior contraste para os hexágonos
  geom_point(aes(color = Cluster), alpha = 0) +  # Ponto invisível só para definir cores dos clusters
  stat_ellipse(aes(color = Cluster), type = "norm", linetype = "dashed", size = 1.2) +  # Contorno dos clusters com elipse tracejada e espessura maior
  labs(title = "Hexplot dos Clusters com Maior Contraste", x = "Componente Principal 1", y = "Componente Principal 2") +
  theme_minimal() +
  theme(legend.position = "right")

# Calcular a matriz de correlação das variáveis selecionadas
matriz_correlacao <- cor(dados_selecionados, use = "complete.obs")

# Converter a matriz de correlação em uma matriz de distâncias (usando 1 - correlação)
matriz_distancia <- as.dist(1 - matriz_correlacao)

# Aplicar o agrupamento hierárquico usando o método "ward.D2"
agrupamento_hierarquico <- hclust(matriz_distancia, method = "ward.D2")

# Plotar o dendrograma usando factoextra
fviz_dend(agrupamento_hierarquico, 
          k = 4,  # Número de clusters a ser visualizado (ajustável)
          cex = 0.8,  # Tamanho do texto
          color_labels_by_k = TRUE,  # Colorir os rótulos por cluster
          rect = TRUE,  # Adicionar retângulos em torno dos clusters
          rect_border = "jco",  # Cores dos retângulos
          rect_fill = TRUE,  # Preencher os retângulos
          main = "Dendrograma das Variáveis")
