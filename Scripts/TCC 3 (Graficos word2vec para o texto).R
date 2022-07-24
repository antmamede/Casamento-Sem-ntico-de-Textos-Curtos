# ------------------------------------------------------------------------------
# Importando Pacotes e Funcoes
# ------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(ggrepel)
# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
path_embeddings = "C:\\Users\\aamma\\OneDrive\\ENCE\\TCC\\Embeddings"
path_bases      = "C:\\Users\\aamma\\OneDrive\\ENCE\\TCC\\Bases"
path_resultados = "C:\\Users\\aamma\\OneDrive\\ENCE\\TCC\\Resultados"
path_graficos = "C:\\Users\\aamma\\OneDrive\\ENCE\\TCC\\Graficos"
# ------------------------------------------------------------------------------
# Lendo dados
# ------------------------------------------------------------------------------
setwd(path_bases)
dados <- read.table("fofura_tamanho.txt", header = TRUE, sep = ",")

# ------------------------------------------------------------------------------
# Graficos
# ------------------------------------------------------------------------------

pdf()
g1 <- ggplot(dados, aes(fofura, tamanho, label = animal))+
  geom_text() +
  xlab("Fofura") +
  ylab("Tamanho") +
  xlim(-2, 99) +
  theme_bw()
dev.off()
g1
