# ------------------------------------------------------------------------------
# Importando Pacotes e Funcoes
# ------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(stringr)

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
path_embeddings = "C:\\Users\\aamma\\OneDrive\\ENCE\\TCC\\REFAZENDO EXPERIMENTO\\Embeddings"
path_bases      = "C:\\Users\\aamma\\OneDrive\\ENCE\\TCC\\REFAZENDO EXPERIMENTO\\Bases"
path_resultados = "C:\\Users\\aamma\\OneDrive\\ENCE\\TCC\\REFAZENDO EXPERIMENTO\\Resultados"

# ------------------------------------------------------------------------------
# Lendo dados
# ------------------------------------------------------------------------------
setwd(path_bases)

# Lendo TEP
tep <- read_xlsx("Sinonimos_TEP_v2.xlsx")



