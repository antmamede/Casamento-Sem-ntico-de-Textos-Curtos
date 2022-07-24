# ------------------------------------------------------------------------------
# Importando Pacotes e Funcoes
# ------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
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

# Gabarito para calculo de acuracia
gabarito <- read_xlsx("Base_Descricoes_V2.xlsx")


setwd(path_resultados)
# Levenshtein
levenshtein_ms <- read_xlsx("Levenshtein_MS.xlsx")
names(levenshtein_ms)[1] <- "DESC_POF"

# Jaro
jaro_ms <- read_xlsx("Jaro_MS.xlsx")
names(jaro_ms)[1] <- "DESC_POF"

# Jaccard
jaccard_ms <- read_xlsx("Jaccard_MS.xlsx")
names(jaccard_ms)[1] <- "DESC_POF"

# TF-IDF
tfidf_ms <- read_xlsx("TFIDF_MS.xlsx")
names(tfidf_ms)[1] <- "DESC_POF"

# Word2Vec Soma
w2vs_ms <- read_xlsx("Word2VecSoma_MS.xlsx")
names(w2vs_ms)[1] <- "DESC_POF"
w2vs_ms[w2vs_ms < 0] <- 0


# Word2Vec Media
w2vm_ms <- read_xlsx("Word2VecMedia_MS.xlsx")
names(w2vm_ms)[1] <- "DESC_POF"
w2vm_ms[w2vm_ms < 0] <- 0

# ------------------------------------------------------------------------------
# Matrizes do Experimento 2
# ------------------------------------------------------------------------------

matriz_thiago <- (levenshtein_ms[,2:1150]^2 + jaro_ms[,2:1150]^2 + jaccard_ms[,2:1150]^2 + w2vm_ms[,2:1150]^2)/4
matriz_tudo <- (levenshtein_ms[,2:1150]^2 + jaro_ms[,2:1150]^2 + jaccard_ms[,2:1150]^2 + w2vm_ms[,2:1150]^2 + tfidf_ms[,2:1150]^2)/5
matriz_hartmann <- (tfidf_ms[,2:1150]^2 + w2vm_ms[,2:1150]^2)/2
matriz_jtw <- (jaro_ms[,2:1150]^2 + w2vm_ms[,2:1150]^2 + tfidf_ms[,2:1150]^2)/3

matriz_jtw <- cbind.data.frame(jaro_ms$DESC_POF, matriz_jtw)
names(matriz_jtw)[1] <- "DESC_POF"
matriz_thiago <- cbind.data.frame(levenshtein_ms$DESC_POF, matriz_thiago)
names(matriz_thiago)[1] <- "DESC_POF"
matriz_tudo <- cbind.data.frame(levenshtein_ms$DESC_POF, matriz_tudo)
names(matriz_tudo)[1] <- "DESC_POF"
matriz_hartmann <- cbind.data.frame(levenshtein_ms$DESC_POF, matriz_hartmann)
names(matriz_hartmann)[1] <- "DESC_POF"

# ------------------------------------------------------------------------------
# Transformando colunas em linhas
# ------------------------------------------------------------------------------
matriz_thiago <- matriz_thiago %>% pivot_longer(cols = 2:1150)
matriz_tudo <- matriz_tudo %>% pivot_longer(cols = 2:1150) 
matriz_hartmann <- matriz_hartmann %>% pivot_longer(cols = 2:1150)
matriz_jtw <- matriz_jtw %>% pivot_longer(cols = 2:1150)

# ------------------------------------------------------------------------------
# Boxplots
# ------------------------------------------------------------------------------
matriz_thiago$Metodo <- "MH1"
matriz_tudo$Metodo <- "MH3"
matriz_hartmann$Metodo <- "MH4"
matriz_jtw$Metodo <- "MH5"


basebox <- rbind.data.frame(matriz_thiago, matriz_tudo, matriz_hartmann, matriz_jtw)

setwd(path_resultados)
pdf("Boxplots EXP2.pdf", width = 10, height = 8)
basebox %>% ggplot(aes(x = Metodo, y = value, fill = Metodo)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none") +
  ylab("Similaridades") +
  xlab("Método")
dev.off()


# ------------------------------------------------------------------------------
# Acuracia Estrita
# ------------------------------------------------------------------------------
prepara_ae <- function(ms, gabarito){
  # Filtra por maior valor, e so considera maior valor unico
  ms %>% group_by(DESC_POF) %>% filter(value == max(value)) %>% mutate(count = n()) -> ms
  ms %>% filter(count == 1) -> ms
  ms$count <- NULL
  df_comparacao <- gabarito %>% left_join(ms)
  df_comparacao <- na.omit(df_comparacao)
  df_comparacao %>% summarise(Acuracia_Estrita = sum(DESC_SNIPC == name) / 3305) -> acuracia_estrita
  return(acuracia_estrita)
}
ae_thiago <- prepara_ae(matriz_thiago, gabarito)
ae_tudo <- prepara_ae(matriz_tudo, gabarito)
ae_hartmann <- prepara_ae(matriz_hartmann, gabarito)
ae_jtw <- prepara_ae(matriz_jtw, gabarito)

# ------------------------------------------------------------------------------
# Acuracia Ponderada
# ------------------------------------------------------------------------------
prepara_ap <- function(ms, gabarito){
  # Filtra por maior valor, e so considera maior valor unico
  ms %>% group_by(DESC_POF) %>% filter(value == max(value)) %>% mutate(count = n()) -> ms
  df_comparacao <- gabarito %>% left_join(ms)
  df_comparacao <- na.omit(df_comparacao)
  df_comparacao %>% group_by(DESC_POF) %>% mutate(aux = case_when(DESC_SNIPC %in% name ~ 1/count,
                                                                  !(DESC_SNIPC %in% name) ~ 0)) -> df_comparacao
  df_comparacao %>% select(DESC_POF, DESC_SNIPC, aux) %>% unique() -> df_comparacao
  df_comparacao %>% ungroup(DESC_POF) %>% summarise(Acuracia_Ponderada = sum(aux) / 3305) -> acuracia_ponderada
  return(acuracia_ponderada)
}

ap_thiago <- prepara_ap(matriz_thiago, gabarito)
ap_tudo <- prepara_ap(matriz_tudo, gabarito)
ap_hartmann <- prepara_ap(matriz_hartmann, gabarito)
ap_jtw <- prepara_ap(matriz_jtw, gabarito)

# ------------------------------------------------------------------------------
# Posicao Media
# ------------------------------------------------------------------------------
posicao_media <- function(ms, gabarito){
  df_comparacao <- gabarito %>% left_join(ms)
  df_comparacao %>% group_by(DESC_POF) %>% arrange(desc(value)) -> df_comparacao
  df_comparacao %>% group_by(DESC_POF) %>% mutate(rank = row_number()) -> df_comparacao
  df_comparacao %>% filter(DESC_SNIPC == name) -> df_comparacao
  resultado <- data.frame(Posicao_Media = mean(df_comparacao$rank))
  return(resultado)
}

pm_thiago <- posicao_media(matriz_thiago, gabarito)
pm_tudo <- posicao_media(matriz_tudo, gabarito)
pm_hartmann <- posicao_media(matriz_hartmann, gabarito)
pm_jtw <- posicao_media(matriz_jtw, gabarito)


# ------------------------------------------------------------------------------
# Organizando e Salvando Resultados
# ------------------------------------------------------------------------------
aes <- c(ae_thiago,ae_tudo, ae_hartmann, ae_jtw)
aps <- c(ap_thiago,ap_tudo, ap_hartmann, ap_jtw)
pms <- c(pm_thiago,pm_tudo, pm_hartmann, pm_jtw)

aes <- unlist(aes)
aps <- unlist(aps)
pms <- unlist(pms)

df_final <- data.frame("Metodo" = c("Lev_Jar_Jac_W2V",
                                    "Lev_Jar_Jac_TFIDF_W2V",
                                    "TFIDF_W2V",
                                    "Jar_TFIDF_W2V"), "Acuracia_Estrita" = aes, "Acuracia_Ponderada" = aps, "Posicao_Media" = pms)

setwd(path_resultados)
openxlsx::write.xlsx(df_final, "Métricas EXP2.xlsx")
