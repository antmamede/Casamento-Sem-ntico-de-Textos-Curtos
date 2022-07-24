# ------------------------------------------------------------------------------
# Importando Pacotes e Funcoes
# ------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(tidyr)
library(dplyr)
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

# TEP
tep <- read_xlsx("Base_TEP_2Sinonimos.xlsx")


setwd(path_resultados)
# Levenshtein
levenshtein_ms <- read_xlsx("Levenshtein_Sinonimos_MS.xlsx")
names(levenshtein_ms)[1] <- "DESC_POF"

# Jaro
jaro_ms <- read_xlsx("Jaro_Sinonimos_MS.xlsx")
names(jaro_ms)[1] <- "DESC_POF"

# Jaccard
jaccard_ms <- read_xlsx("Jaccard_Sinonimos_MS.xlsx")
names(jaccard_ms)[1] <- "DESC_POF"

# TF-IDF
tfidf_ms <- read_xlsx("TFIDF_Sinonimos_MS.xlsx")
names(tfidf_ms)[1] <- "DESC_POF"

# Word2Vec Soma
w2vs_ms <- read_xlsx("Word2VecSoma_Sinonimos_MS.xlsx")
names(w2vs_ms)[1] <- "DESC_POF"

# Word2Vec Media
w2vm_ms <- read_xlsx("Word2VecMedia_Sinonimos_MS.xlsx")
names(w2vm_ms)[1] <- "DESC_POF"

sinonimos_tep <- function(base, tep){
  x <- unlist(c(base))
  x <- strsplit(x, split = " ")
  y <- x
  for (i in 1:length(x)){
    for(j in 1:length(x[[i]])){
      word <- x[[i]][j]
      aux <- which(word == tep$S1 | word == tep$S2 | word == tep$S3)
      auxwords <- unique(unlist(strsplit(tep$Junto[aux], " ")))
      x[[i]] <- c(x[[i]], auxwords) # se der ruim substituir por y
    }
    x[[i]] <- unique(x[[i]])
    x[[i]] <- paste(x[[i]], collapse = " ")
  }
  x <- as.data.frame(x)
  x <- t(x)
  x <- as.data.frame(x)
  return(x)
}
desc_snipc <- gabarito$DESC_SNIPC
desc_pof <- gabarito$DESC_POF
desc_snipc <- sinonimos_tep(desc_snipc, tep)
names(desc_snipc) <- "DESC_SNIPC"
desc_pof <- sinonimos_tep(desc_pof, tep)
names(desc_pof) <- "DESC_POF"
gabarito <- data.frame("DESC_POF" = desc_pof, "DESC_SNIPC" = desc_snipc)
row.names(gabarito) <- NULL

## MATRIZES HIBRIDAS
### HARTMAN (TFIDF + W2V)
matriz_hartmann <- (tfidf_ms[,2:1150]^2 + w2vm_ms[,2:1150]^2)/2
matriz_hartmann <- cbind.data.frame(levenshtein_ms$DESC_POF, matriz_hartmann)
names(matriz_hartmann)[1] <- "DESC_POF"

### MATRIZ COM TUDO
matriz_tudo <- (levenshtein_ms[,2:1150]^2 + jaro_ms[,2:1150]^2 + jaccard_ms[,2:1150]^2 + w2vm_ms[,2:1150]^2 + tfidf_ms[,2:1150]^2)/5
matriz_tudo <- cbind.data.frame(levenshtein_ms$DESC_POF, matriz_tudo)
names(matriz_tudo)[1] <- "DESC_POF"

### MATRIZ JARO + TFIDF + W2V
matriz_jtw <- (jaro_ms[,2:1150]^2 + w2vm_ms[,2:1150]^2 + tfidf_ms[,2:1150]^2)/3
matriz_jtw <- cbind.data.frame(levenshtein_ms$DESC_POF, matriz_jtw)
names(matriz_jtw)[1] <- "DESC_POF"

### MATRIZ THIAGO (LEV + JARO + JAC + W2V)
matriz_thiago <- (levenshtein_ms[,2:1150]^2 + jaro_ms[,2:1150]^2 + jaccard_ms[,2:1150]^2 + w2vm_ms[,2:1150]^2)/4
matriz_thiago <- cbind.data.frame(levenshtein_ms$DESC_POF, matriz_thiago)
names(matriz_thiago)[1] <- "DESC_POF"


# ------------------------------------------------------------------------------
# Transformando colunas em linhas
# ------------------------------------------------------------------------------
levenshtein_ms <- levenshtein_ms %>% pivot_longer(cols = 2:1150)
jaro_ms <- jaro_ms %>% pivot_longer(cols = 2:1150) 
jaccard_ms <- jaccard_ms %>% pivot_longer(cols = 2:1150)
tfidf_ms <- tfidf_ms %>% pivot_longer(cols = 2:1150)
w2vs_ms <- w2vs_ms %>% pivot_longer(cols = 2:1150)
w2vm_ms <- w2vm_ms %>% pivot_longer(cols = 2:1150)
matriz_hartmann <- matriz_hartmann %>% pivot_longer(cols = 2:1150)
matriz_tudo <- matriz_tudo %>% pivot_longer(cols = 2:1150)
matriz_jtw <- matriz_jtw %>% pivot_longer(cols = 2:1150)
matriz_thiago <- matriz_thiago %>% pivot_longer(cols = 2:1150)
# ------------------------------------------------------------------------------
# Acuracia Estrita
# ------------------------------------------------------------------------------
# tirar esse nrow(df_comparacao)
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
ae_levenshtein <- prepara_ae(levenshtein_ms, gabarito)
ae_jaro <- prepara_ae(jaro_ms, gabarito)
ae_jaccard <- prepara_ae(jaccard_ms, gabarito)
ae_tfidf <- prepara_ae(tfidf_ms, gabarito)
ae_w2vs <- prepara_ae(w2vs_ms, gabarito)
ae_w2vm <- prepara_ae(w2vm_ms, gabarito)
ae_hartmann <- prepara_ae(matriz_hartmann, gabarito)
ae_tudo <- prepara_ae(matriz_tudo, gabarito)
ae_jtw <- prepara_ae(matriz_jtw, gabarito)
ae_thiago <- prepara_ae(matriz_thiago, gabarito)
# ------------------------------------------------------------------------------
# Acuracia Ponderadap
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

ap_levenshtein <- prepara_ap(levenshtein_ms, gabarito)
ap_jaro <- prepara_ap(jaro_ms, gabarito)
ap_jaccard <- prepara_ap(jaccard_ms, gabarito)
ap_tfidf <- prepara_ap(tfidf_ms, gabarito)
ap_w2vs <- prepara_ap(w2vs_ms, gabarito)
ap_w2vm <- prepara_ap(w2vm_ms, gabarito)
ap_hartmann <- prepara_ap(matriz_hartmann, gabarito)
ap_tudo <- prepara_ap(matriz_tudo, gabarito)
ap_jtw <- prepara_ap(matriz_jtw, gabarito)
ap_thiago <- prepara_ap(matriz_thiago, gabarito)
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
pm_levenshtein <- posicao_media(levenshtein_ms, gabarito)
pm_jaro <- posicao_media(jaro_ms, gabarito)
pm_jaccard <- posicao_media(jaccard_ms, gabarito)
pm_tfidf <- posicao_media(tfidf_ms, gabarito)
pm_w2vs <- posicao_media(w2vs_ms, gabarito)
pm_w2vm <- posicao_media(w2vm_ms, gabarito)
pm_hartmann <- posicao_media(matriz_hartmann, gabarito)
pm_tudo <- posicao_media(matriz_tudo, gabarito)
pm_jtw <- posicao_media(matriz_jtw, gabarito)
pm_thiago <- posicao_media(matriz_thiago, gabarito)


aes <- c(ae_jaro, ae_jaccard, ae_tfidf, ae_w2vm, ae_hartmann, ae_thiago, ae_tudo, ae_jtw)
aps <- c(ap_jaro, ap_jaccard, ap_tfidf, ap_w2vm, ap_hartmann, ap_thiago, ap_tudo, ap_jtw)
pms <- c(pm_jaro, pm_jaccard, pm_tfidf, pm_w2vm, pm_hartmann, pm_thiago, pm_tudo, pm_jtw)

aes <- unlist(aes)
aps <- unlist(aps)
pms <- unlist(pms)


df_final <- data.frame("Metodo" = c("Jaro", "Jaccard", "TFIDF", "W2V", "Hartmann", "Thiago", "Tudo", "JTW"), "Acuracia_Estrita" = aes, "Acuracia_Ponderada" = aps, "Posicao_Media" = pms)

setwd(path_resultados)
openxlsx::write.xlsx(df_final, "Métricas 2 Sinonimos.xlsx")

