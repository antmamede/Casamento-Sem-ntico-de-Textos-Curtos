# ------------------------------------------------------------------------------
# Importando Pacotes e Funcoes
# ------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
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
# Transformando colunas em linhas
# ------------------------------------------------------------------------------
levenshtein_ms <- levenshtein_ms %>% pivot_longer(cols = 2:1150)
jaro_ms <- jaro_ms %>% pivot_longer(cols = 2:1150) 
jaccard_ms <- jaccard_ms %>% pivot_longer(cols = 2:1150)
tfidf_ms <- tfidf_ms %>% pivot_longer(cols = 2:1150)
w2vs_ms <- w2vs_ms %>% pivot_longer(cols = 2:1150)
w2vm_ms <- w2vm_ms %>% pivot_longer(cols = 2:1150)

# ------------------------------------------------------------------------------
# Boxplots
# ------------------------------------------------------------------------------
jaccard_ms$Metodo <- "Jaccard"
jaro_ms$Metodo <- "Jaro"
levenshtein_ms$Metodo <- "Levenshtein"
tfidf_ms$Metodo <- "TF-IDF"
w2vm_ms$Metodo <- "word2vec Média"
w2vs_ms$Metodo <- "word2vec Soma"

basebox <- rbind.data.frame(levenshtein_ms, jaro_ms, jaccard_ms, tfidf_ms, w2vm_ms, w2vs_ms)

setwd(path_resultados)
pdf("Boxplots EXP1.pdf", width = 10, height = 8)
basebox %>% ggplot(aes(x = Metodo, y = value, fill = Metodo)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none") +
  ylab("Similaridades") +
  xlab("Método")
dev.off()

#levenshtein_ms <- levenshtein_ms %>% pivot_longer(cols = 2:1222, names_to = "Par_Levenshtein", values_to = "Valor_Levenshtein")
#jaro_ms <- jaro_ms %>% pivot_longer(cols = 2:1222, names_to = "Par_Jaro", values_to = "Valor_Jaro") 
#jaccard_ms <- jaccard_ms %>% pivot_longer(cols = 2:1222, names_to = "Par_Jaccard", values_to = "Valor_Jaccard")
#tfidf_ms <- tfidf_ms %>% pivot_longer(cols = 2:1222, names_to = "Par_TFIDF", values_to = "Valor_TFIDF")
#w2vs_ms <- w2vs_ms %>% pivot_longer(cols = 2:1217, names_to = "Par_Word2VecSoma", values_to = "Valor_Word2VecSoma")
#w2vm_ms <- w2vm_ms %>% pivot_longer(cols = 2:1217, names_to = "Par_Word2VecMedia", values_to = "Valor_Word2VecMedia")

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
ae_levenshtein <- prepara_ae(levenshtein_ms, gabarito)
ae_jaro <- prepara_ae(jaro_ms, gabarito)
ae_jaccard <- prepara_ae(jaccard_ms, gabarito)
ae_tfidf <- prepara_ae(tfidf_ms, gabarito)
ae_w2vs <- prepara_ae(w2vs_ms, gabarito)
ae_w2vm <- prepara_ae(w2vm_ms, gabarito)
teste <- prepara_ae(levenshtein_ms, gabarito)
teste2 <- prepara_ae(teste, gabarito)

acuracias_estritas <- data.frame("Levenshtein" = as.numeric(ae_levenshtein),
                                 "Jaro" = as.numeric(ae_jaro),
                                 "Jaccard" = as.numeric(ae_jaccard),
                                 "TFIDF" = as.numeric(ae_tfidf),
                                 "Word2VecSoma" = as.numeric(w2vs_ms),
                                 "Word2VecMedia" = as.numeric(w2vm_ms))

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

ap_levenshtein <- prepara_ap(levenshtein_ms, gabarito)
ap_jaro <- prepara_ap(jaro_ms, gabarito)
ap_jaccard <- prepara_ap(jaccard_ms, gabarito)
ap_tfidf <- prepara_ap(tfidf_ms, gabarito)
ap_w2vs <- prepara_ap(w2vs_ms, gabarito)
ap_w2vm <- prepara_ap(w2vm_ms, gabarito)

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


# ------------------------------------------------------------------------------
# Organizando e Salvando Resultados
# ------------------------------------------------------------------------------
aes <- c(ae_levenshtein,ae_jaro, ae_jaccard, ae_tfidf, ae_w2vs,ae_w2vm)
aps <- c(ap_levenshtein,ap_jaro, ap_jaccard, ap_tfidf, ap_w2vs,ap_w2vm)
pms <- c(pm_levenshtein,pm_jaro, pm_jaccard, pm_tfidf, pm_w2vs,pm_w2vm)

aes <- unlist(aes)
aps <- unlist(aps)
pms <- unlist(pms)

df_final <- data.frame("Metodo" = c("Levenshtein", "Jaro", "Jaccard", "TFIDF", "W2VS", "W2VM"), "Acuracia_Estrita" = aes, "Acuracia_Ponderada" = aps, "Posicao_Media" = pms)

setwd(path_resultados)
openxlsx::write.xlsx(df_final, "Métricas EXP1.xlsx")

