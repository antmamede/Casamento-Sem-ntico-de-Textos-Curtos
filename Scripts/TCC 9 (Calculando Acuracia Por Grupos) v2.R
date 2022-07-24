acuracia_grupo <- function(grupo){
  # ------------------------------------------------------------------------------
  # Importando Pacotes e Funcoes
  # ------------------------------------------------------------------------------
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
  
  fname_jaro <- paste("Jaro_",grupo,"_MS",".xlsx", sep = "")
  fname_tfidf <- paste("TFIDF_",grupo,"_MS",".xlsx", sep = "")
  fname_w2v <- paste("Word2VecMedia_",grupo,"_MS",".xlsx", sep = "")
  # Jaro
  jaro_ms <- read_xlsx(fname_jaro)
  names(jaro_ms)[1] <- "DESC_POF"
  
  # TF-IDF
  tfidf_ms <- read_xlsx(fname_tfidf)
  names(tfidf_ms)[1] <- "DESC_POF"
  
  # Word2Vec Media
  w2vm_ms <- read_xlsx(fname_w2v)
  names(w2vm_ms)[1] <- "DESC_POF"
  w2vm_ms[w2vm_ms < 0] <- 0
  
  n_obs <- nrow(w2vm_ms)
  
  # ------------------------------------------------------------------------------
  # Matrizes do Experimento 2
  # ------------------------------------------------------------------------------
  matriz_jtw <- (jaro_ms[,2:ncol(jaro_ms)]^2 + w2vm_ms[,2:ncol(jaro_ms)]^2 + tfidf_ms[,2:ncol(jaro_ms)]^2)/3
  
  matriz_jtw <- cbind.data.frame(jaro_ms$DESC_POF, matriz_jtw)
  names(matriz_jtw)[1] <- "DESC_POF"
  
  
  # ------------------------------------------------------------------------------
  # Transformando colunas em linhas
  # ------------------------------------------------------------------------------
  matriz_jtw <- matriz_jtw %>% pivot_longer(cols = 2:ncol(matriz_jtw))
  dadosbox <- matriz_jtw
  dadosbox$Grupo <- grupo
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
    df_comparacao %>% summarise(Acuracia_Estrita = sum(DESC_SNIPC == name) / n_obs) -> acuracia_estrita
    return(acuracia_estrita)
  }
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
    df_comparacao %>% ungroup(DESC_POF) %>% summarise(Acuracia_Ponderada = sum(aux) / n_obs) -> acuracia_ponderada
    return(acuracia_ponderada)
  }
  
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
  
  pm_jtw <- posicao_media(matriz_jtw, gabarito)
  
  
  # ------------------------------------------------------------------------------
  # Organizando e Salvando Resultados
  # ------------------------------------------------------------------------------
  aes <- c(ae_jtw)
  aps <- c(ap_jtw)
  pms <- c(pm_jtw)
  
  aes <- unlist(aes)
  aps <- unlist(aps)
  pms <- unlist(pms)
  
  df_final <- data.frame("Metodo" = c("Jar_TFIDF_W2V"), "Acuracia_Estrita" = aes, "Acuracia_Ponderada" = aps, "Posicao_Media" = pms, "Grupo" = grupo)
  row.names(df_final) <- NULL
  
  return(list(Metricas = df_final, Dados_Boxplot = dadosbox))
}

testando <- acuracia_grupo("g1")
testando$Metricas

boxplot(testando$Dados_Boxplot$value)

r1 <- acuracia_grupo("g1")
r2 <- acuracia_grupo("g2")
r3 <- acuracia_grupo("g3")
r4 <- acuracia_grupo("g4")
r5 <- acuracia_grupo("g5")
r6 <- acuracia_grupo("g6")
r7 <- acuracia_grupo("g7")
r8 <- acuracia_grupo("g8")
r9 <- acuracia_grupo("g9")


metricas_exp4 <- rbind.data.frame(r1$Metricas, r2$Metricas, r3$Metricas,
                 r4$Metricas, r5$Metricas, r6$Metricas,
                 r7$Metricas, r8$Metricas, r9$Metricas)

openxlsx::write.xlsx(metricas_exp4, "Métricas EXP4.xlsx")


dados_boxplot_exp4 <- rbind.data.frame(r1$Dados_Boxplot, r2$Dados_Boxplot, r3$Dados_Boxplot,
                                  r4$Dados_Boxplot, r5$Dados_Boxplot, r6$Dados_Boxplot,
                                  r7$Dados_Boxplot, r8$Dados_Boxplot, r9$Dados_Boxplot)


sub(dados_boxplot_exp4$Grupo, "g1", "Alimentação e Bebidas")
library(stringr)
dados_boxplot_exp4$Grupo <- str_replace_all(dados_boxplot_exp4$Grupo, "g1", "Alimentação e Bebidas")
dados_boxplot_exp4$Grupo <- str_replace_all(dados_boxplot_exp4$Grupo, "g2", "Artigos de Residência")
dados_boxplot_exp4$Grupo <- str_replace_all(dados_boxplot_exp4$Grupo, "g3", "Comunicação")
dados_boxplot_exp4$Grupo <- str_replace_all(dados_boxplot_exp4$Grupo, "g4", "Despesas Pessoais")
dados_boxplot_exp4$Grupo <- str_replace_all(dados_boxplot_exp4$Grupo, "g5", "Educação")
dados_boxplot_exp4$Grupo <- str_replace_all(dados_boxplot_exp4$Grupo, "g6", "Habitação")
dados_boxplot_exp4$Grupo <- str_replace_all(dados_boxplot_exp4$Grupo, "g7", "Saúde e Cuidados Pessoais")
dados_boxplot_exp4$Grupo <- str_replace_all(dados_boxplot_exp4$Grupo, "g8", "Transportes")
dados_boxplot_exp4$Grupo <- str_replace_all(dados_boxplot_exp4$Grupo, "g9", "Vestuário")

setwd(path_resultados)
pdf("Boxplots EXP4.pdf", width = 10, height = 8)
dados_boxplot_exp4 %>% ggplot(aes(y = value, fill = Grupo)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Similaridades") +
  xlab("")
dev.off()
