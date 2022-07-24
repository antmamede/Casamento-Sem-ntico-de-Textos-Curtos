# ------------------------------------------------------------------------------
# Importando Pacotes e Funcoes
# ------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(stringr)
library(tm)
library(wordcloud2)

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

# Base Inicial
base <- read_xlsx("POF_SNIPC_ACENTUADA.xlsx")
base <- base %>% select(DESC_POF, DESC_SNIPC, DESC_GRUPO_SNIPC)
# ------------------------------------------------------------------------------
# Tratamento Inicial da Base
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Tratamento Inicial da Base
# ------------------------------------------------------------------------------


## Ordenando base por ordem alfabetica pela DESC_POF e DESC_SNIPC
base <- base %>% arrange(DESC_POF, DESC_SNIPC)

## Checando NAs
apply(base, 2, function(x){sum(is.na(x))}) ## 3 valores em COD_POF e DESC_POF
base <- na.omit(base)

## Salvando palavras na forma original
#og_pof <- base$DESC_POF
#og_snipc <- base$DESC_SNIPC

## Deixando todos os caracteres minusculos
base$DESC_POF <- tolower(base$DESC_POF)
base$DESC_SNIPC <- tolower(base$DESC_SNIPC)

## Substituindo alguns caracteres especiais por whitespace
base$DESC_POF <- str_replace_all(base$DESC_POF,"[\"'-[,](.)/+]", " " )
base$DESC_SNIPC <- str_replace_all(base$DESC_SNIPC,"[\"'-[,](.)/+]", " " )

## Removendo stopwords
base$DESC_POF <- removeWords(base$DESC_POF, words = stopwords("portuguese"))
base$DESC_SNIPC <- removeWords(base$DESC_SNIPC, words = stopwords("portuguese"))

## Substituindo 2 ou mais espacos por apenas um
base$DESC_POF <- str_replace_all(base$DESC_POF, " +", " ")
base$DESC_SNIPC <- str_replace_all(base$DESC_SNIPC, " +", " ")

# Removendo espaco antes e depois
base$DESC_POF <- str_trim(base$DESC_POF)
base$DESC_SNIPC <- str_trim(base$DESC_SNIPC)

## Removendo palavras com exato mesmo match
base <- base %>% filter(DESC_POF != DESC_SNIPC)

## Removendo valores unicos
base <- unique(base)

## Removendo palavras com mais de uma referencia
base %>% group_by(DESC_POF) %>% summarise(count = n()) -> contando
contando <- contando %>% filter(count > 1)

base <- base %>% filter(!DESC_POF %in% contando$DESC_POF)
base$DESC_POF <- str_trim(base$DESC_POF, side = "both")
base$DESC_SNIPC <- str_trim(base$DESC_SNIPC, side = "both")



analise_exploratoria <- function(base){

  # Estatisticas Descritivas
  # Numero de Tokens
  n_tokens_pof <- lapply(strsplit(base$DESC_POF, " "), length)
  n_tokens_pof <- unlist(n_tokens_pof)
  media_tokens_POF <- mean(n_tokens_pof) #POF
  n_tokens_snipc <- lapply(strsplit(base$DESC_SNIPC, " "), length)
  n_tokens_snipc <- unlist(n_tokens_snipc)
  media_tokens_SNIPC <- mean(n_tokens_snipc) # SNIPC
  
  # Comprimento medio (caracteres)
  n_char_pof <- sapply(base$DESC_POF, nchar)
  media_caracters_POF <- mean(n_char_pof) # POF
  
  n_char_snipc <- sapply(base$DESC_SNIPC, nchar)
  media_caracters_SNIPC <- mean(n_char_snipc) #SNIPC
  
  # Correlacao entre numero de tokens
  correl_n_tokens <- cor(n_tokens_pof, n_tokens_snipc) # correl entre n_tokens
  
  # Palavras em comum
  
  lista_pof <- strsplit(base$DESC_POF, " ")
  lista_snipc <- strsplit(base$DESC_SNIPC, " ")
  length(intersect(lista_pof[[5]],lista_snipc[[5]]))
  
  palavras_comum <- c()
  
  for (i in 1:nrow(base)){
    auxpof <- lista_pof[[i]]
    auxsnipc <- lista_snipc[[i]]
    resposta <- length(intersect(auxpof, auxsnipc))
    palavras_comum <- c(palavras_comum, resposta)
  }
  
  
  # Tokens mais frequentes
  desc_pof <- base$DESC_POF
  desc_pof <- str_split(desc_pof, " ")
  desc_pof <- unlist(desc_pof)
  palavras_unicas_pof <- unique(desc_pof)
  #desc_pof <- unique(desc_pof) # a POF possui 2519 tokens unicos
  desc_pof <- as.data.frame(desc_pof)
  desc_pof %>% group_by(desc_pof) %>% summarise(contador = n()) -> desc_pof
  desc_pof <- desc_pof[order(-desc_pof$contador),]
  tokens_frequentes_pof <- head(desc_pof, n = 10)
  
  desc_snipc <- base$DESC_SNIPC
  desc_snipc <- str_split(desc_snipc, " ")
  desc_snipc <- unlist(desc_snipc)
  palavras_unicas_snipc <- unique(desc_snipc)
  desc_snipc <- as.data.frame(desc_snipc)
  desc_snipc %>% group_by(desc_snipc) %>% summarise(contador = n()) -> desc_snipc
  desc_snipc <- desc_snipc[order(-desc_snipc$contador),]
  tokens_frequentes_snipc <- head(desc_snipc, n = 10)
  
  wordcloud_pof <- wordcloud2(data=desc_pof)
  wordcloud_snipc <- wordcloud2(data=desc_snipc)
  
  
  return(list(Media_Tokens_POF = media_tokens_POF, Media_Tokens_SNIPC = media_tokens_SNIPC,
         Media_Caracters_POF = media_caracters_POF, Media_Caracters_SNIPC = media_caracters_SNIPC,
         Correl_N_Tokens = correl_n_tokens, Palavras_Em_Comum = mean(palavras_comum),
         Tokens_Frequentes_POF = tokens_frequentes_pof, Tokens_Frequentes_SNIPC = tokens_frequentes_snipc,
         WordCloud_POF = wordcloud_pof, WordCloud_SNIPC = wordcloud_snipc,
         Palavras_Unicas_POF = palavras_unicas_pof, Palavras_Unicas_SNIPC = palavras_unicas_snipc))
    
}

alimentacao_e_bebidas <- base %>% filter(DESC_GRUPO_SNIPC == "ALIMENTAÇÃO E BEBIDAS")
artigos_de_residencia <- base %>% filter(DESC_GRUPO_SNIPC == "ARTIGOS DE RESIDÊNCIA")
comunicacao <- base %>% filter(DESC_GRUPO_SNIPC == "COMUNICAÇÃO")
despesas_pessoais <- base %>% filter(DESC_GRUPO_SNIPC == "DESPESAS PESSOAIS")
educacao <- base %>% filter(DESC_GRUPO_SNIPC == "EDUCAÇÃO")
habitacao <- base %>% filter(DESC_GRUPO_SNIPC == "HABITAÇÃO")
saude_e_cuidados_pessoais <- base %>% filter(DESC_GRUPO_SNIPC == "SAÚDE E CUIDADOS PESSOAIS")
transportes <- base %>% filter(DESC_GRUPO_SNIPC == "TRANSPORTES")
vestuario <- base %>% filter(DESC_GRUPO_SNIPC == "VESTUÁRIO")


r1 <- analise_exploratoria(alimentacao_e_bebidas)
r2 <- analise_exploratoria(artigos_de_residencia)
r3 <- analise_exploratoria(comunicacao)
r4 <- analise_exploratoria(despesas_pessoais)
r5 <- analise_exploratoria(educacao)
r6 <- analise_exploratoria(habitacao)
r7 <- analise_exploratoria(saude_e_cuidados_pessoais)
r8 <- analise_exploratoria(transportes)
r9 <- analise_exploratoria(vestuario)
tudo <- analise_exploratoria(base)
base_metade <- base %>% filter(DESC_GRUPO_SNIPC != "ALIMENTAÇÃO E BEBIDAS")
metade <- analise_exploratoria(base_metade)

table(base$DESC_GRUPO_SNIPC)/3305
