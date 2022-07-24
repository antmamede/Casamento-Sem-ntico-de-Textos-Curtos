# ------------------------------------------------------------------------------
# Importando pacotes
# ------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(tm)

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
path_embeddings = "C:\\Users\\aamma\\OneDrive\\ENCE\\TCC\\REFAZENDO EXPERIMENTO\\Embeddings"
path_bases      = "C:\\Users\\aamma\\OneDrive\\ENCE\\TCC\\REFAZENDO EXPERIMENTO\\Bases"

# ------------------------------------------------------------------------------
# Lendo dados
# ------------------------------------------------------------------------------
setwd(path_bases)

# Base a ser utilizada
base <- read_xlsx("POF_SNIPC_ACENTUADA.xlsx")
base <- base %>% select(DESC_POF, DESC_SNIPC, DESC_GRUPO_SNIPC)
palavras_embd <- read_xlsx("palavras_embd.xlsx")
names(palavras_embd) <- "Palavras"

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

# ------------------------------------------------------------------------------
# Separando descricoes da POF e SNIPC
# ------------------------------------------------------------------------------

separa_palavras <- function(base){
  palavras_pof <- base$DESC_POF
  palavras_snipc <- base$DESC_SNIPC
  
  palavras_pof <- unique(palavras_pof)
  palavras_snipc <- unique(palavras_snipc)
  
  palavras_pof <- str_sort(palavras_pof)
  palavras_snipc <- str_sort(palavras_snipc)
  palavras_pof <- data.frame("DESC_POF" = palavras_pof)
  palavras_snipc <- data.frame("DESC_SNIPC" = palavras_snipc)
  
  # Separando por espaco
  tokens_pof <- palavras_pof$DESC_POF
  tokens_snipc <- palavras_snipc$DESC_SNIPC
  tokens_pof <- str_split(tokens_pof, " ")
  tokens_snipc <- str_split(tokens_snipc, " ")
  
  # Unlisting listas de palavras
  tokens_pof <- unlist(tokens_pof)
  tokens_snipc <- unlist(tokens_snipc)
  
  # Unique
  tokens_pof <- unique(tokens_pof)
  tokens_snipc <- unique(tokens_snipc)
  
  # Separa tokens nao contidos
  tokens_pof_nc <- tokens_pof[which(!(tokens_pof %in% palavras_embd$Palavras))]
  tokens_snipc_nc <- tokens_snipc[which(!(tokens_snipc %in% palavras_embd$Palavras))]
  
  
  # Separa todos os tokens
  tokens_all <- c(tokens_pof, tokens_snipc)
  tokens_all <- unique(tokens_all)
  tokens_all <- data.frame("Token" =  tokens_all)
  tokens_all_nc <- c(tokens_pof_nc, tokens_snipc_nc)
  tokens_all_nc <- unique(tokens_all_nc)
  tokens_all_nc <- data.frame("Token" = tokens_all_nc)
  
  return(list(palavras_pof, palavras_snipc, tokens_all, tokens_all_nc))
}

base_g1 <- base %>% filter(DESC_GRUPO_SNIPC == "ALIMENTAÇÃO E BEBIDAS")
base_g2 <- base %>% filter(DESC_GRUPO_SNIPC == "ARTIGOS DE RESIDÊNCIA")
base_g3 <- base %>% filter(DESC_GRUPO_SNIPC == "COMUNICAÇÃO")
base_g4 <- base %>% filter(DESC_GRUPO_SNIPC == "DESPESAS PESSOAIS")
base_g5 <- base %>% filter(DESC_GRUPO_SNIPC == "EDUCAÇÃO")
base_g6 <- base %>% filter(DESC_GRUPO_SNIPC == "HABITAÇÃO")
base_g7 <- base %>% filter(DESC_GRUPO_SNIPC == "SAÚDE E CUIDADOS PESSOAIS")
base_g8 <- base %>% filter(DESC_GRUPO_SNIPC == "TRANSPORTES")
base_g9 <- base %>% filter(DESC_GRUPO_SNIPC == "VESTUÁRIO")

aux_g1 <- separa_palavras(base_g1)
palavras_pof_g1 <- aux_g1[[1]]
palavras_snipc_g1 <- aux_g1[[2]]
tokens_all_g1 <- aux_g1[[3]]
tokens_all_nc_g1 <- aux_g1[[4]]

aux_g2 <- separa_palavras(base_g2)
palavras_pof_g2 <- aux_g2[[1]]
palavras_snipc_g2 <- aux_g2[[2]]
tokens_all_g2 <- aux_g2[[3]]
tokens_all_nc_g2 <- aux_g2[[4]]

aux_g3 <- separa_palavras(base_g3)
palavras_pof_g3 <- aux_g3[[1]]
palavras_snipc_g3 <- aux_g3[[2]]
tokens_all_g3 <- aux_g3[[3]]
tokens_all_nc_g3 <- aux_g3[[4]]

aux_g4 <- separa_palavras(base_g4)
palavras_pof_g4 <- aux_g4[[1]]
palavras_snipc_g4 <- aux_g4[[2]]
tokens_all_g4 <- aux_g4[[3]]
tokens_all_nc_g4 <- aux_g4[[4]]

aux_g5 <- separa_palavras(base_g5)
palavras_pof_g5 <- aux_g5[[1]]
palavras_snipc_g5 <- aux_g5[[2]]
tokens_all_g5 <- aux_g5[[3]]
tokens_all_nc_g5 <- aux_g5[[4]]

aux_g6 <- separa_palavras(base_g6)
palavras_pof_g6 <- aux_g6[[1]]
palavras_snipc_g6 <- aux_g6[[2]]
tokens_all_g6 <- aux_g6[[3]]
tokens_all_nc_g6 <- aux_g6[[4]]

aux_g7 <- separa_palavras(base_g7)
palavras_pof_g7 <- aux_g7[[1]]
palavras_snipc_g7 <- aux_g7[[2]]
tokens_all_g7 <- aux_g7[[3]]
tokens_all_nc_g7 <- aux_g7[[4]]

aux_g8 <- separa_palavras(base_g8)
palavras_pof_g8 <- aux_g8[[1]]
palavras_snipc_g8 <- aux_g8[[2]]
tokens_all_g8 <- aux_g8[[3]]
tokens_all_nc_g8 <- aux_g8[[4]]

aux_g9 <- separa_palavras(base_g9)
palavras_pof_g9 <- aux_g9[[1]]
palavras_snipc_g9 <- aux_g9[[2]]
tokens_all_g9 <- aux_g9[[3]]
tokens_all_nc_g9 <- aux_g9[[4]]


# ------------------------------------------------------------------------------
# Salvando dados
# ------------------------------------------------------------------------------
openxlsx::write.xlsx(palavras_pof_g1, "Descricoes_POF_g1.xlsx")
openxlsx::write.xlsx(palavras_snipc_g1, "Descricoes_SNIPC_g1.xlsx")
openxlsx::write.xlsx(tokens_all_g1, "Tokens_All_g1.xlsx")
openxlsx::write.xlsx(tokens_all_nc_g1, "Tokens_All_NC_g1.xlsx")


openxlsx::write.xlsx(palavras_pof_g2, "Descricoes_POF_g2.xlsx")
openxlsx::write.xlsx(palavras_snipc_g2, "Descricoes_SNIPC_g2.xlsx")
openxlsx::write.xlsx(tokens_all_g2, "Tokens_All_g2.xlsx")
openxlsx::write.xlsx(tokens_all_nc_g2, "Tokens_All_NC_g2.xlsx")

openxlsx::write.xlsx(palavras_pof_g3, "Descricoes_POF_g3.xlsx")
openxlsx::write.xlsx(palavras_snipc_g3, "Descricoes_SNIPC_g3.xlsx")
openxlsx::write.xlsx(tokens_all_g3, "Tokens_All_g3.xlsx")
openxlsx::write.xlsx(tokens_all_nc_g3, "Tokens_All_NC_g3.xlsx")

openxlsx::write.xlsx(palavras_pof_g4, "Descricoes_POF_g4.xlsx")
openxlsx::write.xlsx(palavras_snipc_g4, "Descricoes_SNIPC_g4.xlsx")
openxlsx::write.xlsx(tokens_all_g4, "Tokens_All_g4.xlsx")
openxlsx::write.xlsx(tokens_all_nc_g4, "Tokens_All_NC_g4.xlsx")


openxlsx::write.xlsx(palavras_pof_g5, "Descricoes_POF_g5.xlsx")
openxlsx::write.xlsx(palavras_snipc_g5, "Descricoes_SNIPC_g5.xlsx")
openxlsx::write.xlsx(tokens_all_g5, "Tokens_All_g5.xlsx")
openxlsx::write.xlsx(tokens_all_nc_g5, "Tokens_All_NC_g5.xlsx")

openxlsx::write.xlsx(palavras_pof_g6, "Descricoes_POF_g6.xlsx")
openxlsx::write.xlsx(palavras_snipc_g6, "Descricoes_SNIPC_g6.xlsx")
openxlsx::write.xlsx(tokens_all_g6, "Tokens_All_g6.xlsx")
openxlsx::write.xlsx(tokens_all_nc_g6, "Tokens_All_NC_g6.xlsx")

openxlsx::write.xlsx(palavras_pof_g7, "Descricoes_POF_g7.xlsx")
openxlsx::write.xlsx(palavras_snipc_g7, "Descricoes_SNIPC_g7.xlsx")
openxlsx::write.xlsx(tokens_all_g7, "Tokens_All_g7.xlsx")
openxlsx::write.xlsx(tokens_all_nc_g7, "Tokens_All_NC_g7.xlsx")

openxlsx::write.xlsx(palavras_pof_g8, "Descricoes_POF_g8.xlsx")
openxlsx::write.xlsx(palavras_snipc_g8, "Descricoes_SNIPC_g8.xlsx")
openxlsx::write.xlsx(tokens_all_g8, "Tokens_All_g8.xlsx")
openxlsx::write.xlsx(tokens_all_nc_g8, "Tokens_All_NC_g8.xlsx")

openxlsx::write.xlsx(palavras_pof_g9, "Descricoes_POF_g9.xlsx")
openxlsx::write.xlsx(palavras_snipc_g9, "Descricoes_SNIPC_g9.xlsx")
openxlsx::write.xlsx(tokens_all_g9, "Tokens_All_g9.xlsx")
openxlsx::write.xlsx(tokens_all_nc_g9, "Tokens_All_NC_g9.xlsx")

