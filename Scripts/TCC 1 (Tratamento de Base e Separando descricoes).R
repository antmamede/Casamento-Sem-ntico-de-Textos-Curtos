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
base <- base %>% select(DESC_POF, DESC_SNIPC)

# Base feita pelo professor Eduardo
# base_edu <- read_xlsx("bd_Teste_full.xlsx")

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

## Filtrando valores unicos
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
palavras_pof <- base$DESC_POF
palavras_snipc <- base$DESC_SNIPC

palavras_pof <- unique(palavras_pof)
palavras_snipc <- unique(palavras_snipc)

palavras_pof <- str_sort(palavras_pof)
palavras_snipc <- str_sort(palavras_snipc)
palavras_pof <- data.frame("DESC_POF" = palavras_pof)
palavras_snipc <- data.frame("DESC_SNIPC" = palavras_snipc)

# ------------------------------------------------------------------------------
# Palavras nao contidas no embedding
# ------------------------------------------------------------------------------
palavras_embd <- read_xlsx("palavras_embd.xlsx")
names(palavras_embd) <- "Palavras"

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
tokens_pof_nc_df <- data.frame(DESC_POF = tokens_pof_nc)
tokens_snipc_nc_df <- data.frame(DESC_SNIPC = tokens_snipc_nc)

# Separa todos os tokens
tokens_all <- c(tokens_pof, tokens_snipc)
tokens_all <- unique(tokens_all)
tokens_all <- data.frame("Token" =  tokens_all)
tokens_all_nc <- c(tokens_pof_nc, tokens_snipc_nc)
tokens_all_nc <- unique(tokens_all_nc)
tokens_all_nc <- data.frame("Token" = tokens_all_nc)

# ------------------------------------------------------------------------------
# Removendo sentencas de palavra unica que nao esta contida no embedding
# ------------------------------------------------------------------------------
palavras_pof_ce <- palavras_pof %>% filter(!DESC_POF %in% tokens_pof_nc)
palavras_snipc_ce <- palavras_snipc %>% filter(!DESC_SNIPC %in% tokens_snipc_nc)
fora_pof <- palavras_pof %>% filter(!DESC_POF %in% palavras_pof_ce$DESC_POF)
fora_snipc <- palavras_snipc %>% filter(!DESC_SNIPC %in% palavras_snipc_ce$DESC_SNIPC)
# ------------------------------------------------------------------------------
# Salvando arquivos
# ------------------------------------------------------------------------------
setwd(path_bases)
openxlsx::write.xlsx(tokens_all, "Tokens_All.xlsx")
openxlsx::write.xlsx(tokens_all_nc, "Tokens_All_NC.xlsx")
openxlsx::write.xlsx(tokens_pof_nc_df, "Tokens_Nao_Contidos_Embeddings_POF.xlsx")
openxlsx::write.xlsx(tokens_snipc_nc_df, "Tokens_Nao_contidos_Embeddings_SNIPC.xlsx")
openxlsx::write.xlsx(palavras_pof, file = "Descricoes_POF.xlsx")
openxlsx::write.xlsx(palavras_snipc, file = "Descricoes_SNIPC.xlsx")
openxlsx::write.xlsx(palavras_pof_ce, file = "Descricoes_POF_CE.xlsx")
openxlsx::write.xlsx(palavras_snipc_ce, file = "Descricoes_SNIPC_CE.xlsx")
openxlsx::write.xlsx(base, "Base_Descricoes_V2.xlsx")
