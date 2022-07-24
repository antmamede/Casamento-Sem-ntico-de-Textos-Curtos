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

# Descricoes das Pesquisas
desc_pof <- read_xlsx("Descricoes_POF.xlsx")
desc_snipc <- read_xlsx("Descricoes_SNIPC.xlsx")

# TEP
tep <- read_xlsx("Base_TEP_2Sinonimos.xlsx")

# Embeddings
palavras_embd <- read_xlsx("palavras_embd.xlsx")
names(palavras_embd) <- "Palavras"


# ------------------------------------------------------------------------------
# Adicionando Sinonimos nas Strings
# ------------------------------------------------------------------------------
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

desc_pof_sinonimos <- sinonimos_tep(desc_pof, tep)
desc_snipc_sinonimos <- sinonimos_tep(desc_snipc, tep)
names(desc_pof_sinonimos) <- "DESC_POF"
names(desc_snipc_sinonimos) <- "DESC_SNIPC"

# ------------------------------------------------------------------------------
# Separando todos os Tokens
# ------------------------------------------------------------------------------

# Separando por espaco
tokens_pof <- desc_pof_sinonimos$DESC_POF
tokens_snipc <- desc_snipc_sinonimos$DESC_SNIPC
tokens_pof <- str_split(tokens_pof, " ")
tokens_snipc <- str_split(tokens_snipc, " ")

# Unlisting listas de palavras
tokens_pof <- unlist(tokens_pof)
tokens_snipc <- unlist(tokens_snipc)

# Unique
tokens_pof <- unique(tokens_pof)
tokens_snipc <- unique(tokens_snipc)

# Separa tokens nao contidos no embedding
tokens_pof_nc <- tokens_pof[which(!(tokens_pof %in% palavras_embd$Palavras))]
tokens_snipc_nc <- tokens_snipc[which(!(tokens_snipc %in% palavras_embd$Palavras))]

# Separa todos os tokens
tokens_all <- c(tokens_pof, tokens_snipc)
tokens_all <- unique(tokens_all)
tokens_all <- data.frame("Token" =  tokens_all)
tokens_all_nc <- c(tokens_pof_nc, tokens_snipc_nc)
tokens_all_nc <- unique(tokens_all_nc)
tokens_all_nc <- data.frame("Token" = tokens_all_nc)



# ------------------------------------------------------------------------------
# Salvando dados
# ------------------------------------------------------------------------------
openxlsx::write.xlsx(desc_pof_sinonimos, "Descricoes_POF_Sinonimos.xlsx")
openxlsx::write.xlsx(desc_snipc_sinonimos, "Descricoes_SNIPC_Sinonimos.xlsx")
openxlsx::write.xlsx(tokens_all, "Tokens_All_Sinonimos.xlsx")
openxlsx::write.xlsx(tokens_all_nc, "Tokens_All_NC_Sinonimos.xlsx")
