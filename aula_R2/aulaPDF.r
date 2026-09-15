library(pdftools)
library(stringr)

conteudo <- "./Peru - Evento 4732.pdf"
conteudo <- pdf_text(conteudo)

#quebrando as linhas
conteudo <- str_split(conteudo, "\\n")

#juntando todas as linhas
conteudo <- Reduce(c, conteudo)

pos_out <- grep("OUTBREAK\\s+REFERENCE", conteudo)
pos_out <- c(pos_out, length(conteudo)+1)


i <- 1

conteudo_out <- conteudo[pos_out[i]:(pos_out[i + 1] - 1)]

#location

pos_loc <- grep("LOCATION\\s+Latitude", conteudo_out)
pos_afec <- grep("AFFECTED\\s+POPULATION\\s+DESCRIPTION", conteudo_out)
linha <- conteudo_out[pos_loc]
localizacao <- str_locate(linha, "LOCATION\\s+Latitude")
localizacao_lat <- str_locate(linha, "Latitude")

loc <- c()
jj <- 1

for(j in (pos_loc+1):pos_afec-1)){
    linha_s <- conteudo[j]

    if (str_detect(linha_s,"-?\\d+\\.\\d+\\s*,\\s*-?" ))
}
