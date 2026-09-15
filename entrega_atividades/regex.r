#bibliotecas
library(pdftools)
library(stringr)

#leitura do doc
tarefa <- pdf_text("./cadastro.pdf")
assunto <- str_split(tarefa, "\\n")
assunto <- Reduce(c,assunto)

#localizando e extraindo dados
pos_nome <- grep("nome", assunto, ignore.case = TRUE)
nomes <- sub("[Nn]ome:(.*)\\(aka.*", "\\1", assunto[pos_nome])
nomes <- trimws(nomes)

aka <- sub(".*aka(.*)\\)", "\\1", assunto[pos_nome])
aka <- trimws(aka)

pos_nasc <- grep(".*[Nn]asc.*:", assunto)
nascimento <- trimws(sub(".*[Nn]asc.*:(.*)$","\\1", assunto[pos_nasc]))

pos_endereço <- grep(".*Endereço.*", assunto)
endereço <- trimws(sub(".*Endereço:(.*?)(?:CEP.*)?$", "\\1", assunto[pos_endereço], perl=TRUE))

pos_cep <- grep("CEP", assunto)
cep <- trimws(sub(".*CEP:(.*)$", "\\1", assunto[pos_cep]))

pos_tel <- grep("Tel.*:", assunto)
tel <- trimws(sub(".*Tel.*:(.*)$", "\\1", assunto[pos_tel]))

pos_cpf <- grep("CPF", assunto, ignore.case=TRUE)
cpf <- trimws(sub(".*:(.*)$", "\\1", assunto[pos_cpf]))

#dataframe

dados_brutos <- data.frame(
  nome = nomes,
  apelido = aka,
  nascimento = nascimento,
  endereço = endereço,
  CEP = cep,
  telefone = tel,
  cpf = cpf,
  stringsAsFactors = FALSE
)

dados <- data.frame(
  nome = nomes,
  apelido = aka,
  nascimento = nascimento,
  endereço = endereço,
  CEP = cep,
  telefone = tel,
  cpf = cpf,
  stringsAsFactors = FALSE)

# Padronização
dados$CEP <- gsub("[^0-9]", "", dados$CEP)

dados$telefone <- gsub("[^0-9]", "", dados$telefone)

dados$cpf <- gsub("[^0-9]", "", dados$cpf)

dados$nascimento <- gsub("/", "-", dados$nascimento)
dados$nascimento <- gsub(
  "dec",
  "12",
  dados$nascimento,
  ignore.case = TRUE
)

View(dados)

