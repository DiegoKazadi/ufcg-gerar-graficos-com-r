install.packages("tidyr")
install.packages("stringr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readr)
library(tools)

###############################################################################

# Caminho base dos arquivos CSV
caminho_base <- "/home/diego/Documentos/Semestre 2024.2/Nova_Analise/dados/tabelas"

# Lista arquivos CSV no diretório
arquivos <- list.files(caminho_base, pattern = "\\.csv$", full.names = TRUE)

# Lista para armazenar os dataframes
tabelas <- list()

# Carregamento dos arquivos com verificação de delimitador
for (arquivo in arquivos) {
  nome_base <- file_path_sans_ext(basename(arquivo))
  
  tryCatch({
    df <- read_csv(arquivo, show_col_types = FALSE)
    
    # Se veio com só 1 coluna, tenta novamente com ponto e vírgula
    if (ncol(df) == 1) {
      message(paste("⚠️ Tabela", nome_base, "carregada com 1 coluna. Tentando com delimitador ';'..."))
      df <- read_delim(arquivo, delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
    }
    
    tabelas[[nome_base]] <- df
    message(paste("✔️ Arquivo carregado:", nome_base))
    
  }, error = function(e) {
    message(paste("❌ Erro ao carregar", arquivo, ":", e$message))
  })
}

message(paste("\nTotal de arquivos carregados:", length(tabelas)))

# Visualizar nomes das tabelas
print(names(tabelas))
readLines(file.path(caminho_base, "alunos-novos-filtrado-sem-metricas.csv"), n = 5)

# Visualizar número de colunas de cada tabela
print(sapply(tabelas, ncol))

################################################################################
### Etapas para padronizar nomes de variáveis
lapply(tabelas, names)

# Criar função para padronizar os nomes
padronizar_nomes <- function(df) {
  nomes <- names(df)
  nomes <- tolower(nomes)                       # minúsculas
  nomes <- gsub(" ", "_", nomes)                # espaços por underscores
  nomes <- gsub("[^a-z0-9_]", "", nomes)        # remove acentos e símbolos
  names(df) <- nomes
  return(df)
}


# Aplicar a função a todas as tabelas
tabelas <- lapply(tabelas, padronizar_nomes)
lapply(tabelas, names)

###############################################################################

corrigir_colunas_sem_nome <- function(df) {
  # Se os nomes forem todos vazios ou parecidos com dados, vamos assumir que os nomes não foram lidos
  if (all(is.na(names(df))) || any(grepl("^[0-9]{11}$", names(df)))) {
    # Extrair a primeira linha como nomes das colunas
    nomes <- as.character(df[1, ])
    # Substituir os nomes
    names(df) <- nomes
    # Remover a primeira linha apenas se ela foi usada como nome
    df <- df[-1, ]
  }
  return(df)
}

# Aplicando a correção só nas tabelas afetadas
nomes_corretos <- list(
  "cpf", "matricula_do_estudante", "periodo_de_ingresso", "forma_de_ingresso",
  "codigo_do_curriculo", "estado_civil", "sexo", "data_de_nascimento", "cor",
  "ano_de_conclusao_ensino_medio", "tipo_de_ensino_medio", "politica_afirmativa",
  "situacao", "motivo_de_evasao", "periodo_de_evasao"
)

# Atualizando apenas as tabelas com colunas erradas
tabelas$`alunos-novos-filtrado-sem-metricas` <- {
  df <- tabelas$`alunos-novos-filtrado-sem-metricas`
  names(df) <- nomes_corretos
  df
}

tabelas$`alunos-novos-sem-reingresso-filtrado-sem-metricas` <- {
  df <- tabelas$`alunos-novos-sem-reingresso-filtrado-sem-metricas`
  names(df) <- nomes_corretos
  df
}

tabelas$`alunos-novos-sem-reingressos-novos-ou-antigos-sem-metricas` <- {
  df <- tabelas$`alunos-novos-sem-reingressos-novos-ou-antigos-sem-metricas`
  names(df) <- nomes_corretos
  df
}

lapply(tabelas, names)

###############################################################################


print(ncol(tabelas[["alunos-novos-sem-reingresso-filtrado-sem-metricas"]]))

# Filtrar os ingressantes entre 2011.1 e 2017.2

# Função que verifica se uma coluna está presente
tem_colunas <- function(df, cols) {
  all(cols %in% names(df))
}

# Define o intervalo de interesse
periodos_ingresso <- c()
anos <- 2011:2017
semestres <- c(1, 2)
for (ano in anos) {
  for (semestre in semestres) {
    if (ano == 2017 && semestre == 2) break
    periodos_ingresso <- c(periodos_ingresso, as.integer(paste0(ano, semestre)))
  }
}
###############################################################################

# padronizar o nome da coluna matrcula para matricula_do_estudante
names(tabelas[["alunos-final"]])[names(tabelas[["alunos-final"]]) == "matrcula"] <- "matricula_do_estudante"
names(tabelas[["alunos-final"]])

# Etapas para tratar duplicação
# Função para deduplicar por CPF e por Matrícula

tratar_duplicacao <- function(tabela, nome_tabela) {
  cpf_col <- "cpf"
  matricula_col <- "matricula"  # ou "matricula_do_estudante"
  
  colunas_presentes <- names(tabela)
  
  # Ajuste para nomes alternativos de coluna
  if (!cpf_col %in% colunas_presentes && "CPF" %in% colunas_presentes) cpf_col <- "CPF"
  if (!matricula_col %in% colunas_presentes && "matricula_do_estudante" %in% colunas_presentes) {
    matricula_col <- "matricula_do_estudante"
  }
  
  resultados <- list()
  
  # Deduplicar por CPF
  if (cpf_col %in% names(tabela)) {
    dedup_cpf <- tabela %>% 
      arrange(!!sym(cpf_col)) %>%
      distinct(!!sym(cpf_col), .keep_all = TRUE)
    
    resultados[[paste0(nome_tabela, "_dedup_cpf")]] <- dedup_cpf
  }
  
  # Deduplicar por matrícula
  if (matricula_col %in% names(tabela)) {
    dedup_matricula <- tabela %>% 
      arrange(!!sym(matricula_col)) %>%
      distinct(!!sym(matricula_col), .keep_all = TRUE)
    
    resultados[[paste0(nome_tabela, "_dedup_matricula")]] <- dedup_matricula
  }
  
  return(resultados)
}

# licar a todas as tabelas

# Aplicar a função a todas as tabelas
tabelas_tratadas <- purrr::map2(
  tabelas,
  names(tabelas),
  tratar_duplicacao
)

# Como o resultado é uma lista de listas, vamos "achatar"
tabelas_tratadas_flat <- purrr::flatten(tabelas_tratadas)

# Ver quais tabelas temos agora
names(tabelas_tratadas_flat)

#  verificação

# Tabelas originais
tabelas_originais <- names(tabelas)

# Verificações de quais tabelas foram tratadas
tabelas_tratadas_nomes <- names(tabelas_tratadas_flat)

# Função para verificar se ambas versões (cpf e matricula) existem para cada tabela
verificar_tratamento <- function(nome) {
  cpf_ok <- paste0(nome, "_dedup_cpf") %in% tabelas_tratadas_nomes
  matricula_ok <- paste0(nome, "_dedup_matricula") %in% tabelas_tratadas_nomes
  data.frame(
    tabela = nome,
    cpf_tratado = cpf_ok,
    matricula_tratado = matricula_ok
  )
}

# Aplicar verificação a todas as tabelas
verificacoes <- purrr::map_dfr(tabelas_originais, verificar_tratamento)

# Mostrar resultado
print(verificacoes)

str(tabelas[["alunos-final"]])
head(tabelas[["alunos-final"]])


###############################################################################

# Renomear colunas, se necessário
colnames(tabelas[["alunos-novos-sem-reingressos-novos-ou-antigos-sem-metricas"]])[colnames(tabelas[["alunos-novos-sem-reingressos-novos-ou-antigos-sem-metricas"]]) == "perodo_de_ingresso"] <- "periodo_de_ingresso"
colnames(tabelas[["alunos-novos-sem-reingressos-novos-ou-antigos-sem-metricas"]])[colnames(tabelas[["alunos-novos-sem-reingressos-novos-ou-antigos-sem-metricas"]]) == "perodo_de_evaso"] <- "periodo_de_evasao"
colnames(tabelas[["alunos-novos-sem-reingressos-novos-ou-antigos-sem-metricas"]])[colnames(tabelas[["alunos-novos-sem-reingressos-novos-ou-antigos-sem-metricas"]]) == "matrcula"] <- "matricula_do_estudante"

library(dplyr)

# 🔧 Exibir todas as linhas (caso queira visualizar no View)
options(dplyr.print_max = Inf)

# 📅 Função para calcular o próximo período semestral
proximo_periodo <- function(periodo) {
  partes <- unlist(strsplit(periodo, "\\."))
  ano <- as.integer(partes[1])
  semestre <- as.integer(partes[2])
  if (semestre == 1) {
    return(paste0(ano, ".2"))
  } else {
    return(paste0(ano + 1, ".1"))
  }
}

# 📊 Função principal para calcular evasão ao final do primeiro período
evasao_primeiro_periodo <- function(df, inicio = "2011.1", fim = "2017.2") {
  # Tratar NA e converter para texto
  df$periodo_de_ingresso <- as.character(df$periodo_de_ingresso)
  df$periodo_de_evasao <- as.character(df$periodo_de_evasao)
  
  # Filtrar alunos no intervalo de ingresso
  df_filtrado <- df %>%
    filter(periodo_de_ingresso >= inicio & periodo_de_ingresso <= fim) %>%
    mutate(
      periodo_esperado_evasao = sapply(periodo_de_ingresso, proximo_periodo),
      evadiu_no_primeiro_periodo = (periodo_de_evasao == periodo_esperado_evasao)
    )
  
  # Agrupar e calcular total e evasão
  resumo <- df_filtrado %>%
    group_by(periodo_de_ingresso) %>%
    summarise(
      total_ingressantes = n(),
      total_evasao = sum(evadiu_no_primeiro_periodo, na.rm = TRUE),
      taxa_evasao = round(total_evasao / total_ingressantes, 4)
    ) %>%
    ungroup()
  
  return(resumo)
}

# 📁 Nome da tabela de interesse
nome_tabela <- "alunos-novos-sem-reingressos-novos-ou-antigos-sem-metricas"
df_tabela <- tabelas[[nome_tabela]]

# 🧪 Aplicar a função na tabela
resultado <- evasao_primeiro_periodo(df_tabela)

# 📋 Visualizar
print(paste("📘 Evasão para a tabela:", nome_tabela))
print(resultado)
View(resultado)

###############################################################################

# final do segundo período após o ingresso

library(dplyr)

# Função para avançar um período (semestre)
library(dplyr)

# Função para avançar um período
avancar_periodo <- function(periodo) {
  partes <- unlist(strsplit(periodo, "\\."))
  ano <- as.integer(partes[1])
  semestre <- as.integer(partes[2])
  if (semestre == 1) {
    return(paste0(ano, ".2"))
  } else {
    return(paste0(ano + 1, ".1"))
  }
}

# Função para avançar três períodos
avancar_tres_periodos <- function(periodo) {
  primeiro <- avancar_periodo(periodo)
  segundo <- avancar_periodo(primeiro)
  terceiro <- avancar_periodo(segundo)
  return(terceiro)
}

# Função para calcular evasão ao final do terceiro período após ingresso
evasao_terceiro_periodo <- function(df, inicio = "2011.1", fim = "2016.2") {
  df$periodo_de_ingresso <- as.character(df$periodo_de_ingresso)
  df$periodo_de_evasao <- as.character(df$periodo_de_evasao)
  
  df_filtrado <- df %>%
    filter(periodo_de_ingresso >= inicio & periodo_de_ingresso <= fim) %>%
    mutate(
      periodo_esperado_evasao = sapply(periodo_de_ingresso, avancar_tres_periodos),
      evadiu_no_terceiro_periodo = (periodo_de_evasao == periodo_esperado_evasao)
    )
  
  resumo <- df_filtrado %>%
    group_by(periodo_de_ingresso) %>%
    summarise(
      total_ingressantes = n(),
      total_evasao = sum(evadiu_no_terceiro_periodo, na.rm = TRUE),
      taxa_evasao = round(total_evasao / total_ingressantes, 4)
    ) %>%
    ungroup()
  
  return(resumo)
}

# Aplicar na tabela indicada
nome_tabela <- "alunos-novos-sem-reingressos-novos-ou-antigos-sem-metricas"
df_tabela <- tabelas[[nome_tabela]]

resultado_terceiro_periodo <- evasao_terceiro_periodo(df_tabela)

print(paste("📘 Evasão ao final do terceiro período para a tabela:", nome_tabela))
print(resultado_terceiro_periodo)
View(resultado_terceiro_periodo)



##############################################################################

# evasão ao final do quarto período:
library(dplyr)

# Função para avançar um período (semestre)
avancar_periodo <- function(periodo) {
  partes <- unlist(strsplit(periodo, "\\."))
  ano <- as.integer(partes[1])
  semestre <- as.integer(partes[2])
  if (semestre == 1) {
    return(paste0(ano, ".2"))
  } else {
    return(paste0(ano + 1, ".1"))
  }
}

# Função para avançar N períodos (usando a função avancar_periodo N vezes)
avancar_n_periodos <- function(periodo, n) {
  periodo_atual <- periodo
  for (i in seq_len(n)) {
    periodo_atual <- avancar_periodo(periodo_atual)
  }
  return(periodo_atual)
}

# Função para calcular evasão ao final do quarto período após ingresso
evasao_quarto_periodo <- function(df, inicio = "2011.1", fim = "2016.1") {
  df$periodo_de_ingresso <- as.character(df$periodo_de_ingresso)
  df$periodo_de_evasao <- as.character(df$periodo_de_evasao)
  
  df_filtrado <- df %>%
    filter(periodo_de_ingresso >= inicio & periodo_de_ingresso <= fim) %>%
    mutate(
      periodo_esperado_evasao = sapply(periodo_de_ingresso, avancar_n_periodos, n = 4),
      evadiu_no_quarto_periodo = (periodo_de_evasao == periodo_esperado_evasao)
    )
  
  resumo <- df_filtrado %>%
    group_by(periodo_de_ingresso) %>%
    summarise(
      total_ingressantes = n(),
      total_evasao = sum(evadiu_no_quarto_periodo, na.rm = TRUE),
      taxa_evasao = round(total_evasao / total_ingressantes, 4)
    ) %>%
    ungroup()
  
  return(resumo)
}

# Aplicar na tabela indicada
nome_tabela <- "alunos-novos-sem-reingressos-novos-ou-antigos-sem-metricas"
df_tabela <- tabelas[[nome_tabela]]

resultado_quarto_periodo <- evasao_quarto_periodo(df_tabela)

print(paste("📘 Evasão ao final do quarto período para a tabela:", nome_tabela))
print(resultado_quarto_periodo)
View(resultado_quarto_periodo)


##############################################################################

# Gráfico comparativo com ggplot2
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(tidyr)

# Função para calcular o N-ésimo período após o ingresso
proximo_n_periodo <- function(periodo, n = 1) {
  ano <- as.integer(sub("\\..*", "", periodo))
  semestre <- as.integer(sub(".*\\.", "", periodo))
  for (i in seq_len(n)) {
    if (semestre == 1) {
      semestre <- 2
    } else {
      semestre <- 1
      ano <- ano + 1
    }
  }
  paste0(ano, ".", semestre)
}

# Função genérica para calcular evasão ao final de N períodos
evasao_apos_n_periodos <- function(df, n_periodo, inicio, fim) {
  
  # Verifica se as colunas necessárias existem
  colunas_necessarias <- c("periodo_de_ingresso", "periodo_de_evasao", "cpf", "tipo_de_evasao", "status")
  if (!all(colunas_necessarias %in% colnames(df))) {
    warning("A tabela não possui todas as colunas necessárias e será ignorada.")
    return(NULL)
  }
  
  df <- df %>%
    mutate(
      periodo_de_ingresso = as.character(periodo_de_ingresso),
      periodo_de_evasao = as.character(periodo_de_evasao)
    ) %>%
    filter(periodo_de_ingresso >= inicio, periodo_de_ingresso <= fim) %>%
    mutate(
      periodo_esperado_evasao = sapply(periodo_de_ingresso, proximo_n_periodo, n = n_periodo),
      evadiu_no_periodo = (periodo_de_evasao == periodo_esperado_evasao & tipo_de_evasao != "-" & status == "INATIVO")
    ) %>%
    group_by(periodo_de_ingresso) %>%
    summarise(
      total_ingressantes = n(),
      total_evasao = sum(evadiu_no_periodo, na.rm = TRUE),
      taxa_evasao = round(100 * total_evasao / total_ingressantes, 2),
      .groups = "drop"
    )
  
  return(df)
}

# Definindo os currículos e seus intervalos
curriculos <- list(
  "1999" = list(inicio = "2011.1", fim = "2016.2"),
  "2017" = list(inicio = "2018.1", fim = "2022.3")
)

# Lista das tabelas
tabelas_nomes <- names(tabelas)

# Lista para armazenar resultados
dados_para_plot <- data.frame()

# Loop pelos currículos, períodos e tabelas
for (curriculo in names(curriculos)) {
  intervalo <- curriculos[[curriculo]]
  
  for (n in 1:4) {  # Do 1º ao 4º período
    for (nome_tabela in tabelas_nomes) {
      tabela <- tabelas[[nome_tabela]]
      resultado <- evasao_apos_n_periodos(tabela, n, intervalo$inicio, intervalo$fim)
      
      if (!is.null(resultado)) {
        resultado$periodo <- paste0(n, "º período")
        resultado$curriculo <- curriculo
        resultado$tabela <- nome_tabela
        
        dados_para_plot <- bind_rows(dados_para_plot, resultado)
      }
    }
  }
}

# Visualizar um pouco dos dados para conferir
head(dados_para_plot)

# Gráfico: boxplot taxa de evasão por período e currículo, para cada tabela
library(ggplot2)

for (tabela_nome in unique(dados_para_plot$tabela)) {
  dados_tabela <- dados_para_plot %>% filter(tabela == tabela_nome)
  
  ggplot(dados_tabela, aes(x = periodo, y = taxa_evasao, fill = curriculo)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Taxa de Evasão por Período e Currículo - Tabela:", tabela_nome),
         x = "Período após ingresso",
         y = "Taxa de Evasão (%)",
         fill = "Currículo") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) -> p
  
  print(p)
}
chooseCRANmirror()

