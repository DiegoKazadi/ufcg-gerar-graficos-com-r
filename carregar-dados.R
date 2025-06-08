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
colunas_necessarias <- c("periodo_de_ingresso", "periodo_de_evasao", "cpf", "tipo_de_evasao", "status")

for (nome in names(tabelas)) {
  colunas <- colnames(tabelas[[nome]])
  faltando <- setdiff(colunas_necessarias, colunas)
  if (length(faltando) > 0) {
    cat("🚫 Tabela:", nome, "não tem colunas:", paste(faltando, collapse = ", "), "\n")
  } else {
    cat("✅ Tabela:", nome, "OK\n")
  }
}


##############################################################################




# Gráfico comparativo com ggplot2
library(dplyr)
library(ggplot2)
library(tidyr)

# Funções auxiliares
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

avancar_n_periodos <- function(periodo, n) {
  periodo_atual <- periodo
  for (i in seq_len(n)) {
    periodo_atual <- avancar_periodo(periodo_atual)
  }
  return(periodo_atual)
}

# ✅ Função simplificada, compatível com seus dados
evasao_apos_n_periodos_simples <- function(df, n_periodo, inicio, fim) {
  colunas_necessarias <- c("periodo_de_ingresso", "periodo_de_evasao")
  if (!all(colunas_necessarias %in% colnames(df))) {
    return(NULL)
  }
  
  df <- df %>%
    mutate(
      periodo_de_ingresso = as.character(periodo_de_ingresso),
      periodo_de_evasao = as.character(periodo_de_evasao)
    ) %>%
    filter(periodo_de_ingresso >= inicio, periodo_de_ingresso <= fim) %>%
    mutate(
      periodo_esperado_evasao = sapply(periodo_de_ingresso, avancar_n_periodos, n = n_periodo),
      evadiu_no_periodo = (periodo_de_evasao == periodo_esperado_evasao)
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

# Currículos e intervalos
curriculos <- list(
  "1999" = list(inicio = "2011.1", fim = "2016.2"),
  "2017" = list(inicio = "2018.1", fim = "2022.3")
)

tabelas_nomes <- names(tabelas)
dados_para_plot <- data.frame()

# Loop principal
for (curriculo in names(curriculos)) {
  intervalo <- curriculos[[curriculo]]
  
  for (n in 1:4) {
    for (nome_tabela in tabelas_nomes) {
      tabela <- tabelas[[nome_tabela]]
      resultado <- evasao_apos_n_periodos_simples(tabela, n, intervalo$inicio, intervalo$fim)
      
      if (!is.null(resultado) && nrow(resultado) > 0) {
        resultado$periodo <- paste0(n, "º período")
        resultado$curriculo <- curriculo
        resultado$tabela <- nome_tabela
        dados_para_plot <- bind_rows(dados_para_plot, resultado)
      }
    }
  }
}

# ⚠️ Verificar se há dados
if (nrow(dados_para_plot) == 0) {
  stop("Nenhum dado foi gerado. Verifique as colunas das tabelas ou os intervalos.")
}

# 📊 Gráfico boxplot
ggplot(dados_para_plot, aes(x = periodo, y = taxa_evasao, fill = curriculo)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ tabela, scales = "free_y") +
  labs(
    title = "📉 Taxa de Evasão por Período e Currículo",
    x = "Período após ingresso",
    y = "Taxa de Evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0),
    legend.position = "top"
  )

##############################################################################
# Evolução Semestral da Evasão com
library(dplyr)
library(ggplot2)
library(tidyr)

# Funções auxiliares
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

avancar_n_periodos <- function(periodo, n) {
  periodo_atual <- periodo
  for (i in seq_len(n)) {
    periodo_atual <- avancar_periodo(periodo_atual)
  }
  return(periodo_atual)
}

# Função de evasão compatível com suas tabelas
evasao_apos_n_periodos_simples <- function(df, n_periodo, inicio, fim) {
  colunas_necessarias <- c("periodo_de_ingresso", "periodo_de_evasao")
  if (!all(colunas_necessarias %in% colnames(df))) {
    return(NULL)
  }
  
  df <- df %>%
    mutate(
      periodo_de_ingresso = as.character(periodo_de_ingresso),
      periodo_de_evasao = as.character(periodo_de_evasao)
    ) %>%
    filter(periodo_de_ingresso >= inicio, periodo_de_ingresso <= fim) %>%
    mutate(
      periodo_esperado_evasao = sapply(periodo_de_ingresso, avancar_n_periodos, n = n_periodo),
      evadiu_no_periodo = (periodo_de_evasao == periodo_esperado_evasao)
    ) %>%
    group_by(periodo_esperado_evasao) %>%
    summarise(
      total_ingressantes = n(),
      total_evasao = sum(evadiu_no_periodo, na.rm = TRUE),
      taxa_evasao = round(100 * total_evasao / total_ingressantes, 2),
      .groups = "drop"
    ) %>%
    mutate(
      periodo = paste0(n_periodo, "º período"),
      periodo_esperado_evasao = factor(periodo_esperado_evasao, levels = sort(unique(periodo_esperado_evasao)))
    )
  
  return(df)
}

# Intervalos por currículo
curriculos <- list(
  "1999" = list(inicio = "2011.1", fim = "2016.2"),
  "2017" = list(inicio = "2018.1", fim = "2022.3")
)

tabelas_nomes <- names(tabelas)
dados_linha <- data.frame()

# Loop principal
for (curriculo in names(curriculos)) {
  intervalo <- curriculos[[curriculo]]
  
  for (n in 1:4) {
    for (nome_tabela in tabelas_nomes) {
      tabela <- tabelas[[nome_tabela]]
      resultado <- evasao_apos_n_periodos_simples(tabela, n, intervalo$inicio, intervalo$fim)
      
      if (!is.null(resultado) && nrow(resultado) > 0) {
        resultado$curriculo <- curriculo
        resultado$tabela <- nome_tabela
        dados_linha <- bind_rows(dados_linha, resultado)
      }
    }
  }
}

# ⚠️ Verificação
if (nrow(dados_linha) == 0) {
  stop("Nenhum dado encontrado. Verifique os dados de entrada.")
}

# 📊 Gráfico de linha: evolução por período esperado de evasão
ggplot(dados_linha, aes(x = periodo_esperado_evasao, y = taxa_evasao, color = curriculo, group = interaction(curriculo, periodo))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ periodo, scales = "free_y") +
  labs(
    title = "📈 Evolução Semestral da Taxa de Evasão por Período Após Ingresso",
    x = "Período em que a evasão ocorreu",
    y = "Taxa de Evasão (%)",
    color = "Currículo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
##############################################################################
# Evasão por cor
library(dplyr)
library(ggplot2)
library(tidyr)

# Funções auxiliares
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

avancar_n_periodos <- function(periodo, n) {
  periodo_atual <- periodo
  for (i in seq_len(n)) {
    periodo_atual <- avancar_periodo(periodo_atual)
  }
  return(periodo_atual)
}

# Função com cor adicionada
evasao_por_cor <- function(df, n_periodo, inicio, fim) {
  colunas_necessarias <- c("periodo_de_ingresso", "periodo_de_evasao", "cor")
  if (!all(colunas_necessarias %in% colnames(df))) return(NULL)
  
  df <- df %>%
    mutate(
      periodo_de_ingresso = as.character(periodo_de_ingresso),
      periodo_de_evasao = as.character(periodo_de_evasao)
    ) %>%
    filter(periodo_de_ingresso >= inicio, periodo_de_ingresso <= fim) %>%
    mutate(
      periodo_esperado_evasao = sapply(periodo_de_ingresso, avancar_n_periodos, n = n_periodo),
      evadiu_no_periodo = (periodo_de_evasao == periodo_esperado_evasao)
    ) %>%
    group_by(cor, periodo_esperado_evasao) %>%
    summarise(
      total_ingressantes = n(),
      total_evasao = sum(evadiu_no_periodo, na.rm = TRUE),
      taxa_evasao = round(100 * total_evasao / total_ingressantes, 2),
      .groups = "drop"
    ) %>%
    mutate(
      periodo = paste0(n_periodo, "º período"),
      periodo_esperado_evasao = factor(periodo_esperado_evasao, levels = sort(unique(periodo_esperado_evasao)))
    )
  
  return(df)
}

# Intervalos por currículo
curriculos <- list(
  "1999" = list(inicio = "2011.1", fim = "2016.2"),
  "2017" = list(inicio = "2018.1", fim = "2022.3")
)

tabelas_nomes <- names(tabelas)
dados_cor <- data.frame()

# Loop principal por currículo e período
for (curriculo in names(curriculos)) {
  intervalo <- curriculos[[curriculo]]
  
  for (n in 1:4) {
    for (nome_tabela in tabelas_nomes) {
      tabela <- tabelas[[nome_tabela]]
      resultado <- evasao_por_cor(tabela, n, intervalo$inicio, intervalo$fim)
      
      if (!is.null(resultado) && nrow(resultado) > 0) {
        resultado$curriculo <- curriculo
        resultado$tabela <- nome_tabela
        dados_cor <- bind_rows(dados_cor, resultado)
      }
    }
  }
}

# Verificação
if (nrow(dados_cor) == 0) stop("Nenhum dado encontrado com variável cor. Verifique os dados de entrada.")

# 📊 Gráfico por cor
ggplot(dados_cor, aes(x = periodo_esperado_evasao, y = taxa_evasao, color = cor, group = interaction(cor, curriculo))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ periodo + curriculo, scales = "free_y") +
  labs(
    title = "📉 Evolução da Evasão por Cor/Raça dos Alunos",
    x = "Período em que a evasão ocorreu",
    y = "Taxa de Evasão (%)",
    color = "Cor/Raça"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

##############################################################################
# Histogramas das Taxas de Evasão por Currículo
# Evasão por cor
library(dplyr)
library(ggplot2)
library(tidyr)

# Funções auxiliares
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

avancar_n_periodos <- function(periodo, n) {
  periodo_atual <- periodo
  for (i in seq_len(n)) {
    periodo_atual <- avancar_periodo(periodo_atual)
  }
  return(periodo_atual)
}

# Função com cor adicionada
evasao_por_cor <- function(df, n_periodo, inicio, fim) {
  colunas_necessarias <- c("periodo_de_ingresso", "periodo_de_evasao", "cor")
  if (!all(colunas_necessarias %in% colnames(df))) return(NULL)
  
  df <- df %>%
    mutate(
      periodo_de_ingresso = as.character(periodo_de_ingresso),
      periodo_de_evasao = as.character(periodo_de_evasao)
    ) %>%
    filter(periodo_de_ingresso >= inicio, periodo_de_ingresso <= fim) %>%
    mutate(
      periodo_esperado_evasao = sapply(periodo_de_ingresso, avancar_n_periodos, n = n_periodo),
      evadiu_no_periodo = (periodo_de_evasao == periodo_esperado_evasao)
    ) %>%
    group_by(cor, periodo_esperado_evasao) %>%
    summarise(
      total_ingressantes = n(),
      total_evasao = sum(evadiu_no_periodo, na.rm = TRUE),
      taxa_evasao = round(100 * total_evasao / total_ingressantes, 2),
      .groups = "drop"
    ) %>%
    mutate(
      periodo = paste0(n_periodo, "º período"),
      periodo_esperado_evasao = factor(periodo_esperado_evasao, levels = sort(unique(periodo_esperado_evasao)))
    )
  
  return(df)
}

# Intervalos por currículo
curriculos <- list(
  "1999" = list(inicio = "2011.1", fim = "2016.2"),
  "2017" = list(inicio = "2018.1", fim = "2022.3")
)

tabelas_nomes <- names(tabelas)
dados_cor <- data.frame()

# Loop principal por currículo e período
for (curriculo in names(curriculos)) {
  intervalo <- curriculos[[curriculo]]
  
  for (n in 1:4) {
    for (nome_tabela in tabelas_nomes) {
      tabela <- tabelas[[nome_tabela]]
      resultado <- evasao_por_cor(tabela, n, intervalo$inicio, intervalo$fim)
      
      if (!is.null(resultado) && nrow(resultado) > 0) {
        resultado$curriculo <- curriculo
        resultado$tabela <- nome_tabela
        dados_cor <- bind_rows(dados_cor, resultado)
      }
    }
  }
}

# Verificação
if (nrow(dados_cor) == 0) stop("Nenhum dado encontrado com variável cor. Verifique os dados de entrada.")

ggplot(dados_cor, aes(x = periodo_esperado_evasao, y = taxa_evasao, fill = cor)) +
  geom_col(position = "dodge") +
  facet_wrap(~ periodo + curriculo, scales = "free_y") +
  labs(
    title = "📊 Taxa de Evasão por Cor/Raça dos Alunos por Período e Currículo",
    x = "Período em que a evasão ocorreu",
    y = "Taxa de Evasão (%)",
    fill = "Cor/Raça"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )
##############################################################################

library(ggplot2)
library(dplyr)

# Assumindo que `dados_linha` já está disponível do código anterior, com colunas:
# taxa_evasao, curriculo, periodo, tabela, periodo_esperado_evasao

# Histograma para cada currículo, separando os períodos (1º, 2º, 3º, 4º)
ggplot(dados_linha, aes(x = taxa_evasao, fill = curriculo)) +
  geom_histogram(binwidth = 2, alpha = 0.7, position = "identity", color = "black") +
  facet_wrap(~ curriculo + periodo, scales = "free_y") +
  labs(
    title = "📊 Distribuição das Taxas de Evasão por Currículo e Período",
    x = "Taxa de Evasão (%)",
    y = "Frequência",
    fill = "Currículo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


##############################################################################
# Gráfico de Barras Lado a Lado

library(dplyr)
library(ggplot2)
library(tidyr)

# Funções auxiliares - reaproveitamos do código anterior
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

avancar_n_periodos <- function(periodo, n) {
  periodo_atual <- periodo
  for (i in seq_len(n)) {
    periodo_atual <- avancar_periodo(periodo_atual)
  }
  return(periodo_atual)
}

evasao_apos_n_periodos_simples <- function(df, n_periodo, inicio, fim) {
  colunas_necessarias <- c("periodo_de_ingresso", "periodo_de_evasao")
  if (!all(colunas_necessarias %in% colnames(df))) {
    return(NULL)
  }
  
  df <- df %>%
    mutate(
      periodo_de_ingresso = as.character(periodo_de_ingresso),
      periodo_de_evasao = as.character(periodo_de_evasao)
    ) %>%
    filter(periodo_de_ingresso >= inicio, periodo_de_ingresso <= fim) %>%
    mutate(
      periodo_esperado_evasao = sapply(periodo_de_ingresso, avancar_n_periodos, n = n_periodo),
      evadiu_no_periodo = (periodo_de_evasao == periodo_esperado_evasao)
    ) %>%
    group_by(periodo_esperado_evasao) %>%
    summarise(
      total_ingressantes = n(),
      total_evasao = sum(evadiu_no_periodo, na.rm = TRUE),
      taxa_evasao = round(100 * total_evasao / total_ingressantes, 2),
      .groups = "drop"
    ) %>%
    mutate(
      periodo = paste0(n_periodo, "º período"),
      periodo_esperado_evasao = factor(periodo_esperado_evasao, levels = sort(unique(periodo_esperado_evasao)))
    )
  
  return(df)
}

# Intervalos dos currículos
curriculos <- list(
  "1999" = list(inicio = "2011.1", fim = "2016.2"),
  "2017" = list(inicio = "2018.1", fim = "2022.3")
)

tabelas_nomes <- names(tabelas)
dados_barras <- data.frame()

# Montagem dos dados
for (curriculo in names(curriculos)) {
  intervalo <- curriculos[[curriculo]]
  
  for (n in 1:4) {
    for (nome_tabela in tabelas_nomes) {
      tabela <- tabelas[[nome_tabela]]
      resultado <- evasao_apos_n_periodos_simples(tabela, n, intervalo$inicio, intervalo$fim)
      
      if (!is.null(resultado) && nrow(resultado) > 0) {
        resultado$curriculo <- curriculo
        resultado$tabela <- nome_tabela
        dados_barras <- bind_rows(dados_barras, resultado)
      }
    }
  }
}

# Verificação
if (nrow(dados_barras) == 0) {
  stop("Nenhum dado encontrado para o gráfico.")
}

# Gráfico de barras agrupadas (lado a lado)
ggplot(dados_barras, aes(x = periodo_esperado_evasao, y = taxa_evasao, fill = curriculo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ periodo, scales = "free_y") +
  labs(
    title = "📊 Taxa de Evasão por Semestre e Período Após Ingresso",
    x = "Período em que a evasão ocorreu",
    y = "Taxa de Evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
##############################################################################
# Liste as colunas do seu dataset
colnames(dados)

# Estratificação
library(dplyr)
library(ggplot2)
library(dplyr)
library(dplyr)
library(stringr)

# Função para converter semestre do tipo "2011.1" para número sequencial para facilitar cálculo
semestre_to_num <- function(semestre) {
  ano <- as.integer(str_extract(semestre, "^\\d{4}"))
  parte <- as.integer(str_extract(semestre, "\\.(\\d)$", group = 1))
  return(ano * 2 + parte)
}

# Função para converter número sequencial para semestre "2011.1"
num_to_semestre <- function(num) {
  ano <- num %/% 2
  parte <- num %% 2
  parte <- ifelse(parte == 0, 2, 1)
  ano <- ifelse(parte == 2, ano - 1, ano)
  return(paste0(ano, ".", parte))
}

# Função para calcular evasão por período após ingresso
calcular_evasao_periodos <- function(dados, curriculo, inicio, fim) {
  
  # Filtra currículo e intervalo ingresso
  dados_filtrados <- dados %>%
    filter(currculo == curriculo) %>%
    filter(periodo_de_ingresso >= inicio, periodo_de_ingresso <= fim) %>%
    filter(status %in% c("ATIVO", "INATIVO")) %>%
    filter(!(status == "INATIVO" & tipo_de_evaso == "GRADUADO"))
  
  # Converte semestres para números para cálculo
  dados_filtrados <- dados_filtrados %>%
    mutate(
      semestre_ingresso_num = semestre_to_num(as.character(periodo_de_ingresso)),
      semestre_ultimo = semestre_to_num(as.character(periodo_de_evasao))
      
    )
  
  # Definir período máximo para cálculo da evasão
  max_periodos <- 4
  
  resultados <- list()
  
  for (p in 1:max_periodos) {
    # Para cada aluno, calcular semestre final do período p após ingresso
    dados_filtrados <- dados_filtrados %>%
      mutate(
        semestre_aval = semestre_ingresso_num + p
      )
    
    # Marcar evasão no período avaliado: INATIVO com ultimo_periodo <= semestre_aval
    dados_filtrados <- dados_filtrados %>%
      mutate(
        evadiu_ate_p = case_when(
          status == "INATIVO" & semestre_ultimo <= semestre_aval ~ 1,
          TRUE ~ 0
        )
      )
    
    # Agrupar por faixa_idade (ou outra variável)
    dados_filtrados <- dados_filtrados %>%
      mutate(faixa_idade = case_when(
        idade_aproximada_no_ingresso < 20 ~ "<20",
        idade_aproximada_no_ingresso >= 20 & idade_aproximada_no_ingresso <= 24 ~ "20-24",
        idade_aproximada_no_ingresso >= 25 & idade_aproximada_no_ingresso <= 29 ~ "25-29",
        idade_aproximada_no_ingresso >= 30 ~ ">=30",
        TRUE ~ NA_character_
      ))
    
    stats <- dados_filtrados %>%
      group_by(periodo_de_ingresso, faixa_idade) %>%
      summarise(
        total = n(),
        evasao = sum(evadiu_ate_p),
        taxa_evasao = evasao / total,
        media = mean(evadiu_ate_p),
        desvio_padrao = sd(evadiu_ate_p)
      ) %>%
      mutate(periodo = p) %>%
      ungroup()
    
    resultados[[p]] <- stats
  }
  
  # Combina resultados dos 4 períodos
  resultado_final <- bind_rows(resultados)
  return(resultado_final)
}

# Parâmetros por currículo
curriculos <- list(
  '1999' = list(inicio = "2011.1", fim = "2016.2"),
  '2017' = list(inicio = "2018.1", fim = "2022.3")
)

# Exemplo de uso para currículo 1999
resultados_1999 <- calcular_evasao_periodos(dados, "1999", curriculos[['1999']][['inicio']], curriculos[['1999']][['fim']])
print(resultados_1999)

# Exemplo para currículo 2017
resultados_2017 <- calcular_evasao_periodos(dados, "2017", curriculos[['2017']][['inicio']], curriculos[['2017']][['fim']])
print(resultados_2017)

#####################################

library(dplyr)
library(stringr)
library(ggplot2)

# Função semestre_to_num e num_to_semestre (sua versão já OK)

# Função calcular_evasao_periodos (igual a sua)

# Currículos (igual a sua)

# Calcular resultados para 1999 e 2017 (igual a sua)

# --- Gráfico para o currículo 1999 ---

# Adiciona coluna para texto de faixa etária ordenada
resultados_1999 <- resultados_1999 %>%
  mutate(faixa_idade = factor(faixa_idade, levels = c("<20", "20-24", "25-29", ">=30")),
         periodo = factor(periodo, levels = 1:4, labels = paste0(1:4, "º Período")))

# Gráfico de taxa média de evasão por período e faixa etária
ggplot(resultados_1999, aes(x = periodo, y = media, group = faixa_idade, color = faixa_idade)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = pmax(0, media - desvio_padrao), ymax = pmin(1, media + desvio_padrao), fill = faixa_idade), alpha = 0.2, color = NA) +
  facet_wrap(~ faixa_idade) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  labs(
    title = "Taxa Média de Evasão por Período (Currículo 1999)",
    x = "Período após ingresso",
    y = "Taxa média de evasão (%)",
    color = "Faixa Etária",
    fill = "Faixa Etária"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

# Se quiser gerar para o currículo 2017, é só repetir a mesma lógica:

resultados_2017 <- resultados_2017 %>%
  mutate(faixa_idade = factor(faixa_idade, levels = c("<20", "20-24", "25-29", ">=30")),
         periodo = factor(periodo, levels = 1:4, labels = paste0(1:4, "º Período")))

ggplot(resultados_2017, aes(x = periodo, y = media, group = faixa_idade, color = faixa_idade)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = pmax(0, media - desvio_padrao), ymax = pmin(1, media + desvio_padrao), fill = faixa_idade), alpha = 0.2, color = NA) +
  facet_wrap(~ faixa_idade) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  labs(
    title = "Taxa Média de Evasão por Período (Currículo 2017)",
    x = "Período após ingresso",
    y = "Taxa média de evasão (%)",
    color = "Faixa Etária",
    fill = "Faixa Etária"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

##############################################################################

library(dplyr)
library(stringr)
library(ggplot2)
library(scales)  # Para formatar eixo y em %

# Função para converter semestre "2011.1" -> número sequencial
semestre_to_num <- function(semestre) {
  ano <- as.integer(str_extract(semestre, "^\\d{4}"))
  parte <- as.integer(str_extract(semestre, "\\.(\\d)$", group = 1))
  return(ano * 2 + parte)
}

# Função para converter número sequencial para semestre "2011.1"
num_to_semestre <- function(num) {
  ano <- num %/% 2
  parte <- num %% 2
  parte <- ifelse(parte == 0, 2, 1)
  ano <- ifelse(parte == 2, ano - 1, ano)
  return(paste0(ano, ".", parte))
}

# Função para calcular evasão por períodos após ingresso
calcular_evasao_periodos <- function(dados, curriculo, inicio, fim) {
  
  dados_filtrados <- dados %>%
    filter(currculo == curriculo) %>%
    filter(periodo_de_ingresso >= inicio, periodo_de_ingresso <= fim) %>%
    filter(status %in% c("ATIVO", "INATIVO")) %>%
    filter(!(status == "INATIVO" & tipo_de_evaso == "GRADUADO")) %>%
    mutate(
      semestre_ingresso_num = semestre_to_num(as.character(periodo_de_ingresso)),
      semestre_ultimo = semestre_to_num(as.character(periodo_de_evasao)),
      faixa_idade = case_when(
        idade_aproximada_no_ingresso < 20 ~ "<20",
        idade_aproximada_no_ingresso >= 20 & idade_aproximada_no_ingresso <= 24 ~ "20-24",
        idade_aproximada_no_ingresso >= 25 & idade_aproximada_no_ingresso <= 29 ~ "25-29",
        idade_aproximada_no_ingresso >= 30 ~ ">=30",
        TRUE ~ NA_character_
      )
    )
  
  max_periodos <- 4
  resultados <- list()
  
  for (p in 1:max_periodos) {
    dados_p <- dados_filtrados %>%
      mutate(
        semestre_aval = semestre_ingresso_num + p,
        evadiu_ate_p = ifelse(status == "INATIVO" & semestre_ultimo <= semestre_aval, 1, 0)
      ) %>%
      group_by(periodo_de_ingresso, faixa_idade) %>%
      summarise(
        total = n(),
        evasao = sum(evadiu_ate_p),
        taxa_evasao = evasao / total,
        media = mean(evadiu_ate_p),
        desvio_padrao = sd(evadiu_ate_p),
        .groups = "drop"
      ) %>%
      mutate(periodo = p)
    
    resultados[[p]] <- dados_p
  }
  
  bind_rows(resultados)
}

# Parâmetros currículo 1999
curriculos <- list(
  '1999' = list(inicio = "2011.1", fim = "2016.2")
)

# Calcular evasão currículo 1999
resultados_1999 <- calcular_evasao_periodos(dados, "1999", curriculos[['1999']][['inicio']], curriculos[['1999']][['fim']])

# Preparar dados para gráfico
resultados_1999 <- resultados_1999 %>%
  mutate(
    faixa_idade = factor(faixa_idade, levels = c("<20", "20-24", "25-29", ">=30")),
    periodo = factor(periodo, levels = 1:4, labels = paste0(1:4, "º Período"))
  )

# Gráfico de evasão
ggplot(resultados_1999, aes(x = periodo, y = media, group = faixa_idade, color = faixa_idade)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = pmax(0, media - desvio_padrao), ymax = pmin(1, media + desvio_padrao), fill = faixa_idade), alpha = 0.2, color = NA) +
  facet_wrap(~ faixa_idade) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,1)) +
  labs(
    title = "Taxa Média de Evasão por Período (Currículo 1999)",
    x = "Período após ingresso",
    y = "Taxa média de evasão (%)",
    color = "Faixa Etária",
    fill = "Faixa Etária"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

##############################################################################
library(dplyr)
library(ggplot2)
library(stringr)

library(dplyr)
library(ggplot2)
library(stringr)

# 1. Carregar os dados (se ainda não estiver carregado)
# dados <- read.csv("CAMINHO/ARQUIVO.csv", stringsAsFactors = FALSE)

# 2. Filtrar a tabela desejada
dados_filtrados <- dados %>%
  filter(status == "INATIVO", tipo_de_evaso != "GRADUADO") %>%
  filter(!is.na(periodo_de_ingresso), !is.na(periodo_de_evasao), !is.na(sexo), sexo != "")

# 3. Função para transformar semestre em número
semestre_para_num <- function(semestre) {
  ano <- as.integer(str_extract(semestre, "^\\d{4}"))
  periodo <- as.integer(str_extract(semestre, "\\.(\\d)$"))
  return(ano * 2 + periodo)
}

# 4. Calcular períodos até evasão
dados_filtrados <- dados_filtrados %>%
  mutate(
    ingresso_num = semestre_para_num(as.character(periodo_de_ingresso)),
    evasao_num = semestre_para_num(as.character(periodo_de_evasao)),
    periodos_ate_evasao = evasao_num - ingresso_num
  ) %>%
  filter(periodos_ate_evasao >= 0, periodos_ate_evasao <= 12)

# 5. Verificar se há dados
if(nrow(dados_filtrados) == 0){
  stop("Nenhum dado disponível após o filtro. Verifique se há registros com status INATIVO e tipo_de_evaso diferente de GRADUADO.")
}

# 6. Gerar gráfico boxplot
ggplot(dados_filtrados, aes(x = sexo, y = periodos_ate_evasao, fill = sexo)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.6) +
  labs(
    title = "Tempo até Evasão por Sexo",
    subtitle = "Alunos Inativos (exceto Graduados)",
    x = "Sexo",
    y = "Períodos até a Evasão"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")


##############################################################################
# Estratificar
library(dplyr)
library(stringr)

# 1. Converter semestre para número
semestre_para_num <- function(semestre) {
  ano <- as.integer(str_extract(semestre, "^\\d{4}"))
  semestre_num <- as.integer(str_extract(semestre, "\\.(\\d)$"))
  return(ano * 2 + semestre_num)
}

# 2. Filtrar e tratar os dados
dados_evasao <- dados %>%
  filter(status == "INATIVO", tipo_de_evaso != "GRADUADO") %>%
  filter(!is.na(periodo_de_ingresso), !is.na(periodo_de_evasao), !is.na(sexo)) %>%
  mutate(
    ingresso_num = semestre_para_num(as.character(periodo_de_ingresso)),
    evasao_num = semestre_para_num(as.character(periodo_de_evasao)),
    periodos_ate_evasao = evasao_num - ingresso_num,
    faixa_idade = case_when(
      idade_aproximada_no_ingresso < 20 ~ "<20",
      idade_aproximada_no_ingresso <= 24 ~ "20-24",
      idade_aproximada_no_ingresso <= 29 ~ "25-29",
      idade_aproximada_no_ingresso >= 30 ~ ">=30",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(periodos_ate_evasao >= 0, periodos_ate_evasao <= 12)

# 3. Cálculo das estatísticas
estatisticas <- dados_evasao %>%
  group_by(sexo, faixa_idade, cota) %>%
  summarise(
    n = n(),
    media_periodos = mean(periodos_ate_evasao, na.rm = TRUE),
    mediana_periodos = median(periodos_ate_evasao, na.rm = TRUE),
    desvio_padrao = sd(periodos_ate_evasao, na.rm = TRUE)
  ) %>%
  arrange(desc(n))

# 4. Visualizar resultado
print(estatisticas)


###########################################################################
# Gráfico de barras com média por grupo
library(ggplot2)

dados_evasao %>%
  filter(!is.na(faixa_idade), !is.na(sexo), !is.na(cota)) %>%
  mutate(cota = ifelse(is.na(cota) | cota == "-", "Não cotista", cota)) %>%
  group_by(sexo, faixa_idade, cota) %>%
  summarise(media = mean(periodos_ate_evasao), n = n()) %>%
  filter(n >= 10) %>%
  ggplot(aes(x = faixa_idade, y = media, fill = sexo)) +
  geom_col(position = "dodge") +
  facet_wrap(~ cota) +
  labs(title = "Média de períodos até evasão", y = "Média de períodos", x = "Faixa etária") +
  theme_minimal()


#############################################################################
# Boxplot de períodos até evasão por sexo e cota

dados_evasao %>%
  filter(!is.na(sexo), !is.na(cota)) %>%
  mutate(cota = ifelse(is.na(cota) | cota == "-", "Não cotista", cota)) %>%
  ggplot(aes(x = cota, y = periodos_ate_evasao, fill = sexo)) +
  geom_boxplot() +
  labs(title = "Distribuição dos períodos até evasão por sexo e cota", y = "Períodos até evasão", x = "Cota") +
  theme_minimal()

###########################################################################
# Estado civil e cor
# Carregar pacote dplyr para manipulação dos dados
library(dplyr)

# Criar variável periodos_cursados (diferença entre período de evasão e ingresso)
dados <- dados %>%
  mutate(
    periodos_cursados = as.numeric(periodo_de_evasao) - as.numeric(periodo_de_ingresso)
  )

# Calcular estatísticas descritivas estratificadas por cor e estado civil
estatisticas <- dados %>%
  filter(!is.na(periodos_cursados), !is.na(cor), !is.na(estado_civil)) %>%
  group_by(cor, estado_civil) %>%
  summarise(
    n = n(),
    media = mean(periodos_cursados, na.rm = TRUE),
    mediana = median(periodos_cursados, na.rm = TRUE),
    desvio_padrao = sd(periodos_cursados, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

# Mostrar a tabela com as estatísticas
print(estatisticas)


# Carregar pacote ggplot2 para visualização
install.packages("viridis")

library(ggplot2)
library(viridis)

ggplot(estatisticas, aes(x = cor, y = media, fill = estado_civil)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(
    title = "Média de períodos cursados por cor e estado civil",
    x = "Cor/Raça",
    y = "Média de Períodos Cursados",
    fill = "Estado Civil"
  ) +
  theme_minimal()

###########################################################################
library(dplyr)

estatisticas <- dados %>%
  filter(status == "INATIVO", !is.na(cor), !is.na(estado_civil), !is.na(currculo)) %>%
  group_by(cor, estado_civil, currculo) %>%
  summarise(
    media = mean(periodos_cursados, na.rm = TRUE),
    n = n()
  ) %>%
  filter(n >= 5)  # opcional: remover grupos muito pequenos


###

library(ggplot2)

ggplot(estatisticas, aes(x = cor, y = media, fill = estado_civil)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c(
    "Solteiro(a)" = "#A6CEE3",
    "Casado(a)" = "#B2DF8A",
    "Divorciado(a)" = "#FB9A99",
    "Viúvo(a)" = "#FDBF6F",
    "Separado(a)" = "#CAB2D6"
  )) +
  labs(
    title = "Média de períodos cursados até evasão por cor, estado civil e currículo",
    x = "Cor/Raça",
    y = "Média de Períodos Cursados",
    fill = "Estado Civil"
  ) +
  facet_wrap(~ currculo) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

ggplot(estatisticas, aes(x = cor, y = media, fill = estado_civil)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c(
    "Solteiro(a)" = "#A6CEE3",
    "Casado(a)" = "#B2DF8A",
    "Divorciado(a)" = "#FB9A99",
    "Viúvo(a)" = "#FDBF6F",
    "Separado(a)" = "#CAB2D6"
  )) +
  labs(
    title = "Média de períodos cursados por cor e estado civil",
    x = "Cor/Raça",
    y = "Média de Períodos Cursados",
    fill = "Estado Civil"
  ) +
  theme_minimal()


##############################################################################
# Teste t de Student para comparar a média de idade entre alunos evadidos e não evadidos
# Verifique os nomes e valores corretos das colunas em seu dataset
library(dplyr)

# Filtrar apenas ATIVO e INATIVO, excluir GRADUADO
dados_filtrados <- dados %>%
  filter(status %in% c("ATIVO", "INATIVO")) %>%
  mutate(evasao_bin = ifelse(status == "INATIVO", 1, 0))

# Teste t para idade entre evadidos e não evadidos
resultado_ttest <- t.test(idade_aproximada_no_ingresso ~ evasao_bin, data = dados_filtrados, na.action = na.omit)

print(resultado_ttest)

##############################################################################

# Criar variável binária para evasão: 1 = evadiu, 0 = não evadiu
dados <- dados %>%
  mutate(evasao_bin = ifelse(status == "INATIVO", 1, 0)) # Considera "INATIVO" como evadido

# Função para rodar e imprimir o teste qui-quadrado para uma variável categórica
rodar_teste_chi <- function(variavel) {
  cat("\nTeste Qui-quadrado para evasão vs", variavel, "\n")
  
  tabela <- table(dados[[variavel]], dados$evasao_bin)
  print(tabela)
  
  resultado <- chisq.test(tabela)
  print(resultado)
}

# Rodar para as variáveis desejadas
rodar_teste_chi("sexo")
rodar_teste_chi("estado_civil")
rodar_teste_chi("cor")


##############################################################################
# Teste Fisher
# Crie uma nova coluna cor_agrupada
dados$cor_agrupada <- as.character(dados$cor)
dados$cor_agrupada[dados$cor_agrupada %in% c("Amarela", "Indígena", "Não declarada")] <- "Outros"

# Refaça a tabela
tabela_agrupada <- table(dados$cor_agrupada, dados$evasao_bin)

# Rode Fisher Test na tabela menor
fisher.test(tabela_agrupada)

##############################################################################

# As taxas de evasão Cálculo de média, mediana e desvio padrão
library(dplyr)

estatisticas_taxas <- dados_linha %>%
  group_by(curriculo) %>%
  summarise(
    media = mean(taxa_evasao, na.rm = TRUE),
    mediana = median(taxa_evasao, na.rm = TRUE),
    desvio_padrao = sd(taxa_evasao, na.rm = TRUE),
    .groups = "drop"
  )
print(estatisticas_taxas)

##############################################################################
# Cálculo de assimetria (skewness)
# Instalar se necessário
install.packages("moments")
library(moments)

assimetria <- dados_linha %>%
  group_by(curriculo) %>%
  summarise(
    skewness = skewness(taxa_evasao, na.rm = TRUE),
    .groups = "drop"
  )
print(assimetria)

##############################################################################

estatisticas_completas <- dados_linha %>%
  group_by(curriculo) %>%
  summarise(
    media = mean(taxa_evasao, na.rm = TRUE),
    mediana = median(taxa_evasao, na.rm = TRUE),
    desvio_padrao = sd(taxa_evasao, na.rm = TRUE),
    skewness = skewness(taxa_evasao, na.rm = TRUE),
    .groups = "drop"
  )
print(estatisticas_completas)

###############################################################################
# Geração de Boxplot Comparativo por Currículo e Período
library(ggplot2)
library(dplyr)

# Supondo que você já tenha o `dados_linha` com as colunas:
# curriculo, periodo, taxa_evasao

# Cores diferenciadas para os currículos
cores_curriculo <- c("1999" = "#FF9999", "2017" = "#66C2A5")

# Criar boxplot
ggplot(dados_linha, aes(x = periodo, y = taxa_evasao, fill = curriculo)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "black", outlier.color = "black") +
  scale_fill_manual(values = cores_curriculo) +
  labs(
    title = "Distribuição das Taxas de Evasão por Currículo e Período",
    x = "Período após o ingresso",
    y = "Taxa de Evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

#############################################################################
# Gráfico de Linhas da Evolução das Taxas de Evasão
# Visualizar as primeiras linhas da base
head(dados_linha)
# Visualizar estrutura da base
glimpse(dados_linha)

library(ggplot2)

ggplot(dados_linha, aes(x = periodo_esperado_evasao, y = taxa_evasao, color = curriculo, group = curriculo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolução das Taxas de Evasão por Currículo (Semestre a Semestre)",
    x = "Período Esperado de Evasão",
    y = "Taxa de Evasão (%)",
    color = "Currículo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("1999" = "#F8766D", "2017" = "#00BA38"))


############################################################################
# Histogramas das Taxas de Evasão por Currículo
library(ggplot2)

# Supondo que os dados estejam no mesmo data frame `dados_linha`
# com colunas: 'taxa_evasao' e 'curriculo'

ggplot(dados_linha, aes(x = taxa_evasao, fill = curriculo)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 15) +
  facet_wrap(~ curriculo) +
  labs(
    title = "Distribuição das Taxas de Evasão por Currículo",
    x = "Taxa de Evasão (%)",
    y = "Frequência",
    fill = "Currículo"
  ) +
  scale_fill_manual(values = c("1999" = "#F8766D", "2017" = "#00BA38")) +
  theme_minimal()


###########################################################################
#  Gráfico de Barras por Semestre (lado a lado por currículo)
library(ggplot2)

ggplot(dados_linha, aes(x = periodo_esperado_evasao, y = taxa_evasao, fill = curriculo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Gráfico de Barras das Taxas de Evasão por Semestre",
    x = "Semestre de ocorrência da evasão",
    y = "Taxa de Evasão (%)",
    fill = "Currículo"
  ) +
  scale_fill_manual(values = c("1999" = "#F8766D", "2017" = "#00BA38")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############################################################################
# Filtrar os quatro primeiros períodos
periodos_validos <- c("1º período", "2º período", "3º período", "4º período")
dados_filtrados <- subset(dados_linha, periodo %in% periodos_validos)

# Gerar o gráfico de barras com facetas por período
library(ggplot2)

ggplot(dados_filtrados, aes(x = curriculo, y = taxa_evasao, fill = curriculo)) +
  geom_bar(stat = "identity", width = 0.6) +
  facet_wrap(~periodo, nrow = 1) +
  labs(
    title = "Taxa de Evasão por Currículo nos Quatro Primeiros Períodos",
    x = "Currículo",
    y = "Taxa de Evasão (%)"
  ) +
  scale_fill_manual(values = c("1999" = "#F8766D", "2017" = "#00BA38")) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  )


library(ggplot2)
library(dplyr)

# Filtrar os quatro primeiros períodos
periodos_validos <- c("1º período", "2º período", "3º período", "4º período")
dados_filtrados <- subset(dados_linha, periodo %in% periodos_validos)

# Calcular total de evasão por curriculo e periodo
# Supondo que 'taxa_evasao' seja taxa (em %) e 'total_evasao' seja a contagem absoluta (se não tiver, ajusta aqui)
# Se você não tiver a contagem absoluta, precisará agregar uma variável que represente o total.
# Aqui vamos supor que 'evasao' é um campo que indica a contagem de evasão para cada linha.

# Se não tiver 'evasao', considere criar um resumo baseado nos dados existentes, exemplo:
# dados_summarized <- dados_filtrados %>%
#   group_by(curriculo, periodo) %>%
#   summarise(taxa_evasao = mean(taxa_evasao),
#             total_evasao = sum(contagem_evasao)) # 'contagem_evasao' deve existir ou criar

# Como você não detalhou a variável total, vou supor que tem um campo 'total_evasao' para texto

# Vamos criar um dataset resumo para mostrar as labels do total
dados_resumo <- dados_filtrados %>%
  group_by(curriculo, periodo) %>%
  summarise(
    taxa_evasao = mean(taxa_evasao), # manter a média da taxa para a barra
    total_evasao = sum(total_evasao) # substitua 'total_evasao' pela variável correta
  )

# Plot com geom_bar + texto do total de evasão + legenda
ggplot(dados_resumo, aes(x = curriculo, y = taxa_evasao, fill = curriculo)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = total_evasao), vjust = -0.5, size = 4, fontface = "bold") +
  facet_wrap(~periodo, nrow = 1) +
  labs(
    title = "Taxa de Evasão por Currículo nos Quatro Primeiros Períodos",
    x = "Currículo",
    y = "Taxa de Evasão (%)",
    fill = "Currículo"  # Aqui mantemos a legenda para curriculo
  ) +
  scale_fill_manual(values = c("1999" = "#F8766D", "2017" = "#00BA38")) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "right"
  )

unique(dados_linha$periodo)

names(dados_linha)


#######  ######
# -------------------------------
# Gráficos com barra e boxplot por variável e período
# -------------------------------

library(dplyr)
library(ggplot2)
library(scales)

variaveis <- c("sexo", "cor", "estado_civil", "politica_afirmativa", "forma_de_ingresso", "tipo_de_ensino_medio")

for (periodo in 1:4) {
  cat("\n====== Estatísticas de Evasão -", periodo, "º Período ======\n")
  
  for (var in variaveis) {
    cat("\n[", toupper(var), "]\n")
    
    col_evasao <- paste0("evadiu_p", periodo)
    
    df_resultado <- df_evasao %>%
      group_by_at(var) %>%
      summarise(
        total = n(),
        evasoes = sum(.data[[col_evasao]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(taxa_evasao = evasoes / total)
    
    media <- mean(df_resultado$taxa_evasao, na.rm = TRUE)
    desvio <- sd(df_resultado$taxa_evasao, na.rm = TRUE)
    
    print(df_resultado)
    cat("Média:", round(media, 4), "| Desvio Padrão:", round(desvio, 4), "\n")
    
    # Gráfico de Barras
    g1 <- ggplot(df_resultado, aes_string(x = var, y = "taxa_evasao")) +
      geom_col(fill = "#21908CFF") +
      geom_hline(yintercept = media, color = "red", linetype = "dashed") +
      geom_rect(aes(ymin = media - desvio, ymax = media + desvio, xmin = -Inf, xmax = Inf),
                fill = "red", alpha = 0.1) +
      scale_y_continuous(labels = percent_format()) +
      labs(
        title = paste("Taxa de Evasão por", var, "-", periodo, "º Período"),
        x = var,
        y = "Taxa de Evasão (%)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(g1)
    
    # Gráfico Boxplot
    g2 <- ggplot(df_evasao, aes_string(x = var, y = col_evasao)) +
      geom_boxplot(fill = "steelblue") +
      labs(
        title = paste("Boxplot de Evasão por", var, "-", periodo, "º Período"),
        x = var,
        y = "Evasão (0 = não, 1 = sim)"
      ) +
      theme_minimal()
    
    print(g2)
  }
}
#####
####
####
# -------------------------------
# Gráficos com barra e boxplot por variável, período e currículo
# -------------------------------

library(dplyr)
library(ggplot2)
library(scales)

# Identificação do currículo com base no período de ingresso
library(dplyr)

dados <- dados %>%
  mutate(
    p1 = periodo_de_ingresso,
    p2 = paste0(substr(periodo_de_ingresso, 1, 4), ".", 
                ifelse(substr(periodo_de_ingresso, 6, 6) == "1", "2", 
                       as.character(as.numeric(substr(periodo_de_ingresso, 6, 6)) + 1))),
    p3 = paste0(as.character(as.numeric(substr(periodo_de_ingresso, 1, 4)) + 1), ".", 
                substr(periodo_de_ingresso, 6, 6)),
    p4 = paste0(as.character(as.numeric(substr(periodo_de_ingresso, 1, 4)) + 1), ".", 
                ifelse(substr(periodo_de_ingresso, 6, 6) == "1", "2", "1")),
    evadiu_p1 = ifelse(periodo_de_evasao == p1, 1, 0),
    evadiu_p2 = ifelse(periodo_de_evasao == p2, 1, 0),
    evadiu_p3 = ifelse(periodo_de_evasao == p3, 1, 0),
    evadiu_p4 = ifelse(periodo_de_evasao == p4, 1, 0)
  )


df_evasao <- dados %>%
  mutate(
    periodo_ingresso_num = as.numeric(gsub("\\.", "", periodo_de_ingresso)),
    curriculo = case_when(
      periodo_ingresso_num >= 20111 & periodo_ingresso_num <= 20172 ~ "Currículo 1999",
      periodo_ingresso_num >= 20181 & periodo_ingresso_num <= 20222 ~ "Currículo 2017",
      TRUE ~ "Outro"
    )
  )

variaveis <- c("sexo", "cor", "estado_civil", "forma_de_ingresso")

for (periodo in 1:4) {
  cat("\n====== Estatísticas de Evasão -", periodo, "º Período ======\n")
  col_evasao <- paste0("evadiu_p", periodo)
  
  for (curr in unique(df_evasao$curriculo)) {
    df_curriculo <- df_evasao %>% filter(curriculo == curr)
    
    cat("\n---", curr, "---\n")
    
    for (var in variaveis) {
      cat("\n[", toupper(var), "]\n")
      
      df_resultado <- df_curriculo %>%
        group_by(.data[[var]]) %>%
        summarise(
          total = n(),
          evasoes = sum(.data[[col_evasao]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(taxa_evasao = evasoes / total)
      
      media <- mean(df_resultado$taxa_evasao, na.rm = TRUE)
      desvio <- sd(df_resultado$taxa_evasao, na.rm = TRUE)
      
      print(df_resultado)
      cat("Média:", round(media, 4), "| Desvio Padrão:", round(desvio, 4), "\n")
      
      # Gráfico de Barras
      g1 <- ggplot(df_resultado, aes(x = .data[[var]], y = taxa_evasao)) +
        geom_col(fill = "#21908CFF") +
        geom_hline(yintercept = media, color = "red", linetype = "dashed") +
        geom_rect(aes(ymin = media - desvio, ymax = media + desvio, xmin = -Inf, xmax = Inf),
                  fill = "red", alpha = 0.1, inherit.aes = FALSE) +
        scale_y_continuous(labels = percent_format()) +
        labs(
          title = paste("Taxa de Evasão por", var, "-", periodo, "º Período -", curr),
          x = var,
          y = "Taxa de Evasão (%)"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      print(g1)
      
      # Gráfico Boxplot
      g2 <- ggplot(df_curriculo, aes(x = .data[[var]], y = .data[[col_evasao]])) +
        geom_boxplot(fill = "steelblue") +
        labs(
          title = paste("Boxplot de Evasão por", var, "-", periodo, "º Período -", curr),
          x = var,
          y = "Evasão (0 = não, 1 = sim)"
        ) +
        theme_minimal()
      
      print(g2)
    }
  }
}

unique(df_evasao$periodo_de_ingresso)
table(df_evasao$periodo_ingresso_num)
table(tabelas$periodo_de_ingresso)
table(dados$periodo_de_ingresso)
colnames(dados)
