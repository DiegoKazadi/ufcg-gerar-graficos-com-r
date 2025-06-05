
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

# Calcular evasões ao final do primeiro período

# Função principal
calcular_evasao_primeiro_periodo <- function(tabela, nome_tabela) {
  if (!tem_colunas(tabela, c("periodo_de_ingresso", "periodo_de_evasao"))) {
    return(NULL)
  }
  
  df <- tabela %>%
    filter(periodo_de_ingresso %in% periodos_ingresso) %>%
    mutate(
      periodo_de_ingresso = as.integer(periodo_de_ingresso),
      periodo_de_evasao = as.integer(periodo_de_evasao),
      periodo_alvo = periodo_de_ingresso + ifelse(periodo_de_ingresso %% 10 == 1, 1, 9)
    ) %>%
    mutate(evadiu_no_primeiro_periodo = periodo_de_evasao == periodo_alvo) %>%
    group_by(periodo_de_ingresso) %>%
    summarise(
      total_ingressantes = n(),
      evasao_primeiro_periodo = sum(evadiu_no_primeiro_periodo, na.rm = TRUE),
      taxa_evasao = round(100 * evasao_primeiro_periodo / total_ingressantes, 2)
    ) %>%
    mutate(tabela = nome_tabela)
  
  return(df)
}

# Aplicar às tabelas

estatisticas_evasao <- purrr::map2_dfr(
  tabelas,
  names(tabelas),
  calcular_evasao_primeiro_periodo
)

# Visualizar resultado
print(estatisticas_evasao)
View(estatisticas_evasao)


###############################################################################












# Funções auxiliares
filtrar_periodo <- function(df) {
  if ("periodo_de_ingresso" %in% names(df)) {
    df$periodo_de_ingresso <- as.numeric(df$periodo_de_ingresso)
    df <- df %>% filter(between(periodo_de_ingresso, 2011.1, 2023.2))
  }
  return(df)
}

limpar_dados <- function(df) {
  df[df == "-" | df == "Não declarada"] <- NA
  
  if ("data_de_nascimento" %in% names(df)) {
    df <- df %>%
      mutate(
        data_de_nascimento = as.character(data_de_nascimento),
        data_de_nascimento = gsub(" UTC", "", data_de_nascimento),
        data_de_nascimento = suppressWarnings(as.Date(data_de_nascimento)),
        idade = ifelse(!is.na(data_de_nascimento), year(Sys.Date()) - year(data_de_nascimento), NA)
      )
  }
  
  for (col in c("sexo", "estado_civil", "cor")) {
    if (col %in% names(df)) {
      df[[col]] <- str_to_sentence(trimws(as.character(df[[col]])))
    }
  }
  
  return(df)
}

tabelas_relevantes <- c(
  "alunos-novos-sem-reingressos-novos-ou-antigos-sem-metricas",
  "alunos", "alunos-matriculados", 
  "alunos-final", "alunos-filtrado-sem-metricas"
)

tabelas_tratadas <- list()

for (nome in names(tabelas)) {
  if (nome %in% tabelas_relevantes) {
    df <- tabelas[[nome]] %>% filtrar_periodo() %>% limpar_dados()
    tabelas_tratadas[[nome]] <- df
  }
}

# Função para calcular próximo período
proximo_n_periodo <- function(periodo, n = 1) {
  parts <- str_split_fixed(as.character(periodo), "\\.", 2)
  ano <- as.integer(parts[,1])
  semestre <- as.integer(parts[,2])
  for (i in seq_len(n)) {
    if (semestre == 1) {
      semestre <- 2
    } else {
      semestre <- 1
      ano <- ano + 1
    }
  }
  return(paste0(ano, ".", semestre))
}

# Função principal de evasão
evasao_apos_n_periodos <- function(df, n, inicio, fim) {
  df <- df %>%
    mutate(
      periodo_de_ingresso = as.character(periodo_de_ingresso),
      periodo_de_evasao = as.character(periodo_de_evasao)
    ) %>%
    filter(periodo_de_ingresso >= inicio & periodo_de_ingresso <= fim) %>%
    mutate(
      periodo_esperado_evasao = proximo_n_periodo(periodo_de_ingresso, n),
      evadiu = periodo_de_evasao == periodo_esperado_evasao
    ) %>%
    group_by(periodo_de_ingresso) %>%
    summarise(
      total_ingressantes = n(),
      total_evasao = sum(evadiu, na.rm = TRUE),
      taxa_evasao = total_evasao / total_ingressantes,
      .groups = "drop"
    )
  return(df)
}

# Parâmetros por currículo
curriculos <- list(
  "1999" = list(inicio = "2011.1", fim = "2016.2"),
  "2017" = list(inicio = "2018.1", fim = "2022.3")
)

# Montagem dos dados para gráfico
dados_plot <- data.frame()

for (curriculo in names(curriculos)) {
  intervalo <- curriculos[[curriculo]]
  for (n in 1:4) {
    for (nome in names(tabelas_tratadas)) {
      df <- tabelas_tratadas[[nome]]
      resultado <- evasao_apos_n_periodos(df, n, intervalo$inicio, intervalo$fim)
      if (nrow(resultado) > 0) {
        resultado <- resultado %>%
          mutate(
            periodo = paste0(n, "º período"),
            curriculo = curriculo,
            tabela = nome
          )
        dados_plot <- bind_rows(dados_plot, resultado)
      }
    }
  }
}

# Gráficos
for (tabela_nome in unique(dados_plot$tabela)) {
  dados_tabela <- dados_plot %>% filter(tabela == tabela_nome)
  p <- ggplot(dados_tabela, aes(x = periodo, y = taxa_evasao, fill = curriculo)) +
    geom_boxplot() +
    labs(
      title = paste("Taxa de Evasão por Período e Currículo - Tabela:", tabela_nome),
      x = "Período", y = "Taxa de Evasão", fill = "Currículo"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")
  print(p)
}

View(dados_plot)
