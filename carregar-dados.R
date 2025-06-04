
install.packages("stringr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")


library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# Caminho base dos arquivos CSV
caminho_base <- "/home/diego/Documentos/Semestre 2024.2/Nova_Analise/dados/tabelas"

# Lista arquivos CSV no diretório
arquivos <- list.files(caminho_base, pattern = "\\.csv$", full.names = TRUE)

# Lista para armazenar os dataframes
tabelas <- list()

# Carregamento dos arquivos
for (arquivo in arquivos) {
  nome_base <- tools::file_path_sans_ext(basename(arquivo))
  tryCatch({
    df <- read_csv(arquivo, show_col_types = FALSE)
    tabelas[[nome_base]] <- df
    message(paste("✔️ Arquivo carregado:", nome_base))
  }, error = function(e) {
    message(paste("❌ Erro ao carregar", arquivo, ":", e$message))
  })
}

message(paste("\nTotal de arquivos carregados:", length(tabelas)))


####
# Verificar colunas
for (nome in names(tabelas)) {
  df <- tabelas[[nome]]
  cat("Tabela:", nome, "- Número de colunas:", ncol(df), "\n")
  colnames_sem_nome <- which(is.na(colnames(df)) | colnames(df) == "")
  if (length(colnames_sem_nome) > 0) {
    cat("  Colunas sem nome:", colnames_sem_nome, "\n")
  }
}
# Padronizar colunas
colunas_corrigidas <- c(
  'cpf', 'matricula_do_estudante', 'periodo_de_ingresso', 'forma_de_ingresso',
  'codigo_do_curriculo', 'estado_civil', 'sexo', 'data_de_nascimento', 'cor',
  'ano_de_conclusao_ensino_medio', 'tipo_de_ensino_medio', 'politica_afirmativa',
  'situacao', 'motivo_de_evasao', 'periodo_de_evasao'
)

for (nome in names(tabelas)) {
  if (str_detect(nome, "alunos-novos")) {
    df <- tabelas[[nome]]
    
    # Corrigir colunas sem nome
    colnames(df)[is.na(colnames(df)) | colnames(df) == ""] <- paste0("col_extra_", seq_len(sum(is.na(colnames(df)) | colnames(df) == "")))
    
    if (ncol(df) >= length(colunas_corrigidas)) {
      # Manter só as primeiras colunas que interessam
      df <- df[, 1:length(colunas_corrigidas)]
      colnames(df) <- colunas_corrigidas
      tabelas[[nome]] <- df
      message(paste("✔️ Tabela", nome, "corrigida com colunas padronizadas."))
    } else {
      message(paste("⚠️ Tabela", nome, "tem menos colunas que o esperado e não foi corrigida."))
    }
  }
}


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
