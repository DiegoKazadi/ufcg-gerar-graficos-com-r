library(lubridate)

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
    df$data_de_nascimento <- ymd(df$data_de_nascimento)
    df <- df %>% mutate(
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
