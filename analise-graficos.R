library(ggplot2)

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
      taxa_evasao = total_evasao / total_ingressantes
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
