colunas_corrigidas <- c(
  'cpf', 'matricula_do_estudante', 'periodo_de_ingresso', 'forma_de_ingresso',
  'codigo_do_curriculo', 'estado_civil', 'sexo', 'data_de_nascimento', 'cor',
  'ano_de_conclusao_ensino_medio', 'tipo_de_ensino_medio', 'politica_afirmativa',
  'situacao', 'motivo_de_evasao', 'periodo_de_evasao'
)

for (nome in names(tabelas)) {
  if (str_detect(nome, "alunos-novos")) {
    df <- tabelas[[nome]]
    if (ncol(df) == length(colunas_corrigidas)) {
      colnames(df) <- colunas_corrigidas
      tabelas[[nome]] <- df
    } else {
      message(paste("Tabela", nome, "não tem o número esperado de colunas."))
    }
  }
}
