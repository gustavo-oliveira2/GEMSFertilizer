# ==============================================================================
# MÓDULO DE NÍVEL TECNOLÓGICO
# Classifica o sistema de produção em Baixo / Médio / Alto com base em
# checklist de tratos culturais. Ajusta doses de NPK conforme o nível.
# Referências: 5ª Aproximação MG (2023), Manual Sergipe EMBRAPA/UFS
# ==============================================================================

# --------------------------------------------------------------------------
# CHECKLIST — 10 categorias de tratos culturais
# Cada item tem: id, label, peso (1-3), nivel_minimo (qual nível exige o item)
# --------------------------------------------------------------------------
CHECKLIST_TRATOS <- list(

  list(id="semente_hibrido",
       cat="Semente e Material Genético",
       label="Utiliza híbrido simples ou duplo (não variedade crioula)",
       peso=3, nivel_min="medio"),

  list(id="semente_tratamento",
       cat="Semente e Material Genético",
       label="Realiza tratamento de sementes (fungicida e/ou inseticida)",
       peso=2, nivel_min="alto"),

  list(id="densidade_correta",
       cat="Plantio",
       label="Ajusta a população de plantas conforme recomendação da cultivar",
       peso=2, nivel_min="medio"),

  list(id="epoca_plantio",
       cat="Plantio",
       label="Respeita a época de plantio recomendada para a região",
       peso=2, nivel_min="medio"),

  list(id="analise_solo",
       cat="Fertilidade e Nutrição",
       label="Realiza análise de solo pelo menos a cada 2 anos",
       peso=3, nivel_min="medio"),

  list(id="calagem_sistemica",
       cat="Fertilidade e Nutrição",
       label="Realiza calagem com base em V% alvo (não só quando 'parece ácido')",
       peso=3, nivel_min="medio"),

  list(id="adubacao_plantio",
       cat="Fertilidade e Nutrição",
       label="Aplica adubação de plantio com NPK baseada em análise de solo",
       peso=3, nivel_min="medio"),

  list(id="adubacao_cobertura",
       cat="Fertilidade e Nutrição",
       label="Aplica adubação de cobertura nitrogenada (N em cobertura)",
       peso=3, nivel_min="medio"),

  list(id="adubacao_parcelada",
       cat="Fertilidade e Nutrição",
       label="Parcela a cobertura nitrogenada em 2 ou mais aplicações",
       peso=2, nivel_min="alto"),

  list(id="micronutrientes",
       cat="Fertilidade e Nutrição",
       label="Corrige micronutrientes (Zn, B, etc.) quando deficientes na análise",
       peso=2, nivel_min="alto"),

  list(id="daninhas_herbicida",
       cat="Manejo de Plantas Daninhas",
       label="Utiliza herbicida (pré ou pós-emergente) para controle de invasoras",
       peso=2, nivel_min="medio"),

  list(id="daninhas_mip",
       cat="Manejo de Plantas Daninhas",
       label="Faz monitoramento e manejo integrado de plantas daninhas",
       peso=1, nivel_min="alto"),

  list(id="pragas_monitoramento",
       cat="Manejo de Pragas e Doenças",
       label="Monitora pragas e aplica inseticida somente acima do nível de dano",
       peso=2, nivel_min="medio"),

  list(id="pragas_mip",
       cat="Manejo de Pragas e Doenças",
       label="Adota MIP completo (armadilhas, controle biológico, rotação)",
       peso=1, nivel_min="alto"),

  list(id="doencas_fungicida",
       cat="Manejo de Pragas e Doenças",
       label="Aplica fungicida preventivo em estádios críticos",
       peso=1, nivel_min="alto"),

  list(id="irrigacao",
       cat="Irrigação",
       label="Dispõe de irrigação (suplementar ou total)",
       peso=2, nivel_min="medio"),

  list(id="irrigacao_manejo",
       cat="Irrigação",
       label="Faz manejo da irrigação por tensiômetro, balanço hídrico ou evapotranspiração",
       peso=1, nivel_min="alto"),

  list(id="colheita_mec",
       cat="Colheita e Pós-colheita",
       label="Realiza colheita mecanizada ou semimecanizada",
       peso=1, nivel_min="medio"),

  list(id="armazenamento",
       cat="Colheita e Pós-colheita",
       label="Dispõe de armazenamento adequado (silo, graneleiro, câmara fria)",
       peso=1, nivel_min="alto"),

  list(id="assistencia",
       cat="Gestão e Assistência Técnica",
       label="Recebe assistência técnica de engenheiro agrônomo regularmente",
       peso=1, nivel_min="medio"),

  list(id="registros",
       cat="Gestão e Assistência Técnica",
       label="Mantém caderno de campo / registros de tratos, insumos e custos",
       peso=1, nivel_min="alto")
)

# --------------------------------------------------------------------------
# CLASSIFICAÇÃO AUTOMÁTICA
# Pontuação: soma dos pesos dos itens marcados
# Máximo possível: soma de todos os pesos
# --------------------------------------------------------------------------
classificar_nivel <- function(itens_marcados) {
  # itens_marcados: vetor de IDs dos itens que o usuário marcou
  if (length(itens_marcados) == 0) {
    return(list(
      nivel        = "baixo",
      nivel_label  = "Baixa Tecnologia",
      pontos       = 0,
      pontos_max   = sum(sapply(CHECKLIST_TRATOS, function(x) x$peso)),
      pct          = 0,
      cor          = "#E53935",
      icone        = "\u26a0\ufe0f",
      descricao    = "Sistema com poucas ou nenhuma prática recomendada adotada.",
      itens_ok     = character(0),
      itens_falta  = sapply(CHECKLIST_TRATOS, function(x) x$id)
    ))
  }

  pontos_max  <- sum(sapply(CHECKLIST_TRATOS, function(x) x$peso))
  pontos_user <- sum(sapply(CHECKLIST_TRATOS, function(x) {
    if (x$id %in% itens_marcados) x$peso else 0
  }))
  pct <- round(pontos_user / pontos_max * 100)

  # Itens obrigatórios por nível
  obrig_medio <- sapply(Filter(function(x) x$nivel_min == "medio", CHECKLIST_TRATOS),
                         function(x) x$id)
  obrig_alto  <- sapply(Filter(function(x) x$nivel_min == "alto",  CHECKLIST_TRATOS),
                         function(x) x$id)

  n_medio_ok <- sum(obrig_medio %in% itens_marcados)
  n_alto_ok  <- sum(obrig_alto  %in% itens_marcados)
  n_medio    <- length(obrig_medio)
  n_alto     <- length(obrig_alto)

  # Nível: precisa de % da pontuação E cumprir mínimo dos obrigatórios
  nivel <- if (pct >= 72 && n_alto_ok >= round(n_alto * 0.6)) "alto"
           else if (pct >= 40 && n_medio_ok >= round(n_medio * 0.6)) "medio"
           else "baixo"

  itens_falta <- sapply(Filter(function(x) !x$id %in% itens_marcados, CHECKLIST_TRATOS),
                         function(x) x$id)

  list(
    nivel       = nivel,
    nivel_label = c(baixo="Baixa Tecnologia", medio="M\u00e9dia Tecnologia",
                    alto="Alta Tecnologia")[[nivel]],
    pontos      = pontos_user,
    pontos_max  = pontos_max,
    pct         = pct,
    cor         = c(baixo="#E53935", medio="#F9A825", alto="#43A047")[[nivel]],
    icone       = c(baixo="\ud83d\udfe5", medio="\ud83d\udfe1", alto="\ud83d\udfe2")[[nivel]],
    descricao   = c(
      baixo = "Sistema com poucas pr\u00e1ticas recomendadas. Potencial produtivo limitado e maior risco de resposta negativa \u00e0 aduba\u00e7\u00e3o.",
      medio = "Sistema com pr\u00e1ticas essenciais adotadas. Bom potencial de resposta \u00e0 aduba\u00e7\u00e3o com doses moderadas.",
      alto  = "Sistema tecnificado, com m\u00e1ximo potencial produtivo. Doses mais altas s\u00e3o econ\u00f4micamente justific\u00e1veis."
    )[[nivel]],
    itens_ok    = itens_marcados,
    itens_falta = itens_falta
  )
}

# --------------------------------------------------------------------------
# FATORES DE AJUSTE DE DOSE POR NÍVEL TECNOLÓGICO
# Baseado nas tabelas diferenciadas dos manuais (5ª Aprox. MG + Manual SE)
# Para cada nutriente: multiplicador em relação à dose média (nível médio = 1.0)
# --------------------------------------------------------------------------
FATORES_NIVEL <- list(
  # N plantio
  n_plantio = list(baixo = 0.70, medio = 1.00, alto = 1.20),
  # N cobertura
  n_cobertura = list(baixo = 0.65, medio = 1.00, alto = 1.30),
  # P2O5 plantio
  p2o5 = list(baixo = 0.75, medio = 1.00, alto = 1.15),
  # K2O plantio
  k2o  = list(baixo = 0.75, medio = 1.00, alto = 1.10)
)

# Aplica os fatores de ajuste nas doses calculadas
ajustar_doses_nivel <- function(rec, nivel = "medio") {
  if (is.null(rec) || nivel == "medio") return(rec)
  f <- FATORES_NIVEL
  rec_aj <- rec
  if (!is.null(rec$N_plantio)   && !is.na(rec$N_plantio))
    rec_aj$N_plantio   <- round(rec$N_plantio   * f$n_plantio[[nivel]])
  if (!is.null(rec$N_cobertura) && !is.na(rec$N_cobertura))
    rec_aj$N_cobertura <- round(rec$N_cobertura * f$n_cobertura[[nivel]])
  if (!is.null(rec$P2O5)        && !is.na(rec$P2O5))
    rec_aj$P2O5        <- round(rec$P2O5        * f$p2o5[[nivel]])
  if (!is.null(rec$K2O)         && !is.na(rec$K2O))
    rec_aj$K2O         <- round(rec$K2O         * f$k2o[[nivel]])
  rec_aj$nivel_ajuste <- nivel
  rec_aj
}

# Texto justificativo para o relatório
justificativa_nivel <- function(nivel_res, cultura) {
  f <- FATORES_NIVEL
  nivel <- nivel_res$nivel
  paste0(
    "O sistema de produ\u00e7\u00e3o foi classificado como <b>", nivel_res$nivel_label,
    "</b> (pontuação: ", nivel_res$pontos, "/", nivel_res$pontos_max, " — ",
    nivel_res$pct, "%). ", nivel_res$descricao, " ",
    if (nivel != "medio") {
      fators <- c(
        paste0("N plantio \u00d7", f$n_plantio[[nivel]]),
        paste0("N cobertura \u00d7", f$n_cobertura[[nivel]]),
        paste0("P\u2082O\u2085 \u00d7", f$p2o5[[nivel]]),
        paste0("K\u2082O \u00d7", f$k2o[[nivel]])
      )
      paste0("Fatores de ajuste aplicados: ",
             paste(fators, collapse=", "), ". ")
    } else "Doses na refer\u00eancia do n\u00edvel m\u00e9dio (sem ajuste). ",
    "Refer\u00eancias: Ribeiro et al. (2023) — 5\u00aa Aproxima\u00e7\u00e3o MG; EMBRAPA/UFS — Manual de Sergipe."
  )
}

# Agrupa itens do checklist por categoria para exibição
categorias_checklist <- function() {
  cats <- unique(sapply(CHECKLIST_TRATOS, function(x) x$cat))
  lapply(cats, function(cat) {
    list(
      cat   = cat,
      itens = Filter(function(x) x$cat == cat, CHECKLIST_TRATOS)
    )
  })
}
