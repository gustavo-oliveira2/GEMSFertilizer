# ==============================================================================
# FUNÇÕES DE CÁLCULO E ANÁLISE
# ==============================================================================

# Rodar análise completa de solo
analisar_solo <- function(ph, mo, p, k, ca, mg, al, h_al, argila,
                          s = NA, b = NA, cu = NA, fe = NA, mn = NA, zn = NA) {
  
  ctc <- calcular_ctc(ca, mg, k, al, h_al)
  v   <- calcular_v(ca, mg, k, ctc)
  m   <- calcular_m(al, ca, mg, k)
  sb  <- ca + mg + (k / 391)
  
  list(
    ph  = list(valor = ph,  interp = interpretar_ph(ph)),
    mo  = list(valor = mo,  interp = interpretar_mo(mo)),
    p   = list(valor = p,   interp = interpretar_p(p, argila)),
    k   = list(valor = k,   interp = interpretar_k(k, ctc)),
    ca  = list(valor = ca,  interp = interpretar_ca(ca)),
    mg  = list(valor = mg,  interp = interpretar_mg(mg)),
    al  = list(valor = al),
    h_al = list(valor = h_al),
    ctc = list(valor = ctc),
    sb  = list(valor = round(sb, 2)),
    v   = list(valor = v,   interp = interpretar_v(v)),
    m   = list(valor = m,   interp = interpretar_m(m)),
    s   = if (!is.na(s)) list(valor = s, interp = interpretar_s(s)) else NULL,
    micro = list(
      B  = if (!is.na(b))  list(valor = b,  interp = interpretar_micro(b,  "B"))  else NULL,
      Cu = if (!is.na(cu)) list(valor = cu, interp = interpretar_micro(cu, "Cu")) else NULL,
      Fe = if (!is.na(fe)) list(valor = fe, interp = interpretar_micro(fe, "Fe")) else NULL,
      Mn = if (!is.na(mn)) list(valor = mn, interp = interpretar_micro(mn, "Mn")) else NULL,
      Zn = if (!is.na(zn)) list(valor = zn, interp = interpretar_micro(zn, "Zn")) else NULL
    ),
    argila = argila
  )
}

# Calcular necessidade de calagem com todos os métodos
calcular_todas_calagen <- function(v_atual, ctc, al, ca, mg, ph_smp = NULL, v_alvo = 60, prnt = 70) {
  metodos <- list()
  metodos[["V% (5ª Aprox. MG)"]] <- calagem_v(v_atual, v_alvo, ctc, prnt)
  metodos[["Al³⁺ + Ca+Mg"]]       <- calagem_al(al, ca, mg, prnt = prnt)
  if (!is.null(ph_smp) && !is.na(ph_smp)) {
    metodos[["Tampão SMP"]]        <- calagem_smp(ph_smp, v_alvo)
  }
  return(metodos)
}

# Gerar tabela comparativa de produtos fertilizantes
tabela_comparativa <- function(dose_n, dose_p, dose_k, area = 1,
                               df_fontes = NULL, precos_usuario = NULL) {
  # Usa df_fontes (reactiveVal do módulo de preços) se disponível
  # senão cai para todas_fontes (embutido)
  if (!is.null(df_fontes) && nrow(df_fontes) > 0) {
    fontes_all <- df_fontes
  } else {
    fontes_all <- todas_fontes
  }
  
  # Garante tipos numéricos — read.csv pode ler como character se houver
  # vírgulas decimais ou valores inválidos no CSV
  fontes_all$teor      <- suppressWarnings(as.numeric(
    gsub(",", ".", as.character(fontes_all$teor))
  ))
  fontes_all$preco_ref <- suppressWarnings(as.numeric(
    gsub(",", ".", as.character(fontes_all$preco_ref))
  ))
  
  # Remove linhas com teor ou preço inválidos
  fontes_all <- fontes_all[
    !is.na(fontes_all$teor) & fontes_all$teor > 0 &
    !is.na(fontes_all$preco_ref) & fontes_all$preco_ref >= 0,
  ]
  
  # Atualizar preços se fornecidos manualmente (legado)
  if (!is.null(precos_usuario)) {
    for (i in seq_len(nrow(fontes_all))) {
      prod <- fontes_all$produto[i]
      if (!is.null(precos_usuario[[prod]])) {
        fontes_all$preco_ref[i] <- precos_usuario[[prod]]
      }
    }
  }
  
  resultado <- list()
  
  for (i in seq_len(nrow(fontes_all))) {
    row <- fontes_all[i, ]
    dose <- switch(as.character(row$nutriente),
      "N"    = dose_n,
      "P2O5" = dose_p,
      "K2O"  = dose_k,
      0
    )
    if (is.na(dose) || dose <= 0) next
    
    calc <- calcular_custo_produto(dose, row$teor, row$preco_ref, area)
    
    fonte_label <- if ("fonte" %in% names(row) && !is.na(row$fonte)) row$fonte else "—"
    data_label  <- if ("data_ref" %in% names(row) && !is.na(row$data_ref)) formatar_data_ref(row$data_ref) else "—"
    
    resultado[[i]] <- data.frame(
      Produto      = row$produto,
      Nutriente    = row$nutriente,
      Teor_pct     = paste0(round(row$teor * 100, 0), "%"),
      Dose_kg_ha   = calc$kg_produto_ha,
      Preco_kg     = row$preco_ref,
      Custo_ha     = calc$custo_ha,
      Custo_total  = calc$custo_total,
      Fonte        = fonte_label,
      Data_ref     = data_label,
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, Filter(Negate(is.null), resultado))
}

# Formatar número em BRL
fmt_brl <- function(x) {
  formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
}

# Classificar nível de urgência para fertilidade
nivel_urgencia <- function(classe) {
  switch(classe,
    "Muito Baixo"   = "urgente",
    "Baixo"         = "atenção",
    "Médio"         = "moderado",
    "Bom"           = "adequado",
    "Muito Bom"     = "excelente",
    "Adequado"      = "adequado",
    "moderado"
  )
}
