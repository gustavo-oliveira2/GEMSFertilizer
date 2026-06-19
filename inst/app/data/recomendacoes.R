# ==============================================================================
# RECOMENDAÇÕES DE ADUBAÇÃO E CALAGEM
# Baseado no Manual de Minas Gerais 5ª Aproximação e Manual de Sergipe
# ==============================================================================

# ------------------------------------------------------------------------------
# CALAGEM - CÁLCULO DA NECESSIDADE DE CALAGEM (NC)
# ------------------------------------------------------------------------------

# MÉTODO 1: Saturação por Bases (V%) - Recomendado MG 5ª Aprox.
calagem_v <- function(v_atual, v_alvo, ctc, prnt = 70, prof = 20) {
  # NC (t/ha) = (V_alvo - V_atual) × CTC / (10 × PRNT/100)
  nc <- ((v_alvo - v_atual) * ctc) / (10 * (prnt / 100))
  nc <- max(0, nc)
  return(round(nc, 2))
}

# MÉTODO 2: Neutralização do Alumínio + Elevação de Ca+Mg
calagem_al <- function(al, ca, mg, y = 1.0, prnt = 70) {
  # NC (t/ha) = (2×Al + (2 - Ca - Mg)) × 100/PRNT
  # y = coeficiente de tolerância ao Al (0.5 a 2.0)
  nc <- max(0, (y * al + max(0, (2 - ca - mg)))) * (100 / prnt)
  return(round(nc, 2))
}

# MÉTODO 3: Tampão SMP (menos comum no NE, incluído para MG)
calagem_smp <- function(ph_smp, v_alvo = 70) {
  # Equação aproximada para calagem com tampão SMP
  # NC = f(pH_SMP, V_alvo) - tabela simplificada
  tabela_smp <- data.frame(
    ph_smp = c(4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0),
    nc_v60 = c(14.5, 11.0, 8.5, 6.0, 4.0, 2.0, 0.0),
    nc_v70 = c(17.0, 13.5, 10.5, 7.5, 5.0, 2.5, 0.0)
  )
  if (v_alvo <= 60) {
    nc <- approx(tabela_smp$ph_smp, tabela_smp$nc_v60, xout = ph_smp)$y
  } else {
    nc <- approx(tabela_smp$ph_smp, tabela_smp$nc_v70, xout = ph_smp)$y
  }
  return(round(max(0, nc, na.rm = TRUE), 2))
}

# ------------------------------------------------------------------------------
# GESSAGEM - NECESSIDADE DE GESSO AGRÍCOLA
# Três métodos:
#   1. Textura (Sousa & Lobato, 2004)  — baseado em % argila superficial
#   2. Saturação por Bases V% subsolo (Demattê, 1986; Vitti et al., 2008)
#   3. Saturação por Ca na CTCef (Caires & Guimarães, 2018) — mais atual
#
# Critérios de necessidade (qualquer um):
#   • Al³⁺ (20-40 cm) > 0,5 cmolc dm⁻³
#   • Ca²⁺ (20-40 cm) < 0,5 cmolc dm⁻³
#   • Saturação por Al m% (20-40 cm) > 20%
#   • Saturação por bases V% (20-40 cm) < 35%
#   • Sat. por Ca na CTCef (20-40 cm) < 54%  [critério Caires & Guimarães]
# ------------------------------------------------------------------------------
gessagem <- function(argila,
                     al_sup = NA, ca_sup = NA,       # camada 0-20 cm (surface)
                     ca_sub = NA, mg_sub = NA,        # camada 20-40 cm (subsurface)
                     k_sub  = NA, al_sub = NA) {      # idem

  # ------------------------------------------------------------------
  # MÉTODO 1 — Textura (Sousa & Lobato, 2004)
  # NG (kg/ha) = 50 × argila(%)   → para culturas anuais
  # NG (kg/ha) = 75 × argila(%)   → para culturas perenes / abertura de área
  # ------------------------------------------------------------------
  dose_argila_anual   <- round(argila * 50 / 1000, 1)   # t/ha
  dose_argila_perene  <- round(argila * 75 / 1000, 1)

  # ------------------------------------------------------------------
  # MÉTODO 2 — Saturação por Bases V% subsolo (Demattê/Vitti)
  # NG (t/ha) = (V2 - V1) × CTC_sub / 500
  # V2 alvo subsolo = 50%; usa dados da camada 20-40 cm
  # ------------------------------------------------------------------
  dose_v_sub <- NA
  v_sub      <- NA
  ctc_sub    <- NA

  tem_sub <- !is.na(ca_sub) && !is.na(mg_sub) &&
             !is.na(k_sub)  && !is.na(al_sub)

  if (tem_sub) {
    k_sub_cmolc <- k_sub / 391
    ctc_sub     <- ca_sub + mg_sub + k_sub_cmolc + al_sub
    sb_sub      <- ca_sub + mg_sub + k_sub_cmolc
    v_sub       <- if (ctc_sub > 0) round(sb_sub / ctc_sub * 100, 1) else 0
    v2_alvo     <- 50
    dose_v_sub  <- round(max(0, (v2_alvo - v_sub) * ctc_sub / 500), 1)
  }

  # ------------------------------------------------------------------
  # MÉTODO 3 — Sat. por Ca na CTCef (Caires & Guimarães, 2018)
  # CTCef = Ca + Mg + K + Al  (cmolc dm⁻³)
  # NG (t/ha) = (0,6 × CTCef − Ca₂₀₋₄₀) × 6,4
  # Usar somente se sat. Ca < 54%
  # ------------------------------------------------------------------
  dose_caires <- NA
  sat_ca      <- NA
  ctcef_sub   <- NA

  if (tem_sub) {
    k_sub_cmolc <- k_sub / 391
    ctcef_sub   <- ca_sub + mg_sub + k_sub_cmolc + al_sub
    sat_ca      <- if (ctcef_sub > 0) round(ca_sub / ctcef_sub * 100, 1) else 0

    if (sat_ca < 54) {
      dose_caires <- round(max(0, (0.6 * ctcef_sub - ca_sub) * 6.4), 1)
    } else {
      dose_caires <- 0   # não necessário
    }
  }

  # ------------------------------------------------------------------
  # DIAGNÓSTICO DE NECESSIDADE
  # ------------------------------------------------------------------
  # Usa dados do subsolo se disponíveis; senão usa superfície
  al_ref <- if (!is.na(al_sub)) al_sub else if (!is.na(al_sup)) al_sup else 0
  ca_ref <- if (!is.na(ca_sub)) ca_sub else if (!is.na(ca_sup)) ca_sup else 99

  m_sub <- if (tem_sub) {
    k_sub_cmolc <- k_sub / 391
    sb  <- ca_sub + mg_sub + k_sub_cmolc
    if (sb + al_sub > 0) round(al_sub / (sb + al_sub) * 100, 1) else 0
  } else NA

  necessidade <- al_ref > 0.5 || ca_ref < 0.5 ||
    (!is.na(m_sub)  && m_sub  > 20) ||
    (!is.na(v_sub)  && v_sub  < 35) ||
    (!is.na(sat_ca) && sat_ca < 54)

  # Justificativas
  justifs <- c()
  if (al_ref > 0.5)                         justifs <- c(justifs, paste0("Al\u00b3\u207a > 0,5 cmol\u1d04 dm\u207b\u00b3 (", al_ref, ")"))
  if (ca_ref < 0.5)                         justifs <- c(justifs, paste0("Ca\u00b2\u207a < 0,5 cmol\u1d04 dm\u207b\u00b3 (", ca_ref, ")"))
  if (!is.na(m_sub)  && m_sub  > 20)        justifs <- c(justifs, paste0("m% = ", m_sub, "% > 20%"))
  if (!is.na(v_sub)  && v_sub  < 35)        justifs <- c(justifs, paste0("V% subsolo = ", v_sub, "% < 35%"))
  if (!is.na(sat_ca) && sat_ca < 54)        justifs <- c(justifs, paste0("Sat. Ca = ", sat_ca, "% < 54%"))
  if (length(justifs) == 0)                 justifs <- "Nenhuma limitação detectada"

  return(list(
    necessidade        = necessidade,
    justificativa      = paste(justifs, collapse = "; "),
    # doses por método
    dose_argila_anual  = dose_argila_anual,
    dose_argila_perene = dose_argila_perene,
    dose_v_sub         = dose_v_sub,
    dose_caires        = dose_caires,
    # parâmetros diagnósticos do subsolo
    v_sub              = v_sub,
    sat_ca             = sat_ca,
    ctcef_sub          = ctcef_sub,
    m_sub              = m_sub,
    tem_sub            = tem_sub
  ))
}

# ------------------------------------------------------------------------------
# RECOMENDAÇÕES NPK POR CULTURA
# Manual MG 5ª Aproximação + Adaptações para Sergipe
# ------------------------------------------------------------------------------

rec_milho <- function(p_nivel, k_nivel, produtividade = "media", fase = "plantio", mo, n_anterior = "nenhum") {
  # Produtividade esperada: baixa (<4t), media (4-8t), alta (>8t)
  
  # NITROGÊNIO
  n_base <- switch(produtividade,
    "baixa"  = 20,
    "media"  = 30,
    "alta"   = 40
  )
  n_cobert <- switch(produtividade,
    "baixa"  = 60,
    "media"  = 90,
    "alta"   = 120
  )
  # Desconto por MO alta
  if (!is.na(mo) && mo >= 3.0) { n_base <- n_base * 0.8; n_cobert <- n_cobert * 0.8 }
  # Desconto por leguminosa anterior
  if (n_anterior == "leguminosa") { n_cobert <- n_cobert * 0.7 }
  
  # FÓSFORO (P2O5) - varia por nível de P no solo
  p_rec <- switch(as.character(p_nivel),
    "1" = switch(produtividade, "baixa"=80, "media"=100, "alta"=120),
    "2" = switch(produtividade, "baixa"=60, "media"=80,  "alta"=100),
    "3" = switch(produtividade, "baixa"=40, "media"=60,  "alta"=80),
    "4" = switch(produtividade, "baixa"=20, "media"=40,  "alta"=60),
    "5" = switch(produtividade, "baixa"=0,  "media"=20,  "alta"=40)
  )
  
  # POTÁSSIO (K2O)
  k_rec <- switch(as.character(k_nivel),
    "1" = switch(produtividade, "baixa"=70, "media"=90,  "alta"=110),
    "2" = switch(produtividade, "baixa"=50, "media"=70,  "alta"=90),
    "3" = switch(produtividade, "baixa"=30, "media"=50,  "alta"=70),
    "4" = switch(produtividade, "baixa"=20, "media"=30,  "alta"=50),
    "5" = switch(produtividade, "baixa"=0,  "media"=20,  "alta"=30)
  )
  
  return(list(
    N_plantio = round(n_base),
    N_cobertura = round(n_cobert),
    P2O5 = round(p_rec),
    K2O = round(k_rec)
  ))
}

rec_feijao <- function(p_nivel, k_nivel, produtividade = "media", mo) {
  n_cobert <- switch(produtividade,
    "baixa"  = 0,
    "media"  = 20,
    "alta"   = 30
  )
  
  p_rec <- switch(as.character(p_nivel),
    "1" = 90, "2" = 70, "3" = 50, "4" = 30, "5" = 20
  )
  
  k_rec <- switch(as.character(k_nivel),
    "1" = 70, "2" = 55, "3" = 40, "4" = 25, "5" = 15
  )
  
  return(list(
    N_plantio = 20,
    N_cobertura = round(n_cobert),
    P2O5 = round(p_rec),
    K2O = round(k_rec)
  ))
}

rec_cana <- function(p_nivel, k_nivel, fase = "plantio", mo, toneladas = 80) {
  # Fase plantio vs soca
  if (fase == "plantio") {
    n_dose <- 40
    p_rec <- switch(as.character(p_nivel),
      "1"=120, "2"=100, "3"=80, "4"=60, "5"=40
    )
    k_rec <- switch(as.character(k_nivel),
      "1"=120, "2"=100, "3"=80, "4"=60, "5"=40
    )
  } else {
    # Soca - baseado em toneladas esperadas
    n_dose <- toneladas * 1.2  # 1.2 kg N/t cana
    p_rec <- switch(as.character(p_nivel),
      "1"=80, "2"=60, "3"=40, "4"=20, "5"=0
    )
    k_rec <- toneladas * 2.0  # 2.0 kg K2O/t cana
  }
  
  return(list(
    N_plantio = round(n_dose * 0.3),
    N_cobertura = round(n_dose * 0.7),
    P2O5 = round(p_rec),
    K2O = round(k_rec)
  ))
}

rec_arroz <- function(p_nivel, k_nivel, produtividade = "media", mo, tipo = "sequeiro") {
  n_total <- switch(produtividade,
    "baixa"=60, "media"=80, "alta"=100
  )
  if (!is.na(mo) && mo >= 3.0) n_total <- n_total * 0.8
  
  p_rec <- switch(as.character(p_nivel),
    "1"=80, "2"=60, "3"=40, "4"=20, "5"=10
  )
  k_rec <- switch(as.character(k_nivel),
    "1"=60, "2"=45, "3"=30, "4"=15, "5"=0
  )
  
  return(list(
    N_plantio = round(n_total * 0.25),
    N_cobertura = round(n_total * 0.75),
    P2O5 = round(p_rec),
    K2O = round(k_rec)
  ))
}

rec_mandioca <- function(p_nivel, k_nivel, mo) {
  n_dose <- 40
  if (!is.na(mo) && mo >= 3.0) n_dose <- 25
  
  p_rec <- switch(as.character(p_nivel),
    "1"=80, "2"=60, "3"=40, "4"=20, "5"=10
  )
  k_rec <- switch(as.character(k_nivel),
    "1"=100, "2"=80, "3"=60, "4"=40, "5"=20
  )
  
  return(list(
    N_plantio = 20,
    N_cobertura = round(n_dose - 20),
    P2O5 = round(p_rec),
    K2O = round(k_rec)
  ))
}

rec_amendoim <- function(p_nivel, k_nivel, mo) {
  p_rec <- switch(as.character(p_nivel),
    "1"=90, "2"=70, "3"=50, "4"=30, "5"=20
  )
  k_rec <- switch(as.character(k_nivel),
    "1"=60, "2"=45, "3"=30, "4"=20, "5"=10
  )
  
  return(list(
    N_plantio = 20,
    N_cobertura = 0,  # Fixação biológica
    P2O5 = round(p_rec),
    K2O = round(k_rec)
  ))
}

rec_sorgo <- function(p_nivel, k_nivel, produtividade = "media", mo) {
  n_total <- switch(produtividade,
    "baixa"=50, "media"=70, "alta"=90
  )
  if (!is.na(mo) && mo >= 3.0) n_total <- n_total * 0.8
  
  p_rec <- switch(as.character(p_nivel),
    "1"=70, "2"=55, "3"=40, "4"=25, "5"=10
  )
  k_rec <- switch(as.character(k_nivel),
    "1"=60, "2"=45, "3"=30, "4"=15, "5"=0
  )
  
  return(list(
    N_plantio = round(n_total * 0.3),
    N_cobertura = round(n_total * 0.7),
    P2O5 = round(p_rec),
    K2O = round(k_rec)
  ))
}

rec_pastagem <- function(p_nivel, k_nivel, mo, tipo_capim = "braquiaria") {
  n_ano <- switch(tipo_capim,
    "braquiaria" = 100,
    "tifton"     = 150,
    "napier"     = 200,
    "outros"     = 100
  )
  
  p_rec <- switch(as.character(p_nivel),
    "1"=80, "2"=60, "3"=40, "4"=20, "5"=0
  )
  k_rec <- switch(as.character(k_nivel),
    "1"=60, "2"=45, "3"=30, "4"=15, "5"=0
  )
  
  return(list(
    N_plantio = round(n_ano * 0.3),
    N_cobertura = round(n_ano * 0.7),
    P2O5 = round(p_rec),
    K2O = round(k_rec)
  ))
}

# ------------------------------------------------------------------------------
# ABACAXI (Ananas comosus)
# Ref: EMBRAPA Mandioca e Fruticultura (Cruz das Almas, BA)
#      Manual MG 5ª Aprox. — Frutíferas
#      Tolerante à acidez: pH 4.5–5.5, V% 50–60%
# Ciclo: ~18 meses (plantio → colheita)
# Produtividades: baixa <25 t/ha, média 25–50 t/ha, alta >50 t/ha
# ------------------------------------------------------------------------------
rec_abacaxi <- function(p_nivel, k_nivel, produtividade = "media", mo, fase = "plantio") {
  
  # --- NITROGÊNIO ---
  # Aplicado em cobertura parcelado (3–4x); plantio recebe dose mínima
  n_total <- switch(produtividade,
    "baixa"  = 200,   # kg N/ha/ciclo
    "media"  = 280,
    "alta"   = 350
  )
  if (!is.na(mo) && mo >= 3.0) n_total <- round(n_total * 0.85)
  
  # Plantio: ~10% do N (arranque); cobertura: 90%
  n_plantio   <- round(n_total * 0.10)
  n_cobertura <- round(n_total * 0.90)
  
  # --- FÓSFORO (P₂O₅) ---
  # Abacaxi tem baixa exigência em P; aplicado todo no plantio
  p_rec <- switch(as.character(p_nivel),
    "1" = 120,
    "2" =  90,
    "3" =  60,
    "4" =  40,
    "5" =  20
  )
  # Ajuste por produtividade
  if (produtividade == "alta")  p_rec <- round(p_rec * 1.2)
  if (produtividade == "baixa") p_rec <- round(p_rec * 0.8)
  
  # --- POTÁSSIO (K₂O) ---
  # Cultura muito exigente em K (K > N em alguns manejos)
  k_rec <- switch(as.character(k_nivel),
    "1" = 500,
    "2" = 400,
    "3" = 300,
    "4" = 200,
    "5" = 120
  )
  if (produtividade == "alta")  k_rec <- round(k_rec * 1.15)
  if (produtividade == "baixa") k_rec <- round(k_rec * 0.80)
  
  return(list(
    N_plantio   = n_plantio,
    N_cobertura = n_cobertura,
    P2O5        = p_rec,
    K2O         = k_rec
  ))
}

# ------------------------------------------------------------------------------
# DISPATCHER - CHAMA A RECOMENDAÇÃO CORRETA
# ------------------------------------------------------------------------------
recomendar_adubacao <- function(cultura, p_nivel, k_nivel, produtividade = "media", 
                                mo = 2.0, fase = "plantio", n_anterior = "nenhum",
                                toneladas = 80, tipo_arroz = "sequeiro",
                                tipo_capim = "braquiaria") {
  rec <- switch(cultura,
    "milho"    = rec_milho(p_nivel, k_nivel, produtividade, fase, mo, n_anterior),
    "feijao"   = rec_feijao(p_nivel, k_nivel, produtividade, mo),
    "cana"     = rec_cana(p_nivel, k_nivel, fase, mo, toneladas),
    "arroz"    = rec_arroz(p_nivel, k_nivel, produtividade, mo, tipo_arroz),
    "mandioca" = rec_mandioca(p_nivel, k_nivel, mo),
    "amendoim" = rec_amendoim(p_nivel, k_nivel, mo),
    "sorgo"    = rec_sorgo(p_nivel, k_nivel, produtividade, mo),
    "pastagem" = rec_pastagem(p_nivel, k_nivel, mo, tipo_capim),
    "abacaxi"  = rec_abacaxi(p_nivel, k_nivel, produtividade, mo, fase)
  )
  return(rec)
}

# ------------------------------------------------------------------------------
# FONTES DE NUTRIENTES E TEORES
# Nomes usam Unicode: ₂ = \u2082  ₅ = \u2085  ₄ = \u2084  ⁺ = \u207a
# ------------------------------------------------------------------------------
fontes_nitrogenio <- data.frame(
  produto = c(
    "Ureia (45% N)",
    "Sulfato de Am\u00f4nio (21% N)",
    "Nitrato de Am\u00f4nio (33% N)",
    "MAP (10% N)",
    "DAP (18% N)",
    "Nitrato de C\u00e1lcio (15,5% N)"
  ),
  nutriente = c("N","N","N","N","N","N"),
  teor      = c(0.45, 0.21, 0.33, 0.10, 0.18, 0.155),
  preco_ref = c(2.80, 2.20, 3.50, 4.20, 4.50, 5.50),
  stringsAsFactors = FALSE
)

fontes_fosforo <- data.frame(
  produto = c(
    "Superfosfato Simples (18% P\u2082O\u2085)",
    "Superfosfato Triplo (46% P\u2082O\u2085)",
    "MAP (48% P\u2082O\u2085)",
    "DAP (46% P\u2082O\u2085)",
    "Termofosfato (17% P\u2082O\u2085)",
    "Fosfato Natural Reativo (30% P\u2082O\u2085)"
  ),
  nutriente = c("P2O5","P2O5","P2O5","P2O5","P2O5","P2O5"),
  teor      = c(0.18, 0.46, 0.48, 0.46, 0.17, 0.30),
  preco_ref = c(1.80, 3.20, 4.20, 4.50, 1.50, 1.20),
  stringsAsFactors = FALSE
)

fontes_potassio <- data.frame(
  produto = c(
    "KCl (60% K\u2082O)",
    "Sulfato de Pot\u00e1ssio (50% K\u2082O)",
    "KMag (22% K\u2082O)",
    "Nitrato de Pot\u00e1ssio (45% K\u2082O)"
  ),
  nutriente = c("K2O","K2O","K2O","K2O"),
  teor      = c(0.60, 0.50, 0.22, 0.45),
  preco_ref = c(2.90, 5.80, 4.20, 8.50),
  stringsAsFactors = FALSE
)

todas_fontes <- rbind(fontes_nitrogenio, fontes_fosforo, fontes_potassio)

# ------------------------------------------------------------------------------
# CALCULAR CUSTO POR PRODUTO
# ------------------------------------------------------------------------------
calcular_custo_produto <- function(dose_nutriente, teor_produto, preco_kg_produto, area = 1) {
  # dose_nutriente em kg/ha do nutriente puro
  kg_produto_ha <- dose_nutriente / teor_produto
  custo_ha <- kg_produto_ha * preco_kg_produto
  custo_total <- custo_ha * area
  return(list(
    kg_produto_ha = round(kg_produto_ha, 1),
    custo_ha = round(custo_ha, 2),
    custo_total = round(custo_total, 2)
  ))
}
