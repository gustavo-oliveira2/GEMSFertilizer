# ==============================================================================
# TABELAS DE INTERPRETAÇÃO DE SOLO
# Manual de Minas Gerais - 5ª Aproximação (2023)
# Manual de Recomendações de Adubação e Calagem para Sergipe (EMBRAPA/UFS)
# ==============================================================================

# ------------------------------------------------------------------------------
# INTERPRETAÇÃO DE pH
# ------------------------------------------------------------------------------
interpretar_ph <- function(ph, metodo = "agua") {
  if (metodo == "agua") {
    if (ph < 4.5) return(list(classe = "Muito Ácido", cor = "#d32f2f", icone = "⚠️"))
    else if (ph < 5.5) return(list(classe = "Ácido", cor = "#f57c00", icone = "⚡"))
    else if (ph < 6.0) return(list(classe = "Moderadamente Ácido", cor = "#fbc02d", icone = "⚡"))
    else if (ph < 6.5) return(list(classe = "Levemente Ácido", cor = "#7cb342", icone = "✓"))
    else if (ph <= 7.0) return(list(classe = "Adequado", cor = "#2e7d32", icone = "✓"))
    else return(list(classe = "Alcalino", cor = "#1565c0", icone = "⚠️"))
  }
}

# ------------------------------------------------------------------------------
# INTERPRETAÇÃO MATÉRIA ORGÂNICA (dag/kg)
# ------------------------------------------------------------------------------
interpretar_mo <- function(mo) {
  if (mo < 1.0) return(list(classe = "Muito Baixo", cor = "#d32f2f"))
  else if (mo < 2.0) return(list(classe = "Baixo", cor = "#f57c00"))
  else if (mo < 3.0) return(list(classe = "Médio", cor = "#fbc02d"))
  else if (mo < 4.0) return(list(classe = "Alto", cor = "#7cb342"))
  else return(list(classe = "Muito Alto", cor = "#2e7d32"))
}

# ------------------------------------------------------------------------------
# INTERPRETAÇÃO FÓSFORO (mg/dm³) - Mehlich-1
# Varia de acordo com textura do solo
# ------------------------------------------------------------------------------
interpretar_p <- function(p, argila) {
  # Classe textural
  if (argila <= 15) {
    limites <- c(3, 6, 12, 20)  # Muito Argiloso→Arenoso (invertido)
  } else if (argila <= 35) {
    limites <- c(4, 8, 16, 25)
  } else if (argila <= 60) {
    limites <- c(6, 12, 20, 30)
  } else {
    limites <- c(8, 16, 25, 35)
  }
  
  if (p < limites[1]) return(list(classe = "Muito Baixo", cor = "#d32f2f", nivel = 1))
  else if (p < limites[2]) return(list(classe = "Baixo", cor = "#f57c00", nivel = 2))
  else if (p < limites[3]) return(list(classe = "Médio", cor = "#fbc02d", nivel = 3))
  else if (p < limites[4]) return(list(classe = "Bom", cor = "#7cb342", nivel = 4))
  else return(list(classe = "Muito Bom", cor = "#2e7d32", nivel = 5))
}

# ------------------------------------------------------------------------------
# INTERPRETAÇÃO POTÁSSIO (mg/dm³)
# ------------------------------------------------------------------------------
interpretar_k <- function(k, ctc) {
  k_cmolc <- k / 391  # Conversão mg/dm³ → cmolc/dm³
  sat_k <- (k_cmolc / ctc) * 100
  
  if (sat_k < 1.5) return(list(classe = "Muito Baixo", cor = "#d32f2f", nivel = 1))
  else if (sat_k < 3.0) return(list(classe = "Baixo", cor = "#f57c00", nivel = 2))
  else if (sat_k < 5.0) return(list(classe = "Médio", cor = "#fbc02d", nivel = 3))
  else if (sat_k < 7.0) return(list(classe = "Bom", cor = "#7cb342", nivel = 4))
  else return(list(classe = "Muito Bom", cor = "#2e7d32", nivel = 5))
}

# ------------------------------------------------------------------------------
# INTERPRETAÇÃO CÁLCIO (cmolc/dm³)
# ------------------------------------------------------------------------------
interpretar_ca <- function(ca) {
  if (ca < 0.5) return(list(classe = "Muito Baixo", cor = "#d32f2f"))
  else if (ca < 1.5) return(list(classe = "Baixo", cor = "#f57c00"))
  else if (ca < 3.0) return(list(classe = "Médio", cor = "#fbc02d"))
  else if (ca < 5.0) return(list(classe = "Bom", cor = "#7cb342"))
  else return(list(classe = "Muito Bom", cor = "#2e7d32"))
}

# ------------------------------------------------------------------------------
# INTERPRETAÇÃO MAGNÉSIO (cmolc/dm³)
# ------------------------------------------------------------------------------
interpretar_mg <- function(mg) {
  if (mg < 0.3) return(list(classe = "Muito Baixo", cor = "#d32f2f"))
  else if (mg < 0.8) return(list(classe = "Baixo", cor = "#f57c00"))
  else if (mg < 1.5) return(list(classe = "Médio", cor = "#fbc02d"))
  else if (mg < 2.5) return(list(classe = "Bom", cor = "#7cb342"))
  else return(list(classe = "Muito Bom", cor = "#2e7d32"))
}

# ------------------------------------------------------------------------------
# INTERPRETAÇÃO ENXOFRE (mg/dm³)
# ------------------------------------------------------------------------------
interpretar_s <- function(s) {
  if (s < 5) return(list(classe = "Baixo", cor = "#d32f2f"))
  else if (s < 10) return(list(classe = "Médio", cor = "#fbc02d"))
  else return(list(classe = "Adequado", cor = "#2e7d32"))
}

# ------------------------------------------------------------------------------
# INTERPRETAÇÃO MICRONUTRIENTES
# ------------------------------------------------------------------------------
interpretar_micro <- function(valor, nutriente) {
  limites <- list(
    B  = c(0.10, 0.20, 0.60),   # mg/dm³ (água quente)
    Cu = c(0.20, 0.40, 0.80),   # mg/dm³ (DTPA)
    Fe = c(5.0,  9.0,  18.0),   # mg/dm³ (DTPA)
    Mn = c(1.5,  3.0,  6.0),    # mg/dm³ (DTPA)
    Zn = c(0.50, 1.00, 2.00)    # mg/dm³ (DTPA)
  )
  
  lim <- limites[[nutriente]]
  if (is.null(lim)) return(list(classe = "N/D", cor = "#9e9e9e"))
  
  if (valor < lim[1]) return(list(classe = "Baixo", cor = "#d32f2f"))
  else if (valor < lim[2]) return(list(classe = "Médio", cor = "#fbc02d"))
  else if (valor < lim[3]) return(list(classe = "Bom", cor = "#7cb342"))
  else return(list(classe = "Alto", cor = "#2e7d32"))
}

# ------------------------------------------------------------------------------
# CÁLCULO DA CTC
# ------------------------------------------------------------------------------
calcular_ctc <- function(ca, mg, k, al, h_al) {
  k_cmolc <- k / 391
  ctc <- ca + mg + k_cmolc + h_al
  return(round(ctc, 2))
}

# ------------------------------------------------------------------------------
# SATURAÇÃO POR BASES (V%)
# ------------------------------------------------------------------------------
calcular_v <- function(ca, mg, k, ctc) {
  k_cmolc <- k / 391
  sb <- ca + mg + k_cmolc
  v <- (sb / ctc) * 100
  return(round(v, 1))
}

interpretar_v <- function(v) {
  if (v < 25) return(list(classe = "Muito Baixo / Distrófico", cor = "#d32f2f"))
  else if (v < 50) return(list(classe = "Baixo / Distrófico", cor = "#f57c00"))
  else if (v < 70) return(list(classe = "Médio / Mesotrófico", cor = "#fbc02d"))
  else if (v < 85) return(list(classe = "Adequado / Eutrófico", cor = "#7cb342"))
  else return(list(classe = "Alto / Eutrófico", cor = "#2e7d32"))
}

# ------------------------------------------------------------------------------
# SATURAÇÃO POR ALUMÍNIO (m%)
# ------------------------------------------------------------------------------
calcular_m <- function(al, ca, mg, k) {
  k_cmolc <- k / 391
  sb <- ca + mg + k_cmolc
  if (sb + al == 0) return(0)
  m <- (al / (sb + al)) * 100
  return(round(m, 1))
}

interpretar_m <- function(m) {
  if (m < 15) return(list(classe = "Baixo", cor = "#2e7d32"))
  else if (m < 30) return(list(classe = "Médio", cor = "#fbc02d"))
  else if (m < 50) return(list(classe = "Alto", cor = "#f57c00"))
  else return(list(classe = "Muito Alto / Tóxico", cor = "#d32f2f"))
}

# ------------------------------------------------------------------------------
# CULTURAS DISPONÍVEIS
# ------------------------------------------------------------------------------
culturas_lista <- list(
  "milho"        = "Milho (Zea mays)",
  "feijao"       = "Feijão (Phaseolus vulgaris)",
  "cana"         = "Cana-de-açúcar (Saccharum spp.)",
  "arroz"        = "Arroz (Oryza sativa)",
  "mandioca"     = "Mandioca (Manihot esculenta)",
  "amendoim"     = "Amendoim (Arachis hypogaea)",
  "sorgo"        = "Sorgo (Sorghum bicolor)",
  "pastagem"     = "Capim/Pastagem (Brachiaria spp.)",
  "abacaxi"      = "Abacaxi (Ananas comosus)"
)

# Saturações de bases alvo por cultura (V%) - Manual MG 5ª aprox. / Sergipe
v_alvo_cultura <- list(
  milho    = list(mg = 55, se = 60),
  feijao   = list(mg = 60, se = 65),
  cana     = list(mg = 60, se = 60),
  arroz    = list(mg = 50, se = 50),
  mandioca = list(mg = 50, se = 50),
  amendoim = list(mg = 60, se = 65),
  sorgo    = list(mg = 50, se = 55),
  pastagem = list(mg = 50, se = 55),
  # Abacaxi: tolera solo ácido, V% alvo moderado
  # Ref: Manual MG 5ª aprox. + EMBRAPA Mandioca e Fruticultura
  abacaxi  = list(mg = 50, se = 50)
)
