# ==============================================================================
# MÓDULO BANCO REGIONAL
# Estatísticas regionais, benchmarking e mapas coropléticos de fertilidade
# a partir do template_banco_regional.xlsx (179 análises / 9 municípios / 3 anos)
# ==============================================================================

# ------------------------------------------------------------------------------
# COLUNAS E METADADOS
# ------------------------------------------------------------------------------
colunas_solo_regional <- c("ph","mo","p","k","ca","mg","al","h_al","argila",
                            "ctc","v_pct","m_pct","s","b","cu","fe","mn","zn",
                            "produtividade","dose_n","dose_calcario","dose_gesso")

nomes_atributos_regional <- c(
  ph     = "pH (H\u2082O)",
  mo     = "M.O. (dag kg\u207b\u00b9)",
  p      = "P-Mehlich (mg dm\u207b\u00b3)",
  k      = "K\u207a (mg dm\u207b\u00b3)",
  ca     = "Ca\u00b2\u207a (cmol\u1d04 dm\u207b\u00b3)",
  mg     = "Mg\u00b2\u207a (cmol\u1d04 dm\u207b\u00b3)",
  al     = "Al\u00b3\u207a (cmol\u1d04 dm\u207b\u00b3)",
  h_al   = "(H+Al) (cmol\u1d04 dm\u207b\u00b3)",
  argila = "Argila (%)",
  ctc    = "CTC (cmol\u1d04 dm\u207b\u00b3)",
  v_pct  = "Satura\u00e7\u00e3o por Bases V%",
  m_pct  = "Satura\u00e7\u00e3o por Al\u00b3\u207a m%",
  s      = "S-SO\u2084\u00b2\u207b (mg dm\u207b\u00b3)",
  b      = "B (mg dm\u207b\u00b3)",
  cu     = "Cu-DTPA (mg dm\u207b\u00b3)",
  fe     = "Fe-DTPA (mg dm\u207b\u00b3)",
  mn     = "Mn-DTPA (mg dm\u207b\u00b3)",
  zn     = "Zn-DTPA (mg dm\u207b\u00b3)",
  produtividade = "Produtividade (kg ha\u207b\u00b9)",
  dose_n        = "Dose N aplicada (kg ha\u207b\u00b9)",
  dose_calcario = "Dose Calc\u00e1rio aplicada (t ha\u207b\u00b9)",
  dose_gesso    = "Dose Gesso aplicada (t ha\u207b\u00b9)"
)

# ------------------------------------------------------------------------------
# CARREGAR E PREPARAR DADOS
# Lê o arquivo .xlsx (aba "Dados") preenchido a partir do template
# Calcula CTC, V% e m% quando ausentes
# ------------------------------------------------------------------------------
carregar_banco_regional <- function(filepath) {

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("O pacote 'readxl' \u00e9 necess\u00e1rio para ler o arquivo .xlsx. ",
         "Instale com: install.packages('readxl')")
  }

  df <- tryCatch(
    readxl::read_excel(filepath, sheet = "Dados", skip = 1, col_names = TRUE),
    error = function(e) NULL
  )

  if (is.null(df) || nrow(df) == 0) {
    stop("N\u00e3o foi poss\u00edvel ler a aba 'Dados' do arquivo. ",
         "Verifique se o arquivo segue o template fornecido.")
  }

  # ----------------------------------------------------------------------
  # RENOMEAR COLUNAS POR POSI\u00c7\u00c3O (n\u00e3o por texto do cabe\u00e7alho)
  #
  # Comparar pelo texto do cabe\u00e7alho \u00e9 fr\u00e1gil: o Excel pode alterar a
  # codifica\u00e7\u00e3o de quebras de linha (\\n -> \\r\\n ou _x000D_) e de
  # caracteres especiais (\u00b2, \u2082 etc.) sempre que o arquivo \u00e9 salvo,
  # mesmo sem o usu\u00e1rio editar os cabe\u00e7alhos. Por isso usamos a ORDEM
  # FIXA de 30 colunas do template, que \u00e9 est\u00e1vel mesmo ap\u00f3s edi\u00e7\u00f5es
  # no Excel (desde que colunas n\u00e3o sejam reordenadas/removidas).
  # ----------------------------------------------------------------------
  nomes_esperados <- c(
    "codigo_amostra","municipio","ano","data_coleta","cultura","profundidade_cm",
    "ph","mo","p","k","ca","mg","al","h_al","argila","ctc","v_pct","m_pct",
    "produtividade","dose_n","dose_calcario","dose_gesso","tipo_parcela","obs_produtividade",
    "s","b","cu","fe","mn","zn","latitude","longitude","observacoes"
  )

  if (ncol(df) < length(nomes_esperados)) {
    stop(paste0(
      "O arquivo cont\u00e9m ", ncol(df), " coluna(s) na aba 'Dados', mas o template ",
      "possui ", length(nomes_esperados), ". Verifique se nenhuma coluna foi removida ",
      "ou reordenada \u2014 utilize sempre o arquivo template_banco_regional.xlsx original ",
      "como base, copiando apenas os DADOS (sem alterar cabe\u00e7alhos ou a ordem das colunas)."
    ))
  }

  df <- df[, seq_along(nomes_esperados)]
  names(df) <- nomes_esperados

  # Remove linhas totalmente vazias (sem município ou pH)
  df <- df[!is.na(df$municipio) & !is.na(df$ph), ]

  if (nrow(df) == 0) {
    stop("Nenhuma linha v\u00e1lida encontrada (verifique se 'munic\u00edpio' e 'pH' est\u00e3o preenchidos).")
  }

  # Garante colunas numéricas
  for (col in colunas_solo_regional) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    } else {
      df[[col]] <- NA_real_
    }
  }
  df$ano <- suppressWarnings(as.integer(df$ano))

  # Calcula CTC, V%, m% quando ausentes (usa as mesmas funções de tabelas_solo.R)
  precisa_calc <- is.na(df$ctc) | is.na(df$v_pct) | is.na(df$m_pct)
  if (any(precisa_calc) && all(c("ca","mg","k","al","h_al") %in% names(df))) {
    for (i in which(precisa_calc)) {
      ca_i <- df$ca[i]; mg_i <- df$mg[i]; k_i <- df$k[i]
      al_i <- df$al[i]; hal_i <- df$h_al[i]
      if (!any(is.na(c(ca_i, mg_i, k_i, al_i, hal_i)))) {
        if (is.na(df$ctc[i]))   df$ctc[i]   <- calcular_ctc(ca_i, mg_i, k_i, al_i, hal_i)
        if (is.na(df$v_pct[i])) df$v_pct[i] <- calcular_v(ca_i, mg_i, k_i, df$ctc[i])
        if (is.na(df$m_pct[i])) df$m_pct[i] <- calcular_m(al_i, ca_i, mg_i, k_i)
      }
    }
  }

  df$municipio <- trimws(df$municipio)

  df
}

# ------------------------------------------------------------------------------
# ESTATÍSTICAS POR MUNICÍPIO (E ANO, OPCIONAL)
# ------------------------------------------------------------------------------
estatisticas_regionais <- function(df, atributo, ano = NULL) {

  if (!atributo %in% names(df)) {
    stop(paste0("Atributo '", atributo, "' n\u00e3o encontrado nos dados."))
  }

  d <- df[!is.na(df[[atributo]]), ]
  if (!is.null(ano) && ano != "Todos") {
    d <- d[d$ano == as.integer(ano), ]
  }

  if (nrow(d) == 0) {
    return(data.frame(municipio = character(0), n = integer(0),
                       media = numeric(0), mediana = numeric(0),
                       dp = numeric(0), p10 = numeric(0), p90 = numeric(0)))
  }

  agg <- do.call(rbind, lapply(split(d[[atributo]], d$municipio), function(x) {
    data.frame(
      n       = length(x),
      media   = round(mean(x, na.rm = TRUE), 2),
      mediana = round(median(x, na.rm = TRUE), 2),
      dp      = round(sd(x, na.rm = TRUE), 2),
      p10     = round(quantile(x, 0.10, na.rm = TRUE), 2),
      p90     = round(quantile(x, 0.90, na.rm = TRUE), 2)
    )
  }))
  agg$municipio <- rownames(agg)
  rownames(agg) <- NULL
  agg[, c("municipio","n","media","mediana","dp","p10","p90")]
}

# ------------------------------------------------------------------------------
# SÉRIE TEMPORAL (médias por ano, geral ou por município)
# ------------------------------------------------------------------------------
serie_temporal_regional <- function(df, atributo, municipio = NULL) {
  d <- df[!is.na(df[[atributo]]) & !is.na(df$ano), ]
  if (!is.null(municipio) && municipio != "Todos") {
    d <- d[d$municipio == municipio, ]
  }
  if (nrow(d) == 0) return(data.frame(ano = integer(0), media = numeric(0), n = integer(0)))

  agg <- do.call(rbind, lapply(split(d[[atributo]], d$ano), function(x) {
    data.frame(media = round(mean(x, na.rm = TRUE), 2), n = length(x))
  }))
  agg$ano <- as.integer(rownames(agg))
  rownames(agg) <- NULL
  agg[order(agg$ano), c("ano","media","n")]
}

# ------------------------------------------------------------------------------
# BENCHMARKING — compara um valor individual com a distribuição regional
# Retorna percentil aproximado e classificação relativa
# ------------------------------------------------------------------------------
benchmark_valor <- function(df, atributo, municipio, valor) {
  d <- df[!is.na(df[[atributo]]) & df$municipio == municipio, ]

  if (nrow(d) < 3) {
    return(list(
      disponivel = FALSE,
      mensagem   = paste0("Dados regionais insuficientes para ", municipio,
                          " (m\u00ednimo de 3 amostras necess\u00e1rio).")
    ))
  }

  vetor <- d[[atributo]]
  pct   <- round(mean(vetor <= valor) * 100, 0)
  media_reg <- round(mean(vetor), 2)

  classe <- if (pct < 25) "abaixo da m\u00e9dia regional"
            else if (pct < 50) "pr\u00f3ximo \u00e0 m\u00e9dia regional (inferior)"
            else if (pct < 75) "pr\u00f3ximo \u00e0 m\u00e9dia regional (superior)"
            else "acima da m\u00e9dia regional"

  list(
    disponivel = TRUE,
    percentil  = pct,
    media_regional = media_reg,
    n          = nrow(d),
    classe     = classe,
    mensagem   = paste0(
      "Percentil ", pct, " em ", municipio, " (n=", nrow(d), "). ",
      "M\u00e9dia regional: ", media_reg, ". Esta amostra est\u00e1 ", classe, "."
    )
  )
}

# ------------------------------------------------------------------------------
# MAPA COROPLÉTICO (leaflet + geobr)
# Requer pacotes: geobr, sf, leaflet
# uf: sigla do estado (ex: "SE")
# ------------------------------------------------------------------------------
mapa_coropletico_regional <- function(df_stats, uf = "SE", coluna_valor = "media",
                                      titulo = "", paleta = "YlGn") {

  faltam <- c("geobr","sf","leaflet")[!sapply(c("geobr","sf","leaflet"), requireNamespace, quietly = TRUE)]
  if (length(faltam) > 0) {
    stop(paste0(
      "Pacote(s) necess\u00e1rio(s) ausente(s): ", paste(faltam, collapse = ", "), ". ",
      "Instale com: install.packages(c(", paste0("'", faltam, "'", collapse=", "), "))"
    ))
  }

  # Baixa malha municipal do estado (cache automático do geobr)
  malha <- geobr::read_municipality(code_muni = uf, year = 2020, showProgress = FALSE)

  # Normaliza nomes para casar (remove acentos, maiúsculas)
  normalizar <- function(x) {
    x <- toupper(x)
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    trimws(x)
  }

  malha$.nome_norm    <- normalizar(malha$name_muni)
  df_stats$.nome_norm <- normalizar(df_stats$municipio)

  malha_join <- merge(malha, df_stats, by.x = ".nome_norm", by.y = ".nome_norm", all.x = TRUE)

  # Municípios não encontrados (para aviso ao usuário)
  nao_encontrados <- setdiff(df_stats$.nome_norm, malha$.nome_norm)

  pal <- leaflet::colorNumeric(
    palette  = paleta,
    domain   = malha_join[[coluna_valor]],
    na.color = "#e0e0e0"
  )

  labels <- sprintf(
    "<b>%s</b><br/>%s: %s<br/>n = %s",
    malha_join$name_muni,
    titulo,
    ifelse(is.na(malha_join[[coluna_valor]]), "sem dado", as.character(malha_join[[coluna_valor]])),
    ifelse(is.na(malha_join$n), "-", as.character(malha_join$n))
  )
  labels <- lapply(labels, shiny::HTML)

  m <- leaflet::leaflet(malha_join) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolygons(
      fillColor   = ~pal(get(coluna_valor)),
      weight      = 1.5,
      opacity     = 1,
      color       = "white",
      fillOpacity = 0.75,
      highlightOptions = leaflet::highlightOptions(
        weight = 3, color = "#1b4332", fillOpacity = 0.9, bringToFront = TRUE
      ),
      label        = labels,
      labelOptions = leaflet::labelOptions(
        style    = list("font-weight" = "normal", padding = "6px 10px"),
        textsize = "13px"
      )
    ) |>
    leaflet::addLegend(
      pal      = pal,
      values   = ~get(coluna_valor),
      opacity  = 0.8,
      title    = titulo,
      position = "bottomright"
    )

  list(mapa = m, nao_encontrados = nao_encontrados)
}

# ------------------------------------------------------------------------------
# RESUMO GERAL DO BANCO (para cabeçalho da aba)
# ------------------------------------------------------------------------------
resumo_banco_regional <- function(df) {
  list(
    n_amostras  = nrow(df),
    n_municipios = length(unique(df$municipio)),
    anos        = sort(unique(df$ano[!is.na(df$ano)])),
    municipios  = sort(unique(df$municipio))
  )
}
