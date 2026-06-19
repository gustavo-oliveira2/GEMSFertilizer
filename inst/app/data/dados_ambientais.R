# ==============================================================================
# DADOS AMBIENTAIS — Open-Meteo API (gratuita, sem chave, cobertura nacional)
# https://open-meteo.com/en/docs
# ==============================================================================

# Busca dados climáticos históricos para um município
# lat/lon: coordenadas do centroide do município
# inicio/fim: "YYYY-MM-DD"
buscar_clima_municipio <- function(lat, lon, inicio, fim,
                                   variaveis = c("precipitation_sum",
                                                  "temperature_2m_max",
                                                  "temperature_2m_min",
                                                  "et0_fao_evapotranspiration")) {
  url_base <- "https://archive-api.open-meteo.com/v1/archive"
  params <- paste0(
    "?latitude=", lat,
    "&longitude=", lon,
    "&start_date=", inicio,
    "&end_date=", fim,
    "&daily=", paste(variaveis, collapse = ","),
    "&timezone=America%2FSao_Paulo",
    "&wind_speed_unit=ms"
  )

  resposta <- tryCatch(
    httr::GET(paste0(url_base, params), httr::timeout(20)),
    error = function(e) NULL
  )

  if (is.null(resposta) || httr::status_code(resposta) != 200) {
    return(list(erro = "Não foi possível conectar à API Open-Meteo. Verifique sua conexão."))
  }

  dados <- tryCatch(
    jsonlite::fromJSON(httr::content(resposta, "text", encoding = "UTF-8")),
    error = function(e) NULL
  )

  if (is.null(dados) || is.null(dados$daily)) {
    return(list(erro = "Resposta inválida da API de clima."))
  }

  df <- as.data.frame(dados$daily)
  names(df)[names(df) == "time"] <- "data"
  df$data <- as.Date(df$data)

  # Renomeia para nomes amigáveis
  nomes_amig <- c(
    precipitation_sum         = "chuva_mm",
    temperature_2m_max        = "temp_max_c",
    temperature_2m_min        = "temp_min_c",
    et0_fao_evapotranspiration = "et0_mm"
  )
  for (orig in names(nomes_amig)) {
    if (orig %in% names(df)) names(df)[names(df) == orig] <- nomes_amig[orig]
  }

  # Balanço hídrico simples: chuva - ET0
  if (all(c("chuva_mm", "et0_mm") %in% names(df))) {
    df$balanco_hidrico_mm <- df$chuva_mm - df$et0_mm
  }

  # Temperatura média
  if (all(c("temp_max_c", "temp_min_c") %in% names(df))) {
    df$temp_media_c <- round((df$temp_max_c + df$temp_min_c) / 2, 1)
  }

  list(erro = NULL, dados = df, lat = lat, lon = lon,
       inicio = inicio, fim = fim)
}

# Agrega dados diários em resumo mensal
resumo_mensal <- function(df_diario) {
  df <- df_diario
  df$mes <- format(df$data, "%Y-%m")

  mensal <- data.frame(
    mes = unique(df$mes),
    stringsAsFactors = FALSE
  )

  if ("chuva_mm" %in% names(df)) {
    mensal$chuva_total_mm <- tapply(df$chuva_mm, df$mes, sum, na.rm = TRUE)[mensal$mes]
  }
  if ("temp_media_c" %in% names(df)) {
    mensal$temp_media_c <- round(tapply(df$temp_media_c, df$mes, mean, na.rm = TRUE)[mensal$mes], 1)
  }
  if ("et0_mm" %in% names(df)) {
    mensal$et0_total_mm <- tapply(df$et0_mm, df$mes, sum, na.rm = TRUE)[mensal$mes]
  }
  if ("balanco_hidrico_mm" %in% names(df)) {
    mensal$balanco_mm <- round(tapply(df$balanco_hidrico_mm, df$mes, sum, na.rm = TRUE)[mensal$mes], 1)
  }

  rownames(mensal) <- NULL
  mensal
}

# Classificação do balanço hídrico para tomada de decisão
classificar_balanco <- function(balanco_mm) {
  ifelse(balanco_mm > 50,  "Excedente hídrico — risco de lixiviação de nutrientes",
  ifelse(balanco_mm > 0,   "Adequado — condições favoráveis para absorção",
  ifelse(balanco_mm > -50, "Déficit leve — monitorar irrigação",
  ifelse(balanco_mm > -100,"Déficit moderado — adubar com cautela; priorizar irrigação",
                            "Déficit severo — adubação ineficiente sem irrigação"))))
}

# Tabela de municípios do Banco Regional com coordenadas (centroide IBGE)
# Populada dinamicamente a partir do banco regional carregado
extrair_coords_municipios <- function(df_regional) {
  if (!all(c("municipio","latitude","longitude") %in% names(df_regional))) return(NULL)
  coords <- df_regional[!is.na(df_regional$latitude) & !is.na(df_regional$longitude),
                          c("municipio","latitude","longitude")]
  coords <- coords[!duplicated(coords$municipio), ]
  coords
}

