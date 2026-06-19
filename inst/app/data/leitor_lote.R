# ==============================================================================
# LEITOR DE LOTE — lê qualquer Excel/CSV com colunas de fertilidade
# Estratégia: normaliza nomes de colunas (sem acento, minúsculo) e mapeia
# por sinônimos. Robusto a variações de lab, ordem de colunas e encoding.
# ==============================================================================

# Mapa de sinônimos: qualquer variação → nome padrão do app
SINONIMOS_COLUNAS <- list(
  id_amostra   = c("id_amostra","id","amostra","identificacao","identificação",
                    "numeracao","numeração","codigo","código","sample","sample_id",
                    "numero","número","talhao","talhão","area","área"),
  municipio    = c("municipio","município","cidade","local","localidade","location"),
  ph           = c("ph","ph_h2o","ph_agua","ph_agua","reacao","reação","ph (h2o)",
                    "ph em agua","ph em água"),
  mo           = c("mo","m.o.","materia_organica","matéria_orgânica","materia organica",
                    "matéria orgânica","carbono_organico","c_organico","co","organic_matter"),
  p            = c("p","fosforo","fósforo","p_mehlich","p-mehlich","fosforo_disponivel",
                    "fósforo_disponível","phosphorus","p (mg/dm3)","p mehlich"),
  k            = c("k","potassio","potássio","k_mehlich","k-mehlich","potassio_disponivel",
                    "potassium","k (mg/dm3)","k mehlich"),
  ca           = c("ca","calcio","cálcio","ca_trocavel","ca_trocável","calcium",
                    "ca (cmolc/dm3)","ca2+"),
  mg           = c("mg","magnesio","magnésio","mg_trocavel","mg_trocável","magnesium",
                    "mg (cmolc/dm3)","mg2+"),
  al           = c("al","aluminio","alumínio","al_trocavel","al_trocável","aluminium",
                    "al (cmolc/dm3)","al3+"),
  h_al         = c("h_al","h+al","h + al","h_mais_al","acidez_potencial",
                    "hidrog_alum","hidrogenio_aluminio","hidrogênio_alumínio",
                    "h+al (cmolc/dm3)","acidez potencial"),
  argila       = c("argila","clay","teor_argila","argila_pct","argila (%)","granulometria"),
  p_rem        = c("p_rem","p-rem","p_remanescente","fosforo_remanescente",
                    "fósforo_remanescente","phosphorus_remaining"),
  s            = c("s","enxofre","sulfur","s-so4","s_so4","s (mg/dm3)"),
  b            = c("b","boro","boron","b (mg/dm3)"),
  cu           = c("cu","cobre","copper","cu (mg/dm3)","cu-dtpa"),
  fe           = c("fe","ferro","iron","fe (mg/dm3)","fe-dtpa"),
  mn           = c("mn","manganes","manganês","manganese","mn (mg/dm3)","mn-dtpa"),
  zn           = c("zn","zinco","zinc","zn (mg/dm3)","zn-dtpa"),
  cultura      = c("cultura","crop","cultivo","especie","espécie"),
  produtividade= c("produtividade","productivity","prod","nivel_prod","nível_prod",
                    "expectativa","meta_prod"),
  prnt         = c("prnt","prnt_calcario","prnt_calcário","corretivo_prnt"),
  observacoes  = c("observacoes","observações","obs","notas","notes","comentarios",
                    "comentários","remarks")
)

# Normaliza nome de coluna para ASCII minúsculo sem caracteres especiais
norm_col <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9_]", "_", x)    # substitui qualquer não-alfanumérico por _
  x <- gsub("_+", "_", x)             # remove underscores duplos
  x <- gsub("^_|_$", "", x)           # remove _ inicial/final
  x
}

# Mapeia nome normalizado de coluna → nome padrão do app
mapear_coluna <- function(nome_norm) {
  for (padrao in names(SINONIMOS_COLUNAS)) {
    sinonimos_norm <- sapply(SINONIMOS_COLUNAS[[padrao]], norm_col)
    if (nome_norm %in% sinonimos_norm) return(padrao)
  }
  NULL
}

# ==============================================================================
# FUNÇÃO PRINCIPAL: ler_lote
# Retorna lista com $dados (data.frame) e $avisos (character)
# ==============================================================================
ler_lote <- function(filepath) {
  ext <- tolower(tools::file_ext(filepath))

  df_raw <- if (ext %in% c("xlsx","xls")) {
    ler_excel_lote(filepath)
  } else if (ext %in% c("csv","txt")) {
    ler_csv_lote(filepath)
  } else {
    return(list(
      erro = paste0("Formato '.", ext, "' não suportado. Use XLSX, XLS ou CSV.")
    ))
  }

  if (!is.null(df_raw$erro)) return(df_raw)
  df <- df_raw$df

  # Normaliza nomes das colunas e mapeia para padrão
  nomes_norm  <- sapply(names(df), norm_col)
  nomes_mapeados <- sapply(nomes_norm, mapear_coluna)

  # Renomeia colunas mapeadas, mantém as não-mapeadas com prefixo "extra_"
  nomes_finais <- ifelse(
    !sapply(nomes_mapeados, is.null),
    unlist(ifelse(sapply(nomes_mapeados, is.null), names(df), nomes_mapeados)),
    paste0("extra_", nomes_norm)
  )
  names(df) <- nomes_finais

  # Remove colunas duplicadas (mantém a primeira ocorrência)
  df <- df[, !duplicated(names(df)), drop = FALSE]

  # Verifica colunas obrigatórias
  obrigatorias <- c("ph","p","k","ca","mg","al","h_al")
  faltando <- obrigatorias[!obrigatorias %in% names(df)]

  avisos <- character(0)
  if (length(faltando) > 0) {
    avisos <- c(avisos, paste0(
      "Colunas obrigatórias não encontradas: ",
      paste(toupper(faltando), collapse=", "),
      ". Verifique os nomes das colunas no arquivo."
    ))
  }

  # Garante id_amostra
  if (!"id_amostra" %in% names(df)) {
    df$id_amostra <- paste0("AM-", sprintf("%03d", seq_len(nrow(df))))
    avisos <- c(avisos, "Coluna 'id_amostra' não encontrada — IDs gerados automaticamente.")
  }

  # Converte colunas numéricas
  colunas_num <- c("ph","mo","p","k","ca","mg","al","h_al","argila","p_rem",
                   "s","b","cu","fe","mn","zn","prnt")
  for (col in intersect(colunas_num, names(df))) {
    df[[col]] <- suppressWarnings(
      as.numeric(gsub(",", ".", as.character(df[[col]])))
    )
  }

  # Remove linhas completamente vazias
  cols_num_presentes <- intersect(colunas_num, names(df))
  linhas_validas <- apply(df[, cols_num_presentes, drop=FALSE], 1,
                          function(r) sum(!is.na(r)) >= 3)
  n_removidas <- sum(!linhas_validas)
  df <- df[linhas_validas, , drop=FALSE]

  if (n_removidas > 0)
    avisos <- c(avisos, paste0(n_removidas, " linha(s) removida(s) por terem menos de 3 valores numéricos."))

  if (nrow(df) == 0)
    return(list(erro = paste0(
      "Nenhuma linha válida encontrada. ",
      if (length(avisos)>0) paste(avisos, collapse=" ") else
        "Verifique se o arquivo usa ponto ou vírgula como separador decimal."
    )))

  rownames(df) <- NULL
  list(erro=NULL, dados=df, avisos=avisos, n=nrow(df))
}

# ------------------------------------------------------------------------------
ler_excel_lote <- function(filepath) {
  if (!requireNamespace("readxl", quietly=TRUE))
    return(list(erro="Pacote 'readxl' necessário. Instale com install.packages('readxl')."))

  abas <- tryCatch(readxl::excel_sheets(filepath), error=function(e) NULL)
  if (is.null(abas))
    return(list(erro="Não foi possível abrir o arquivo Excel."))

  for (aba in abas) {
    if (grepl("guia|instruc|readme|legenda", tolower(aba))) next

    # Lê sem cabeçalho para inspecionar as primeiras linhas
    df_raw <- tryCatch(
      readxl::read_excel(filepath, sheet=aba, col_names=FALSE,
                          n_max=10, .name_repair="minimal"),
      error=function(e) NULL
    )
    if (is.null(df_raw) || nrow(df_raw) < 1) next

    # Procura a linha de cabeçalho real:
    # É a linha onde pelo menos 3 células coincidem com nomes de parâmetros conhecidos
    termos_chave <- c("ph","mo","p","k","ca","mg","al","h_al","h al","h+al",
                      "fosforo","fósforo","potassio","potássio","calcio","cálcio",
                      "magnesio","magnésio","aluminio","alumínio","argila",
                      "id","amostra","identificacao","sample","municipio")
    cab_row <- NULL
    for (r in seq_len(nrow(df_raw))) {
      celulas <- tolower(trimws(as.character(unlist(df_raw[r, ]))))
      celulas <- celulas[!is.na(celulas) & celulas != "na" & celulas != ""]
      # Normaliza acentos para comparação
      celulas_ascii <- iconv(celulas, to="ASCII//TRANSLIT", sub="")
      celulas_ascii <- gsub("[^a-z0-9_]", "", celulas_ascii)
      n_match <- sum(celulas_ascii %in% gsub("[^a-z0-9_]","",termos_chave))
      if (n_match >= 3) { cab_row <- r; break }
    }

    if (is.null(cab_row)) next

    # Lê o arquivo a partir da linha de cabeçalho encontrada
    df <- tryCatch(
      readxl::read_excel(filepath, sheet=aba, col_names=TRUE,
                          skip=cab_row - 1, .name_repair="minimal"),
      error=function(e) NULL
    )
    if (is.null(df) || nrow(df) < 1) next

    # Pula primeira linha se for linha de unidades ex: "(H2O)", "(dag/kg)"
    if (nrow(df) >= 1) {
      primeira <- trimws(as.character(unlist(df[1,])))
      n_paren  <- sum(grepl("^\\(", primeira), na.rm=TRUE)
      if (n_paren >= 2) df <- df[-1, , drop=FALSE]
    }

    # Remove linhas onde quase tudo é NA (linhas decorativas)
    df <- df[apply(df, 1, function(r) sum(!is.na(r) & trimws(as.character(r)) != "") >= 2), , drop=FALSE]

    if (nrow(df) >= 1)
      return(list(erro=NULL, df=as.data.frame(df, stringsAsFactors=FALSE)))
  }
  list(erro="Nenhuma aba com dados de fertilidade identificada no Excel.")
}

ler_csv_lote <- function(filepath) {
  df <- NULL
  for (enc in c("UTF-8","latin1","UTF-8-BOM")) {
    for (sep in c(";",",","\t")) {
      tryCatch({
        d <- utils::read.csv(filepath, sep=sep, header=TRUE,
                              stringsAsFactors=FALSE, fileEncoding=enc,
                              check.names=FALSE)
        if (ncol(d) >= 3 && nrow(d) >= 1) { df <- d; break }
      }, error=function(e) NULL)
      if (!is.null(df)) break
    }
    if (!is.null(df)) break
  }
  if (is.null(df))
    return(list(erro="Não foi possível ler o CSV. Verifique o separador (;, vírgula ou tab) e o encoding."))
  list(erro=NULL, df=df)
}

# ==============================================================================
# GERAR TEMPLATE EXCEL (copia do arquivo estático)
# ==============================================================================
caminho_template_lote <- function() {
  # Procura primeiro na pasta do app, depois em inst/
  candidatos <- c(
    file.path(getwd(), "template_analises_lote.xlsx"),
    system.file("app/template_analises_lote.xlsx", package="GEMSFertilizer")
  )
  for (p in candidatos) if (file.exists(p)) return(p)
  NULL
}
