# ==============================================================================
# PARSER DE LAUDOS DE ANÁLISE DE SOLO
# Testado com laudos reais: ITPS (Sergipe) e Labominas (MG)
# ==============================================================================

# Normaliza string para ASCII puro — remove acentos de forma robusta
# Estratégia dupla: tenta iconv TRANSLIT; se gerar '?', usa remoção direta de diacríticos
norm_ascii <- function(x) {
  if (is.na(x) || nchar(x) == 0) return("")
  # Tenta TRANSLIT
  y <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "?")
  # Se gerou muitos '?' (iconv não conseguiu translit), usa substituição manual
  n_interroga <- nchar(gsub("[^?]", "", y))
  if (n_interroga > 2) {
    # Substituição manual dos diacríticos mais comuns em português
    y <- x
    y <- chartr("áàãâäÁÀÃÂÄ", "aaaaaaaaaaa", y)  # note: same length!
    y <- chartr("\u00e9\u00e8\u00ea\u00ebÉÈÊË", "eeeeEEEE", y)
    y <- chartr("\u00ed\u00ec\u00ee\u00efÍÌÎÏ", "iiiiIIII", y)
    y <- chartr("\u00f3\u00f2\u00f5\u00f4\u00f6ÓÒÕÔÖ", "oooooOOOOO", y)
    y <- chartr("\u00fa\u00f9\u00fb\u00fcÚÙÛÜ", "uuuuUUUU", y)
    y <- chartr("\u00e7Ç", "cC", y)
    y <- chartr("\u00f1Ñ", "nN", y)
    y <- iconv(y, to = "ASCII", sub = "")  # remove qualquer restante
  } else {
    y <- gsub("\\?", "", y)  # remove '?' residuais
  }
  # en-dash / em-dash → hífen
  y <- gsub("\u2013|\u2014|-", "-", y)
  tolower(trimws(y))
}

# --------------------------------------------------------------------------
# Padrões em ASCII (sem acentos) — usados em todas as comparações
# --------------------------------------------------------------------------
LAB_PARAMS_ASCII <- list(
  ph     = c("ph em agua", "ph em h2o", "ph h2o", "ph unid", "ph (agua"),
  mo     = c("materia organica", "mat.org.", "mat.org(", "m.o.", "carbono organico"),
  p      = c("fosforo (mehlich", "fosforo mehlich", "fosforo (m -", "fosforo (m-"),
  k      = c("potassio (mehlich", "potassio mehlich", "potassio (m -", "potassio (m-"),
  ca     = c("calcio (k", "calcio (kcl", "calcio(k", "ca (kcl"),
  mg     = c("magnesio (k", "magnesio (kcl", "magnesio(k", "mg (kcl"),
  al     = c("aluminio (k", "aluminio (kcl", "aluminio(k", "al (kcl"),
  h_al   = c("h + al", "h+al", "hidrogenio + aluminio", "acidez potencial",
              "h + al (acetato", "h+al (acetato"),
  s_solo = c("enxofre (fosfato", "enxofre monocal", "enxofre ("),
  b      = c("boro (agua quente", "boro(agua", "boro ("),
  cu     = c("cobre (mehlich", "cu (mehlich"),
  fe     = c("ferro (mehlich", "fe (mehlich"),
  mn     = c("manganes (mehlich", "mn (mehlich"),
  zn     = c("zinco (mehlich", "zn (mehlich")
)

# Padrões ITPS — nomes mais curtos sem "(Mehlich...)"
LAB_PARAMS_ITPS_ASCII <- list(
  ph     = c("ph em agua", "ph em h2o"),
  mo     = c("materia organica", "mat.org"),
  p      = c("fosforo"),          # linha começa com "fosforo"
  ca     = c("calcio"),
  mg     = c("magnesio"),
  al     = c("aluminio"),
  h_al   = c("hidrogenio + aluminio", "h + al"),
  s_solo = c("enxofre"),
  b      = c("boro"),
  cu     = c("cobre"),
  fe     = c("ferro"),
  mn     = c("manganes"),
  zn     = c("zinco")
)

FAIXAS <- list(
  ph = c(3.0, 9.5), mo = c(0.0, 80.0), p = c(0.0, 500.0),
  k  = c(0.0, 1500.0), ca = c(0.0, 30.0), mg = c(0.0, 15.0),
  al = c(0.0, 10.0), h_al = c(0.0, 30.0),
  s_solo = c(0.0, 200.0), b = c(0.0, 20.0),
  cu = c(0.0, 100.0), fe = c(0.0, 2000.0),
  mn = c(0.0, 500.0), zn = c(0.0, 100.0)
)

# --------------------------------------------------------------------------
# FUNÇÃO PRINCIPAL
# --------------------------------------------------------------------------
parsear_laudo <- function(filepath, laboratorio = "auto", id_amostra = NULL) {
  ext <- tolower(tools::file_ext(filepath))
  resultado <- if (ext == "pdf") {
    parsear_pdf(filepath)
  } else if (ext %in% c("xlsx","xls")) {
    parsear_excel(filepath)
  } else if (ext %in% c("csv","txt")) {
    parsear_csv_file(filepath)
  } else {
    list(erro = paste0("Formato '.",ext,"' não suportado. Use PDF, XLSX ou CSV."))
  }
  if (!is.null(resultado$erro)) return(resultado)
  if (!is.null(id_amostra)) {
    for (i in seq_along(resultado$amostras)) {
      if (is.null(resultado$amostras[[i]]$id_amostra))
        resultado$amostras[[i]]$id_amostra <- paste0(
          id_amostra, if(length(resultado$amostras)>1) paste0("_",i) else "")
    }
  }
  resultado
}

# --------------------------------------------------------------------------
# PARSER PDF
# --------------------------------------------------------------------------
parsear_pdf <- function(filepath) {
  if (!requireNamespace("pdftools", quietly=TRUE))
    return(list(
      erro = paste0("Pacote 'pdftools' necessário. ",
                    "Instale com: install.packages('pdftools'). ",
                    "Alternativa: exporte o laudo como CSV/Excel.")
    ))
  paginas <- tryCatch(pdftools::pdf_text(filepath), error=function(e) NULL)
  if (is.null(paginas) || length(paginas) == 0)
    return(list(erro = "Não foi possível extrair texto do PDF. O arquivo pode ser escaneado. Use CSV/Excel."))
  texto <- paste(paginas, collapse="\n")
  lab   <- detectar_lab(texto)
  if      (lab == "ITPS")      parsear_itps(texto)
  else if (lab == "Labominas") parsear_labominas(texto)
  else                          parsear_generico(texto)
}

# --------------------------------------------------------------------------
# ITPS — layout: "Nome  Valor  Unidade  LQ  Metodo  Data"
# Tudo normalizado para ASCII antes de qualquer grepl
# --------------------------------------------------------------------------
parsear_itps <- function(texto) {
  linhas  <- strsplit(texto, "\n")[[1]]
  linhasN <- sapply(linhas, norm_ascii, USE.NAMES=FALSE)  # versão ASCII de cada linha
  vals    <- list()
  k_vals  <- numeric(0)

  # ID da amostra
  id_am <- NA_character_
  m_id  <- regmatches(texto, regexpr("ITPS N[\\xb0\\xba\\u00b0\\u00ba]\\s*([0-9/]+)", texto, perl=TRUE))
  if (length(m_id) > 0) id_am <- gsub("ITPS N.\\s*","",m_id)

  pegar_primeiro_num <- function(ln) {
    ln2 <- gsub("--", " ", ln)
    ln2 <- gsub("[0-9]{2}/[0-9]{2}/[0-9]{2,4}", " ", ln2)
    ln2 <- gsub(",", ".", ln2)
    m   <- regmatches(ln2, gregexpr("[0-9]+\\.?[0-9]*", ln2))[[1]]
    v   <- suppressWarnings(as.numeric(m))
    v   <- v[!is.na(v) & v > 0]
    if (length(v) == 0) return(NULL)
    v[1]
  }

  for (i in seq_along(linhas)) {
    lnN <- linhasN[i]   # linha normalizada ASCII
    ln  <- linhas[i]    # linha original (para extrair números)
    if (nchar(lnN) < 3) next

    # pH
    if (is.null(vals$ph) && startsWith(lnN, "ph em ")) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v >= 3 && v <= 10) { vals$ph <- v; next }
    }
    # MO — ITPS reporta g/dm3; converter div10 para dag/kg
    if (is.null(vals$mo) && startsWith(lnN, "materia org")) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v > 0) { vals$mo <- round(v / 10, 2); next }
    }
    # Ca — comeca com "calcio" mas nao e Ca+Mg nem saturacao
    if (is.null(vals$ca) && startsWith(lnN, "calcio") &&
        !grepl("efet", lnN, fixed=TRUE) && !grepl("soma", lnN, fixed=TRUE) &&
        !grepl("ctc", lnN, fixed=TRUE) && !grepl("satura", lnN, fixed=TRUE) &&
        !grepl("+", lnN, fixed=TRUE)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v >= 0 && v <= 30) { vals$ca <- v; next }
    }
    # Mg
    if (is.null(vals$mg) && startsWith(lnN, "magnesio") &&
        !grepl("soma", lnN, fixed=TRUE) && !grepl("ctc", lnN, fixed=TRUE) &&
        !grepl("satura", lnN, fixed=TRUE)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v >= 0 && v <= 15) { vals$mg <- v; next }
    }
    # Al
    if (is.null(vals$al) && startsWith(lnN, "aluminio") &&
        !grepl("soma", lnN, fixed=TRUE) && !grepl("ctc", lnN, fixed=TRUE) &&
        !grepl("satura", lnN, fixed=TRUE)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v >= 0 && v <= 10) { vals$al <- v; next }
    }
    # H+Al
    if (is.null(vals$h_al) &&
        (grepl("hidrogenio", lnN, fixed=TRUE) || startsWith(lnN, "h + al"))) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v >= 0 && v <= 30) { vals$h_al <- v; next }
    }
    # K — acumula todos; usa o maior (mg/dm3 >> cmolc/dm3)
    if (startsWith(lnN, "potassio") &&
        !grepl("soma", lnN, fixed=TRUE) && !grepl("ctc", lnN, fixed=TRUE) &&
        !grepl("satura", lnN, fixed=TRUE)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v > 0) k_vals <- c(k_vals, v)
      next
    }
    # P — linha comeca com "fosforo" E contem "mg" (exclui linha de adubacao)
    if (is.null(vals$p) &&
        startsWith(lnN, "fosforo") &&
        grepl("mg", lnN, fixed=TRUE) &&
        !grepl("p2o5", lnN, fixed=TRUE) &&
        !grepl("adubacao", lnN, fixed=TRUE)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v >= 0 && v <= 500) { vals$p <- v; next }
    }
    # Micronutrientes
    for (par in c("s_solo","b","cu","fe","mn","zn")) {
      if (!is.null(vals[[par]])) next
      pref <- list(s_solo="enxofre", b="boro", cu="cobre",
                   fe="ferro", mn="manganes", zn="zinco")[[par]]
      f <- FAIXAS[[par]]
      if (startsWith(lnN, pref)) {
        v <- pegar_primeiro_num(ln)
        if (!is.null(v) && v >= f[1] && v <= f[2]) vals[[par]] <- v
      }
    }
  }

  if (length(k_vals) > 0) vals$k <- max(k_vals)

  n_ok <- sum(sapply(c("ph","p","k","ca","mg","al","h_al"), function(v) !is.null(vals[[v]])))
  if (n_ok < 4)
    return(list(erro = paste0(
      "ITPS: apenas ", n_ok, " parâmetros extraídos. ",
      "Encontrados: ", paste(names(vals), collapse=", "),
      ". Verifique se o PDF tem texto selecionável."
    )))

  vals$id_amostra  <- id_am
  vals$laboratorio <- "ITPS"
  list(erro=NULL, amostras=list(vals), laboratorio="ITPS")
}

# --------------------------------------------------------------------------
# LABOMINAS — layout tabular, 3 amostras em colunas
# --------------------------------------------------------------------------
parsear_labominas <- function(texto) {
  linhas  <- strsplit(texto, "\n")[[1]]
  linhasN <- sapply(linhas, norm_ascii, USE.NAMES=FALSE)

  # IDs: linhas com NNNN/AAAA que têm texto descritivo depois
  # e NÃO são linhas de cabeçalho (relatório, laudo, etc.)
  ids <- character(0)
  for (i in seq_along(linhas)) {
    lnN <- linhasN[i]
    if (grepl("relatorio|laudo|cnpj|responsavel|data de entrada|emitido", lnN)) next
    m <- regmatches(linhas[i], gregexpr("[0-9]{4,}/[0-9]{4}", linhas[i]))[[1]]
    if (length(m) == 0) next
    texto_apos <- trimws(gsub(m[1], "", linhas[i], fixed=TRUE))
    if (nchar(texto_apos) > 3) ids <- c(ids, m[1])
  }
  ids <- unique(ids)
  n_am <- max(length(ids), 1)

  amostras <- replicate(n_am, list(), simplify=FALSE)
  for (i in seq_along(ids)) amostras[[i]]$id_amostra <- ids[i]

  # Identifica parâmetro usando ASCII normalizado
  id_param_labo <- function(lnN) {
    for (par in names(LAB_PARAMS_ASCII)) {
      for (pat in LAB_PARAMS_ASCII[[par]]) {
        if (grepl(pat, lnN, fixed=TRUE)) return(par)
      }
    }
    # K especial — nome começa com "potassio" sem "(mehlich" antes
    if (grepl("^potassio\\b", lnN, perl=TRUE)) return("k")
    NULL
  }

  for (i in seq_along(linhas)) {
    lnN <- linhasN[i]
    if (nchar(lnN) < 4) next

    param <- id_param_labo(lnN)
    if (is.null(param)) next

    # Extrai números da linha original, filtra anos e página
    ln2  <- gsub(",", ".", linhas[i])
    m    <- regmatches(ln2, gregexpr("[0-9]+\\.?[0-9]*", ln2))[[1]]
    nums <- suppressWarnings(as.numeric(m))
    nums <- nums[!is.na(nums) & nums >= 0 & nums < 5000 &
                   !(nums >= 2020 & nums <= 2030)]

    if (length(nums) < n_am) next
    vals_linha <- tail(nums, n_am)

    for (j in seq_len(n_am)) {
      val <- vals_linha[j]
      if (!is.na(val) && is.null(amostras[[j]][[param]])) {
        f  <- FAIXAS[[param]]
        ok <- is.null(f) || (val >= f[1] && val <= f[2])
        if (ok) amostras[[j]][[param]] <- val
      }
    }
  }

  amostras_v <- Filter(function(am) {
    sum(sapply(c("ph","p","k","ca","mg"), function(v) !is.null(am[[v]]))) >= 3
  }, amostras)

  if (length(amostras_v) == 0)
    return(list(erro = paste0(
      "Labominas: poucos parâmetros extraídos. IDs: ",
      paste(ids, collapse=", "), ". Tente exportar como CSV/Excel."
    )))

  list(erro=NULL, amostras=amostras_v, laboratorio="Labominas")
}

# --------------------------------------------------------------------------
# PARSER GENÉRICO
# --------------------------------------------------------------------------
parsear_generico <- function(texto) {
  linhas  <- strsplit(texto, "\n")[[1]]
  linhasN <- sapply(linhas, norm_ascii, USE.NAMES=FALSE)
  vals    <- list()
  k_vals  <- numeric(0)

  pegar_primeiro_num <- function(ln) {
    ln2 <- gsub("--"," ", ln); ln2 <- gsub("[0-9]{2}/[0-9]{2}/[0-9]{2,4}"," ",ln2)
    ln2 <- gsub(",",".",ln2)
    m   <- regmatches(ln2, gregexpr("[0-9]+\\.?[0-9]*", ln2))[[1]]
    v   <- suppressWarnings(as.numeric(m)); v <- v[!is.na(v) & v > 0]
    if (length(v)==0) return(NULL); v[1]
  }

  for (i in seq_along(linhas)) {
    lnN <- linhasN[i]; ln <- linhas[i]
    if (nchar(lnN) < 3) next
    for (par in names(LAB_PARAMS_ASCII)) {
      if (!is.null(vals[[par]])) next
      for (pat in LAB_PARAMS_ASCII[[par]]) {
        if (grepl(pat, lnN, fixed=TRUE)) {
          v <- pegar_primeiro_num(ln); f <- FAIXAS[[par]]
          if (!is.null(v) && (is.null(f)||(v>=f[1]&&v<=f[2]))) {
            vals[[par]] <- v; break
          }
        }
      }
    }
    if (startsWith(lnN, "potassio") && !grepl("soma", lnN, fixed=TRUE) && !grepl("ctc", lnN, fixed=TRUE)) {
      v <- pegar_primeiro_num(ln); if (!is.null(v) && v>0) k_vals <- c(k_vals, v)
    }
  }
  if (length(k_vals)>0) vals$k <- max(k_vals)
  if (length(vals)<3)
    return(list(erro=paste0("Apenas ",length(vals)," parâmetros. Formato desconhecido — use CSV/Excel.")))
  list(erro=NULL, amostras=list(vals), laboratorio="Generico")
}

# --------------------------------------------------------------------------
# EXCEL / CSV
# --------------------------------------------------------------------------
parsear_excel <- function(filepath) {
  if (!requireNamespace("readxl", quietly=TRUE))
    return(list(erro = "Pacote 'readxl' necessário."))
  abas <- tryCatch(readxl::excel_sheets(filepath), error=function(e) NULL)
  if (is.null(abas)) return(list(erro = "Não foi possível abrir o arquivo Excel."))
  for (aba in abas) {
    df  <- tryCatch(readxl::read_excel(filepath,sheet=aba,col_names=FALSE), error=function(e) NULL)
    if (is.null(df)||nrow(df)<2) next
    res <- parsear_df(df)
    if (is.null(res$erro)) return(res)
  }
  list(erro = "Nenhuma aba com dados de fertilidade identificada.")
}

parsear_csv_file <- function(filepath) {
  df <- NULL
  for (sep in c(";",",","\t")) {
    tryCatch({
      d <- utils::read.csv(filepath,sep=sep,header=FALSE,stringsAsFactors=FALSE,fileEncoding="UTF-8")
      if (ncol(d)>=2) { df<-d; break }
    }, error=function(e) NULL)
  }
  if (is.null(df)) tryCatch({
    df <- utils::read.csv(filepath,sep=";",header=FALSE,stringsAsFactors=FALSE,fileEncoding="latin1")
  }, error=function(e) NULL)
  if (is.null(df)) return(list(erro="Não foi possível ler o CSV."))
  parsear_df(df)
}

parsear_df <- function(df) {
  primcol <- sapply(as.character(df[,1]), norm_ascii, USE.NAMES=FALSE)
  n_match <- sum(sapply(primcol, function(x) {
    for (par in names(LAB_PARAMS_ASCII))
      for (pat in LAB_PARAMS_ASCII[[par]])
        if (grepl(pat, x, fixed=TRUE)) return(TRUE)
    FALSE
  }))

  if (n_match >= 3) {
    n_am <- ncol(df) - 1
    amostras <- replicate(n_am, list(), simplify=FALSE)
    for (i in seq_len(nrow(df))) {
      lnN   <- primcol[i]
      param <- NULL
      for (par in names(LAB_PARAMS_ASCII))
        for (pat in LAB_PARAMS_ASCII[[par]])
          if (grepl(pat, lnN, fixed=TRUE)) { param<-par; break }
      if (is.null(param)) next
      for (j in seq_len(n_am)) {
        v  <- suppressWarnings(as.numeric(gsub(",",".",as.character(df[i,j+1]))))
        f  <- FAIXAS[[param]]
        if (!is.na(v) && is.null(amostras[[j]][[param]]))
          if (is.null(f)||(v>=f[1]&&v<=f[2])) amostras[[j]][[param]] <- v
      }
    }
    am_v <- Filter(function(am) length(am)>=3, amostras)
    if (length(am_v)>0) return(list(erro=NULL,amostras=am_v,laboratorio="Excel/CSV"))
  }

  list(erro="Não foi possível identificar o layout do arquivo.")
}

# --------------------------------------------------------------------------
# CONVERTER AMOSTRAS → DATA.FRAME (colunas iguais para rbind seguro)
# --------------------------------------------------------------------------
amostras_para_dataframe <- function(resultado_parse) {
  if (!is.null(resultado_parse$erro)) return(NULL)
  amostras <- resultado_parse$amostras
  if (length(amostras)==0) return(NULL)
  params  <- c("id_amostra","ph","mo","p","k","ca","mg","al","h_al",
                "s_solo","b","cu","fe","mn","zn")
  linhas  <- lapply(seq_along(amostras), function(i) {
    am  <- amostras[[i]]
    row <- setNames(lapply(params, function(p) { v <- am[[p]]; if(is.null(v)||is.na(v)) NA else v }), params)
    row <- as.data.frame(row, stringsAsFactors=FALSE)
    if (is.na(row$id_amostra)) row$id_amostra <- paste0("AM-",sprintf("%03d",i))
    row
  })
  todos   <- unique(unlist(lapply(linhas,names)))
  padded  <- lapply(linhas, function(r) { for(nm in setdiff(todos,names(r))) r[[nm]]<-NA; r[,todos,drop=FALSE] })
  do.call(rbind, padded)
}

# --------------------------------------------------------------------------
# AUXILIARES
# --------------------------------------------------------------------------
detectar_lab <- function(texto) {
  txt <- norm_ascii(texto)
  if (grepl("instituto tecnologico.*sergipe|itps.*sergipe|itps n", txt)) return("ITPS")
  if (grepl("labominas|lomeu|sistema ceres", txt))  return("Labominas")
  if (grepl("labossolo", txt)) return("Labossolo")
  "Generico"
}

`%||%` <- function(a, b) if (!is.null(a) && length(a)>0 && !is.na(a[1])) a else b

# --------------------------------------------------------------------------
# Mapa: palavras-chave de cada parâmetro (strings fixas, sem regex)
# --------------------------------------------------------------------------
LAB_PARAMS_FIXO <- list(
  ph     = c("ph em água","ph em agua","ph (água","ph (agua",
              "ph h2o","ph unid"),
  mo     = c("matéria orgânica","materia organica","mat.org.",
              "m.o.","carbono organico","carbono orgânico"),
  p      = c("fósforo (mehlich","fosforo (mehlich",
              "fósforo mehlich","fosforo mehlich"),
  k      = c("potássio (mehlich","potassio (mehlich",
              "potássio mehlich","potassio mehlich"),
  ca     = c("cálcio (k","calcio (k","cálcio (kcl","calcio (kcl"),
  mg     = c("magnésio (k","magnesio (k","magnésio (kcl","magnesio (kcl"),
  al     = c("alumínio (k","aluminio (k","alumínio (kcl","aluminio (kcl"),
  h_al   = c("h + al","h+al","hidrogênio + alumínio","hidrogenio + aluminio",
              "h + al (acetato","acidez potencial"),
  s_solo = c("enxofre (fosfato","enxofre (","s-so4"),
  b      = c("boro (água quente","boro (agua quente","boro ("),
  cu     = c("cobre (mehlich","cu (mehlich"),
  fe     = c("ferro (mehlich","fe (mehlich"),
  mn     = c("manganês (mehlich","manganes (mehlich","mn (mehlich"),
  zn     = c("zinco (mehlich","zn (mehlich")
)

# Para ITPS: busca sem "(Mehlich..." pois ele escreve só o nome
LAB_PARAMS_ITPS <- list(
  ph     = c("ph em água","ph em agua"),
  mo     = c("matéria orgânica","materia organica"),
  p      = c("fósforo","fosforo"),
  ca     = c("^cálcio$","^calcio$","^cálcio ","^calcio "),
  mg     = c("^magnésio$","^magnesio$","^magnésio ","^magnesio "),
  al     = c("^alumínio$","^aluminio$","^alumínio ","^aluminio "),
  h_al   = c("hidrogênio + alumínio","hidrogenio + aluminio","h + al"),
  s_solo = c("enxofre"),
  b      = c("boro"),
  cu     = c("cobre"),
  fe     = c("ferro"),
  mn     = c("manganês","manganes"),
  zn     = c("zinco")
)

FAIXAS <- list(
  ph = c(3.0, 9.5), mo = c(0.0, 80.0), p = c(0.0, 500.0),
  k  = c(0.0, 1500.0), ca = c(0.0, 30.0), mg = c(0.0, 15.0),
  al = c(0.0, 10.0), h_al = c(0.0, 30.0),
  s_solo = c(0.0, 200.0), b = c(0.0, 20.0),
  cu = c(0.0, 100.0), fe = c(0.0, 2000.0),
  mn = c(0.0, 500.0), zn = c(0.0, 100.0)
)

# --------------------------------------------------------------------------
# FUNÇÃO PRINCIPAL
# --------------------------------------------------------------------------
parsear_laudo <- function(filepath, laboratorio = "auto", id_amostra = NULL) {
  ext <- tolower(tools::file_ext(filepath))
  resultado <- if (ext == "pdf") {
    parsear_pdf(filepath)
  } else if (ext %in% c("xlsx","xls")) {
    parsear_excel(filepath)
  } else if (ext %in% c("csv","txt")) {
    parsear_csv_file(filepath)
  } else {
    list(erro = paste0("Formato '.",ext,"' não suportado. Use PDF, XLSX ou CSV."))
  }
  if (!is.null(resultado$erro)) return(resultado)
  # Atribui id_amostra se ausente
  if (!is.null(id_amostra)) {
    for (i in seq_along(resultado$amostras)) {
      if (is.null(resultado$amostras[[i]]$id_amostra))
        resultado$amostras[[i]]$id_amostra <- paste0(
          id_amostra, if(length(resultado$amostras)>1) paste0("_",i) else "")
    }
  }
  resultado
}

# --------------------------------------------------------------------------
# PARSER PDF — detecta lab e despacha
# --------------------------------------------------------------------------
parsear_pdf <- function(filepath) {
  if (!requireNamespace("pdftools", quietly=TRUE))
    return(list(
      erro = paste0("Pacote 'pdftools' necessário. ",
                    "Instale com: install.packages('pdftools'). ",
                    "Alternativa: exporte o laudo como CSV/Excel.")
    ))
  paginas <- tryCatch(pdftools::pdf_text(filepath), error=function(e) NULL)
  if (is.null(paginas) || length(paginas) == 0)
    return(list(erro = "Não foi possível extrair texto do PDF. O arquivo pode ser escaneado. Use CSV/Excel."))
  texto <- paste(paginas, collapse="\n")
  lab   <- detectar_lab(texto)
  if      (lab == "ITPS")     parsear_itps(texto)
  else if (lab == "Labominas") parsear_labominas(texto)
  else                         parsear_generico(texto)
}

# --------------------------------------------------------------------------
# PARSER ITPS — layout: "Nome  Valor  Unidade  LQ  Metodo  Data"
# Ex: "pH em Água 4,85 -- -- H2O 29/02/24"
#     "Potássio 0,53 cmolc/dm3 -- ... 04/03/24"   ← cmolc, ignorar
#     "Potássio 208 mg/dm3 1,40 ...  29/02/24"    ← mg/dm³, usar este
# --------------------------------------------------------------------------
parsear_itps <- function(texto) {
  linhas <- strsplit(texto, "\n")[[1]]
  vals   <- list()

  # Captura ID da amostra
  m_id <- regmatches(texto, regexpr("ITPS N[°º]\\s*([0-9/]+)", texto))
  id_am <- if(length(m_id)>0) gsub("ITPS N[°º]\\s*","",m_id) else NA_character_
  m_am  <- regmatches(texto, regexpr("Amostra\\s+[0-9]+\\s*[-–]\\s*([^\n]+)\n", texto))
  if (length(m_am)>0)
    id_am <- trimws(gsub("^Amostra\\s+[0-9]+\\s*[-–]\\s*","", gsub("\n","",m_am)))

  # Função: extrai primeiro número de uma linha (ignora "--" e datas dd/mm/aa)
  pegar_primeiro_num <- function(ln) {
    # Remove "--", datas (29/02/24) e texto puro
    ln2 <- gsub("--", " ", ln)
    ln2 <- gsub("\\b[0-9]{2}/[0-9]{2}/[0-9]{2,4}\\b", " ", ln2)
    ln2 <- gsub(",", ".", ln2)
    m   <- regmatches(ln2, gregexpr("\\b[0-9]+\\.?[0-9]*\\b", ln2))[[1]]
    v   <- suppressWarnings(as.numeric(m))
    v   <- v[!is.na(v) & v > 0]
    if (length(v) == 0) return(NULL)
    v[1]
  }

  k_vals <- numeric(0)  # acumula TODOS os K encontrados; no final pega o maior

  for (ln in linhas) {
    lnl <- tolower(trimws(ln))
    if (nchar(lnl) < 3) next

    # pH
    if (is.null(vals$ph) && grepl("^ph em ", lnl)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v >= 3 && v <= 10) { vals$ph <- v; next }
    }
    # MO — ITPS reporta em g/dm³; converter ÷10 para dag/kg
    if (is.null(vals$mo) && grepl("^mat.ria org", lnl)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v > 0 && v <= 800) { vals$mo <- round(v/10, 2); next }
    }
    # Ca — linha começa com "Cálcio" mas NÃO "Cálcio +" nem "CTC" nem "Efetiva"
    if (is.null(vals$ca) && grepl("^c.lcio\\b", lnl) &&
        !grepl("\\+|efet|soma|ctc|satura", lnl)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v >= 0 && v <= 30) { vals$ca <- v; next }
    }
    # Mg
    if (is.null(vals$mg) && grepl("^magn.sio\\b", lnl) &&
        !grepl("soma|\\+|ctc|satura", lnl)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v >= 0 && v <= 15) { vals$mg <- v; next }
    }
    # Al — apenas "Alumínio" simples, não saturação
    if (is.null(vals$al) && grepl("^alum.nio\\b", lnl) &&
        !grepl("soma|\\+|ctc|satura", lnl)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v >= 0 && v <= 10) { vals$al <- v; next }
    }
    # H+Al
    if (is.null(vals$h_al) && grepl("hidrog.nio.*alum|^h \\+ al\\b", lnl)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v >= 0 && v <= 30) { vals$h_al <- v; next }
    }
    # K — acumula todos (cmolc e mg/dm³); pega o maior ao final
    if (grepl("^pot.ssio\\b", lnl) && !grepl("soma|\\+|ctc|satura", lnl)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v > 0) k_vals <- c(k_vals, v)
      next
    }
    # P — apenas quando a unidade for mg/dm³ (linha contém "mg")
    if (is.null(vals$p) && grepl("^f.sforo\\b", lnl) && grepl("mg", lnl)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v >= 0 && v <= 500) { vals$p <- v; next }
    }
    # Micronutrientes (opcionais)
    for (par in c("s_solo","b","cu","fe","mn","zn")) {
      if (!is.null(vals[[par]])) next
      termos <- list(s_solo=c("^enxofre"),b=c("^boro"),cu=c("^cobre"),
                     fe=c("^ferro"),mn=c("^mangan"),zn=c("^zinco"))[[par]]
      faixa  <- FAIXAS[[par]]
      if (any(grepl(termos, lnl))) {
        v <- pegar_primeiro_num(ln)
        if (!is.null(v) && v >= faixa[1] && v <= faixa[2]) vals[[par]] <- v
      }
    }
  }

  # K: o maior valor é sempre o de mg/dm³ (ex: 208 >> 0,53)
  if (length(k_vals) > 0) vals$k <- max(k_vals)

  # Verifica mínimo de parâmetros
  obrig  <- c("ph","p","k","ca","mg","al","h_al")
  n_ok   <- sum(sapply(obrig, function(v) !is.null(vals[[v]])))
  if (n_ok < 4)
    return(list(erro = paste0(
      "ITPS: apenas ", n_ok, " parâmetros extraídos (esperado ≥4). ",
      "Parâmetros encontrados: ", paste(names(vals), collapse=", "),
      ". Verifique se o PDF tem texto selecionável."
    )))

  vals$id_amostra  <- id_am
  vals$laboratorio <- "ITPS"
  list(erro=NULL, amostras=list(vals), laboratorio="ITPS")
}

# --------------------------------------------------------------------------
# PARSER LABOMINAS — layout tabular
# Linha 16 (índice): "8964 8965 8966"  (IDs SEM /ano)
# Linhas seguintes: "NomeParam Método Unidade val1 val2 val3"
# --------------------------------------------------------------------------
parsear_labominas <- function(texto) {
  linhas <- strsplit(texto, "\n")[[1]]

  # IDs de amostras: linhas onde NNNN/AAAA vem SEGUIDO de texto descritivo
  # Ex: "8964/2025 AMOSTRA - EXP. - UFS - AREA 01"
  # Exclui: "RELATÓRIO ANALÍTICO DE SOLO N° 5732/2025" (número do relatório)
  ids <- character(0)
  for (ln in linhas) {
    m <- regmatches(ln, gregexpr("[0-9]{4,}/[0-9]{4}", ln))[[1]]
    if (length(m) == 0) next
    # Só é ID de amostra se a linha contém texto após o número (amostra, área, etc.)
    # e NÃO contém palavras de cabeçalho (relatório, laudo, nº, n°)
    lnl <- tolower(ln)
    if (grepl("relat[oó]rio|laudo|n[°o]\\s*[0-9]|cnpj|responsavel|data de entrada", lnl)) next
    texto_apos <- trimws(gsub(m[1], "", ln, fixed=TRUE))
    if (nchar(texto_apos) > 3) ids <- c(ids, m[1])
  }
  ids <- unique(ids)

  # Fallback: linha com 2+ números de 4 dígitos separados por espaço
  if (length(ids) == 0) {
    for (ln in linhas[seq_len(min(30, length(linhas)))]) {
      m <- regmatches(ln, gregexpr("\\b[0-9]{4,}\\b", ln))[[1]]
      m <- m[!m %in% c("2023","2024","2025","2026")]
      if (length(m) >= 2) { ids <- m; break }
    }
  }

  n_am <- max(length(ids), 1)
  amostras <- replicate(n_am, list(), simplify=FALSE)
  for (i in seq_along(ids)) amostras[[i]]$id_amostra <- ids[i]

  # Função para identificar parâmetro
  id_param_labo <- function(txt) {
    for (par in names(LAB_PARAMS_FIXO)) {
      for (pat in LAB_PARAMS_FIXO[[par]]) {
        if (grepl(pat, txt, fixed=TRUE)) return(par)
      }
    }
    # K não está em LAB_PARAMS_FIXO com nome só
    if (grepl("^potássio", txt) || grepl("^potassio", txt)) return("k")
    NULL
  }

  for (ln in linhas) {
    lnl <- tolower(trimws(ln))
    if (nchar(lnl) < 4) next

    param <- id_param_labo(lnl)
    if (is.null(param)) next

    # Extrai números da linha, ignora anos
    ln2  <- gsub(",", ".", ln)
    m    <- regmatches(ln2, gregexpr("[0-9]+\\.?[0-9]*", ln2))[[1]]
    nums <- suppressWarnings(as.numeric(m))
    nums <- nums[!is.na(nums) & nums >= 0 & nums < 5000 &
                   !(nums >= 2020 & nums <= 2030)]

    if (length(nums) < n_am) next

    vals_linha <- tail(nums, n_am)

    for (j in seq_len(n_am)) {
      val <- vals_linha[j]
      if (!is.na(val) && is.null(amostras[[j]][[param]])) {
        faixa <- FAIXAS[[param]]
        ok <- if (!is.null(faixa)) val >= faixa[1] && val <= faixa[2] else TRUE
        if (ok) amostras[[j]][[param]] <- val
      }
    }
  }

  # Verifica mínimo
  amostras_v <- Filter(function(am) {
    sum(sapply(c("ph","p","k","ca","mg"), function(v) !is.null(am[[v]]))) >= 3
  }, amostras)

  if (length(amostras_v) == 0)
    return(list(erro = paste0(
      "Labominas: poucos parâmetros extraídos. IDs: ",
      paste(ids, collapse=", "), ". Tente exportar como CSV/Excel."
    )))

  list(erro=NULL, amostras=amostras_v, laboratorio="Labominas")
}

# --------------------------------------------------------------------------
# PARSER GENÉRICO — fallback para outros labs
# --------------------------------------------------------------------------
parsear_generico <- function(texto) {
  linhas <- strsplit(texto, "\n")[[1]]
  vals   <- list()
  k_vals <- numeric(0)

  pegar_primeiro_num <- function(ln) {
    ln2 <- gsub("--"," ", ln)
    ln2 <- gsub("\\b[0-9]{2}/[0-9]{2}/[0-9]{2,4}\\b"," ", ln2)
    ln2 <- gsub(",",".", ln2)
    m   <- regmatches(ln2, gregexpr("\\b[0-9]+\\.?[0-9]*\\b", ln2))[[1]]
    v   <- suppressWarnings(as.numeric(m))
    v   <- v[!is.na(v) & v > 0]
    if (length(v)==0) return(NULL); v[1]
  }

  for (ln in linhas) {
    lnl <- tolower(trimws(ln))
    if (nchar(lnl) < 3) next
    for (par in names(LAB_PARAMS_FIXO)) {
      if (!is.null(vals[[par]])) next
      for (pat in LAB_PARAMS_FIXO[[par]]) {
        if (grepl(pat, lnl, fixed=TRUE)) {
          v <- pegar_primeiro_num(ln)
          f <- FAIXAS[[par]]
          if (!is.null(v) && (is.null(f) || (v>=f[1] && v<=f[2])))
            vals[[par]] <- v
        }
      }
    }
    if (grepl("^pot.ssio\\b", lnl) && !grepl("soma|ctc|satura", lnl)) {
      v <- pegar_primeiro_num(ln)
      if (!is.null(v) && v > 0) k_vals <- c(k_vals, v)
    }
  }
  if (length(k_vals) > 0) vals$k <- max(k_vals)

  if (length(vals) < 3)
    return(list(erro = paste0("Apenas ", length(vals), " parâmetros identificados. Formato desconhecido — use CSV/Excel.")))
  list(erro=NULL, amostras=list(vals), laboratorio="Generico")
}

# --------------------------------------------------------------------------
# EXCEL / CSV
# --------------------------------------------------------------------------
parsear_excel <- function(filepath) {
  if (!requireNamespace("readxl", quietly=TRUE))
    return(list(erro = "Pacote 'readxl' necessário."))
  abas <- tryCatch(readxl::excel_sheets(filepath), error=function(e) NULL)
  if (is.null(abas)) return(list(erro = "Não foi possível abrir o arquivo Excel."))
  for (aba in abas) {
    df <- tryCatch(readxl::read_excel(filepath,sheet=aba,col_names=FALSE), error=function(e) NULL)
    if (is.null(df) || nrow(df)<2) next
    res <- parsear_df(df)
    if (is.null(res$erro)) return(res)
  }
  list(erro = "Nenhuma aba com dados de fertilidade identificada.")
}

parsear_csv_file <- function(filepath) {
  df <- NULL
  for (sep in c(";",",","\t")) {
    tryCatch({
      d <- utils::read.csv(filepath,sep=sep,header=FALSE,stringsAsFactors=FALSE,fileEncoding="UTF-8")
      if (ncol(d)>=2) { df<-d; break }
    }, error=function(e) NULL)
  }
  if (is.null(df)) tryCatch({
    df <- utils::read.csv(filepath,sep=";",header=FALSE,stringsAsFactors=FALSE,fileEncoding="latin1")
  }, error=function(e) NULL)
  if (is.null(df)) return(list(erro="Não foi possível ler o CSV."))
  parsear_df(df)
}

parsear_df <- function(df) {
  # Detecta se parâmetros estão nas linhas (primeira coluna) ou no cabeçalho
  primcol <- tolower(as.character(df[,1]))
  n_match <- sum(sapply(primcol, function(x) {
    for (par in names(LAB_PARAMS_FIXO))
      for (pat in LAB_PARAMS_FIXO[[par]])
        if (grepl(pat, x, fixed=TRUE)) return(TRUE)
    FALSE
  }))

  if (n_match >= 3) {
    # Parâmetros nas linhas, amostras nas colunas
    n_am <- ncol(df) - 1
    amostras <- replicate(n_am, list(), simplify=FALSE)
    for (i in seq_len(nrow(df))) {
      lnl <- tolower(as.character(df[i,1]))
      param <- NULL
      for (par in names(LAB_PARAMS_FIXO))
        for (pat in LAB_PARAMS_FIXO[[par]])
          if (grepl(pat, lnl, fixed=TRUE)) { param <- par; break }
      if (is.null(param)) next
      for (j in seq_len(n_am)) {
        v_str <- gsub(",",".", as.character(df[i, j+1]))
        v <- suppressWarnings(as.numeric(v_str))
        f <- FAIXAS[[param]]
        if (!is.na(v) && is.null(amostras[[j]][[param]]))
          if (is.null(f) || (v>=f[1]&&v<=f[2])) amostras[[j]][[param]] <- v
      }
    }
    am_v <- Filter(function(am) length(am)>=3, amostras)
    if (length(am_v)>0) return(list(erro=NULL,amostras=am_v,laboratorio="Excel/CSV"))
  }

  # Cabeçalho na primeira linha
  for (cab_row in seq_len(min(5,nrow(df)))) {
    n_p <- sum(sapply(tolower(as.character(df[cab_row,])), function(x) {
      for (par in names(LAB_PARAMS_FIXO))
        for (pat in LAB_PARAMS_FIXO[[par]])
          if (grepl(pat, x, fixed=TRUE)) return(TRUE)
      FALSE
    }))
    if (n_p >= 3) {
      cab <- tolower(as.character(df[cab_row,]))
      amostras <- list()
      for (r in seq(cab_row+1, nrow(df))) {
        am <- list()
        for (c in seq_along(cab)) {
          param <- NULL
          for (par in names(LAB_PARAMS_FIXO))
            for (pat in LAB_PARAMS_FIXO[[par]])
              if (grepl(pat, cab[c], fixed=TRUE)) { param<-par; break }
          if (is.null(param)) next
          v_str <- gsub(",",".", as.character(df[r,c]))
          v <- suppressWarnings(as.numeric(v_str))
          f <- FAIXAS[[param]]
          if (!is.na(v) && (is.null(f)||(v>=f[1]&&v<=f[2]))) am[[param]] <- v
        }
        if (length(am)>=3) amostras <- c(amostras, list(am))
      }
      if (length(amostras)>0)
        return(list(erro=NULL, amostras=amostras, laboratorio="Excel/CSV"))
    }
  }
  list(erro="Não foi possível identificar o layout do arquivo.")
}

# --------------------------------------------------------------------------
# CONVERTER AMOSTRAS → DATA.FRAME
# --------------------------------------------------------------------------
amostras_para_dataframe <- function(resultado_parse) {
  if (!is.null(resultado_parse$erro)) return(NULL)
  amostras <- resultado_parse$amostras
  if (length(amostras)==0) return(NULL)
  params <- c("id_amostra","ph","mo","p","k","ca","mg","al","h_al",
               "s_solo","b","cu","fe","mn","zn")
  linhas <- lapply(seq_along(amostras), function(i) {
    am  <- amostras[[i]]
    row <- setNames(lapply(params, function(p) am[[p]] %||% NA), params)
    row <- as.data.frame(row, stringsAsFactors=FALSE)
    if (is.na(row$id_amostra)) row$id_amostra <- paste0("AM-",sprintf("%03d",i))
    row
  })
  # Garante colunas iguais antes do rbind
  todos_nomes <- unique(unlist(lapply(linhas,names)))
  linhas_pad  <- lapply(linhas, function(r) {
    faltam <- setdiff(todos_nomes, names(r))
    for (nm in faltam) r[[nm]] <- NA
    r[, todos_nomes, drop=FALSE]
  })
  do.call(rbind, linhas_pad)
}

# --------------------------------------------------------------------------
# AUXILIARES
# --------------------------------------------------------------------------
detectar_lab <- function(texto) {
  txt <- tolower(texto)
  if (grepl("instituto tecnol.gico.*sergipe|itps.*sergipe|itps n", txt)) return("ITPS")
  if (grepl("labominas|lomeu|sistema ceres", txt)) return("Labominas")
  if (grepl("labossolo", txt)) return("Labossolo")
  "Generico"
}

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b
