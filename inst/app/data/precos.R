# ==============================================================================
# MÓDULO DE PREÇOS — Opção D (CSV) + Scraping CEPEA (opcional)
# ==============================================================================

# Caminho do CSV de referência (relativo à raiz do app)
PRECOS_CSV <- "data/precos_referencia.csv"

# ------------------------------------------------------------------------------
# CARREGAR PREÇOS DO CSV (Opção D)
# Lê o arquivo local; se não existir, usa os preços embutidos como fallback
# ------------------------------------------------------------------------------
carregar_precos_csv <- function() {
  if (file.exists(PRECOS_CSV)) {
    df <- tryCatch(
      read.csv(PRECOS_CSV, stringsAsFactors = FALSE, encoding = "UTF-8"),
      error = function(e) NULL
    )
    if (!is.null(df) && nrow(df) > 0 && "preco_ref" %in% names(df)) {
      # Coerce para numeric imediatamente — CSV pode trazer character
      df$teor      <- suppressWarnings(as.numeric(gsub(",", ".", df$teor)))
      df$preco_ref <- suppressWarnings(as.numeric(gsub(",", ".", df$preco_ref)))
      # Adiciona colunas obrigatórias se ausentes
      if (!"fonte"    %in% names(df)) df$fonte    <- "CSV local"
      if (!"data_ref" %in% names(df)) df$data_ref <- format(Sys.Date(), "%Y-%m")
      if (!"unidade"  %in% names(df)) df$unidade  <- "R$/kg"
      return(df)
    }
  }
  # Fallback: retorna todas_fontes com colunas extra
  df <- todas_fontes
  df$unidade   <- "R$/kg"
  df$fonte     <- "Embutido (fallback)"
  df$data_ref  <- format(Sys.Date(), "%Y-%m")
  return(df)
}

# ------------------------------------------------------------------------------
# SALVAR PREÇOS NO CSV
# ------------------------------------------------------------------------------
salvar_precos_csv <- function(df) {
  tryCatch({
    write.csv(df, PRECOS_CSV, row.names = FALSE, fileEncoding = "UTF-8")
    TRUE
  }, error = function(e) FALSE)
}

# ------------------------------------------------------------------------------
# SCRAPING CEPEA/ESALQ
# Tenta buscar indicadores de insumos agrícolas do CEPEA
# URL: https://www.cepea.esalq.usp.br/br/indicador/insumos-agropecuarios.aspx
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# SCRAPING CEPEA / AGROLINK / NOTÍCIAS AGRÍCOLAS
#
# PROBLEMA CONHECIDO: CEPEA e AgroLink bloqueiam user-agents genéricos e
# usam JavaScript para renderizar preços (conteúdo dinâmico). A estratégia
# abaixo usa cabeçalhos de navegador completos + múltiplas fontes alternativas.
# ------------------------------------------------------------------------------
scrape_cepea <- function() {

  resultado <- list(
    sucesso  = FALSE,
    precos   = list(),
    mensagem = "",
    data     = format(Sys.Date(), "%d/%m/%Y"),
    fonte    = "CEPEA/ESALQ"
  )

  # Cabeçalhos que imitam um navegador Chrome real
  headers_chrome <- c(
    "User-Agent"      = paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
                               "AppleWebKit/537.36 (KHTML, like Gecko) ",
                               "Chrome/124.0.0.0 Safari/537.36"),
    "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language" = "pt-BR,pt;q=0.9,en;q=0.8",
    "Accept-Encoding" = "gzip, deflate, br",
    "Connection"      = "keep-alive",
    "Upgrade-Insecure-Requests" = "1",
    "Sec-Fetch-Dest"  = "document",
    "Sec-Fetch-Mode"  = "navigate",
    "Sec-Fetch-Site"  = "none",
    "Cache-Control"   = "max-age=0"
  )

  # Produtos buscados e seus nomes no CSV
  mapa_produtos <- list(
    list(termos = c("ureia", "ur\u00e9ia", "urea"),
         produtos = "Ureia (45% N)", nutriente = "N"),
    list(termos = c("map", "fosfato monoam\u00f4nico", "monoamonium"),
         produtos = c("MAP (10% N)", "MAP (48% P\u2082O\u2085)"), nutriente = c("N","P2O5")),
    list(termos = c("kcl", "cloreto de pot\u00e1ssio", "potassio"),
         produtos = "KCl (60% K\u2082O)", nutriente = "K2O"),
    list(termos = c("superfosfato triplo", "sft", "triple superphosphate"),
         produtos = "Superfosfato Triplo (46% P\u2082O\u2085)", nutriente = "P2O5"),
    list(termos = c("dap", "fosfato diam\u00f4nico", "diammonium"),
         produtos = c("DAP (18% N)", "DAP (46% P\u2082O\u2085)"), nutriente = c("N","P2O5")),
    list(termos = c("sulfato de am\u00f4nio", "sulfato am\u00f4nio", "ammonium sulfate"),
         produtos = "Sulfato de Am\u00f4nio (21% N)", nutriente = "N"),
    list(termos = c("ssimples", "superfosfato simples", "ssp"),
         produtos = "Superfosfato Simples (18% P\u2082O\u2085)", nutriente = "P2O5"),
    list(termos = c("nitrato de am\u00f4nio", "ammonium nitrate"),
         produtos = "Nitrato de Am\u00f4nio (32% N)", nutriente = "N")
  )

  # ---- Fonte 1: CEPEA insumos ----
  precos <- tentar_fonte(
    url      = "https://www.cepea.esalq.usp.br/br/indicador/insumos-agropecuarios.aspx",
    headers  = headers_chrome,
    mapa     = mapa_produtos,
    timeout  = 20
  )
  if (length(precos) > 0) {
    resultado$sucesso  <- TRUE
    resultado$precos   <- precos
    resultado$fonte    <- "CEPEA/ESALQ"
    resultado$mensagem <- paste0("\u2713 ", length(precos),
      " produto(s) atualizados via CEPEA em ", resultado$data)
    return(resultado)
  }

  # ---- Fonte 2: AgroLink cotações ----
  precos <- tentar_fonte(
    url      = "https://www.agrolink.com.br/cotacoes/insumos",
    headers  = c(headers_chrome,
                 "Referer" = "https://www.google.com.br/"),
    mapa     = mapa_produtos,
    timeout  = 20
  )
  if (length(precos) > 0) {
    resultado$sucesso  <- TRUE
    resultado$precos   <- precos
    resultado$fonte    <- "AgroLink"
    resultado$mensagem <- paste0("\u2713 ", length(precos),
      " produto(s) atualizados via AgroLink em ", resultado$data)
    return(resultado)
  }

  # ---- Fonte 3: Notícias Agrícolas (fertilizantes) ----
  precos <- tentar_fonte(
    url      = "https://www.noticiasagricolas.com.br/cotacoes/fertilizantes",
    headers  = c(headers_chrome,
                 "Referer" = "https://www.google.com.br/"),
    mapa     = mapa_produtos,
    timeout  = 20
  )
  if (length(precos) > 0) {
    resultado$sucesso  <- TRUE
    resultado$precos   <- precos
    resultado$fonte    <- "Not\u00edcias Agr\u00edcolas"
    resultado$mensagem <- paste0("\u2713 ", length(precos),
      " produto(s) atualizados via Not\u00edcias Agr\u00edcolas em ", resultado$data)
    return(resultado)
  }

  # ---- Fonte 4: Canal Rural ----
  precos <- tentar_fonte(
    url      = "https://www.canalrural.com.br/cotacoes/insumos-agricolas/",
    headers  = c(headers_chrome,
                 "Referer" = "https://www.google.com.br/"),
    mapa     = mapa_produtos,
    timeout  = 20
  )
  if (length(precos) > 0) {
    resultado$sucesso  <- TRUE
    resultado$precos   <- precos
    resultado$fonte    <- "Canal Rural"
    resultado$mensagem <- paste0("\u2713 ", length(precos),
      " produto(s) atualizados via Canal Rural em ", resultado$data)
    return(resultado)
  }

  # ---- Falhou em todas as fontes ----
  resultado$mensagem <- paste0(
    "\u26a0 N\u00e3o foi poss\u00edvel obter pre\u00e7os online. ",
    "Isso ocorre quando os sites usam JavaScript din\u00e2mico para carregar pre\u00e7os ",
    "(bot\u00f5es que s\u00f3 funcionam em navegador real) ou bloqueiam requisi\u00e7\u00f5es autom\u00e1ticas. ",
    "Os pre\u00e7os do arquivo local continuam sendo usados. ",
    "Atualize os pre\u00e7os manualmente na tabela abaixo."
  )
  return(resultado)
}

# ------------------------------------------------------------------------------
# TENTAR FONTE: faz GET com headers completos e extrai preços
# ------------------------------------------------------------------------------
tentar_fonte <- function(url, headers, mapa, timeout = 20) {

  resposta <- tryCatch(
    do.call(httr::GET, c(
      list(url      = url,
           httr::timeout(timeout)),
      lapply(seq_along(headers), function(i)
        httr::add_headers(.headers = setNames(headers[i], names(headers)[i]))
      )
    )),
    error = function(e) NULL
  )

  # Simplificado: usa httr::add_headers de uma vez
  resposta <- tryCatch(
    httr::GET(
      url     = url,
      config  = httr::add_headers(.headers = headers),
      httr::timeout(timeout)
    ),
    error = function(e) NULL
  )

  if (is.null(resposta)) return(list())
  if (httr::status_code(resposta) != 200) return(list())

  html_text <- tryCatch(
    httr::content(resposta, "text", encoding = "UTF-8"),
    error = function(e) ""
  )

  if (nchar(html_text) < 500) return(list())  # página vazia/JS-only

  extrair_precos_html(html_text, mapa)
}

# ------------------------------------------------------------------------------
# EXTRAIR PREÇOS DO HTML
# Tenta múltiplas estratégias: rvest → regex de padrões monetários
# ------------------------------------------------------------------------------
extrair_precos_html <- function(html_text, mapa_produtos) {
  precos <- list()

  # Estratégia 1: usar rvest (tabelas HTML)
  if (requireNamespace("rvest", quietly = TRUE)) {
    tryCatch({
      pagina  <- rvest::read_html(html_text)
      tabelas <- rvest::html_table(pagina, fill = TRUE)

      for (tab in tabelas) {
        if (ncol(tab) < 2) next
        tab_txt <- apply(tab, 2, function(x) tolower(as.character(x)))

        for (mp in mapa_produtos) {
          for (termo in mp$termos) {
            linhas <- which(apply(tab_txt, 1,
                                   function(r) any(grepl(termo, r, fixed = TRUE))))
            if (length(linhas) == 0) next

            for (ln in linhas) {
              row_vals <- as.character(tab[ln, ])
              nums <- regmatches(row_vals,
                                  gregexpr("[0-9]{1,4}[.,][0-9]{2}", row_vals))
              nums_clean <- suppressWarnings(as.numeric(
                gsub(",", ".", gsub("\\.", "", unlist(nums)))
              ))
              # Faixa plausível: R$0,50/kg a R$25/kg para fertilizantes
              validos <- nums_clean[!is.na(nums_clean) & nums_clean > 0.5 & nums_clean < 25]
              # Valores > 25 podem ser R$/t → converte dividindo por 1000
              em_tonelada <- nums_clean[!is.na(nums_clean) & nums_clean >= 100 & nums_clean < 25000]
              if (length(em_tonelada) > 0 && length(validos) == 0) {
                validos <- em_tonelada / 1000
              }

              if (length(validos) > 0) {
                for (prod in mp$produtos) precos[[prod]] <- validos[1]
                break
              }
            }
            if (length(precos) > 0) break
          }
        }
      }
    }, error = function(e) NULL)
  }

  # Estratégia 2: regex direto no HTML (fallback para conteúdo não-tabular)
  if (length(precos) == 0) {
    html_lower <- tolower(html_text)
    for (mp in mapa_produtos) {
      for (termo in mp$termos) {
        pos <- gregexpr(termo, html_lower, fixed = TRUE)[[1]]
        if (pos[1] == -1) next

        for (p in pos) {
          trecho <- substr(html_text, p, p + 400)
          # Padrão R$ seguido de número
          m <- regmatches(trecho,
                           regexpr("R\\$\\s*([0-9]+[.,][0-9]+)", trecho, perl = TRUE))
          if (length(m) == 0 || nchar(m) == 0) {
            # Tenta apenas número com vírgula (sem R$)
            m <- regmatches(trecho,
                             regexpr("[0-9]{1,4},[0-9]{2}", trecho, perl = TRUE))
          }
          if (length(m) == 0 || nchar(m) == 0) next

          val_str <- regmatches(m, regexpr("[0-9]+[.,][0-9]+", m))
          if (length(val_str) == 0) next
          val <- suppressWarnings(as.numeric(gsub(",", ".", gsub("\\.", "", val_str))))
          if (is.na(val) || val <= 0) next
          if (val >= 100) val <- val / 1000  # R$/t → R$/kg
          if (val < 0.5 || val > 25) next

          for (prod in mp$produtos) precos[[prod]] <- val
          break
        }
        if (length(precos) > 0) break
      }
    }
  }

  return(precos)
}

# ------------------------------------------------------------------------------
# APLICAR PREÇOS CEPEA AO DATAFRAME DO CSV
# ------------------------------------------------------------------------------
aplicar_precos_cepea <- function(df_csv, precos_novos, fonte = "CEPEA/ESALQ") {
  data_hoje <- format(Sys.Date(), "%Y-%m")
  
  for (prod in names(precos_novos)) {
    idx <- which(df_csv$produto == prod)
    if (length(idx) > 0) {
      df_csv$preco_ref[idx] <- precos_novos[[prod]]
      df_csv$fonte[idx]     <- fonte
      df_csv$data_ref[idx]  <- data_hoje
    }
  }
  return(df_csv)
}

# ------------------------------------------------------------------------------
# FORMATAR DATA DE REFERÊNCIA para exibição
# ------------------------------------------------------------------------------
formatar_data_ref <- function(data_ref_str) {
  tryCatch({
    d <- as.Date(paste0(data_ref_str, "-01"))
    format(d, "%b/%Y")
  }, error = function(e) data_ref_str)
}
