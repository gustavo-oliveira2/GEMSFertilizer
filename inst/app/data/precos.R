# ==============================================================================
# MÓDULO DE PREÇOS — Opção D (CSV) + Scraping CEPEA (opcional)
# ==============================================================================

# Caminho do CSV de referência
# Se app.R já definiu PRECOS_CSV (modo pacote), respeita; senão usa default local
if (!exists("PRECOS_CSV")) {
  PRECOS_CSV <- "data/precos_referencia.csv"
}

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
scrape_cepea <- function() {
  
  # Produtos monitorados pelo CEPEA e seus nomes no CSV
  # O CEPEA publica: Ureia, MAP, KCl, SFT, DAP, Sulfato de Amônio
  mapa_produtos <- list(
    list(
      cepea_termo  = c("ureia", "ur\u00e9ia"),
      csv_produto  = "Ureia (45% N)",
      nutriente    = "N"
    ),
    list(
      cepea_termo  = c("map", "fosfato monoam\u00f4nico"),
      csv_produto  = c("MAP (10% N)", "MAP (48% P\u2082O\u2085)"),
      nutriente    = c("N", "P2O5")
    ),
    list(
      cepea_termo  = c("kcl", "cloreto de pot\u00e1ssio"),
      csv_produto  = "KCl (60% K\u2082O)",
      nutriente    = "K2O"
    ),
    list(
      cepea_termo  = c("superfosfato triplo", "sft"),
      csv_produto  = "Superfosfato Triplo (46% P\u2082O\u2085)",
      nutriente    = "P2O5"
    ),
    list(
      cepea_termo  = c("dap", "fosfato diam\u00f4nico"),
      csv_produto  = c("DAP (18% N)", "DAP (46% P\u2082O\u2085)"),
      nutriente    = c("N", "P2O5")
    ),
    list(
      cepea_termo  = c("sulfato de am\u00f4nio", "sulfato am\u00f4nio"),
      csv_produto  = "Sulfato de Am\u00f4nio (21% N)",
      nutriente    = "N"
    )
  )
  
  resultado <- list(
    sucesso   = FALSE,
    precos    = list(),
    mensagem  = "",
    data      = format(Sys.Date(), "%d/%m/%Y"),
    fonte     = "CEPEA/ESALQ"
  )
  
  # ---- Tentativa 1: página de insumos do CEPEA ----
  url_cepea <- "https://www.cepea.esalq.usp.br/br/indicador/insumos-agropecuarios.aspx"
  
  resposta <- tryCatch(
    httr::GET(url_cepea,
      httr::timeout(15),
      httr::user_agent("Mozilla/5.0 (compatible; GEMS_Fertilizer/1.0)")
    ),
    error = function(e) NULL
  )
  
  if (!is.null(resposta) && httr::status_code(resposta) == 200) {
    html_text <- httr::content(resposta, "text", encoding = "UTF-8")
    
    # Tenta extrair tabelas com rvest (se disponível) ou regex
    precos_extraidos <- extrair_precos_html(html_text, mapa_produtos)
    
    if (length(precos_extraidos) > 0) {
      resultado$sucesso  <- TRUE
      resultado$precos   <- precos_extraidos
      resultado$mensagem <- paste0("✓ ", length(precos_extraidos), " produto(s) atualizados via CEPEA em ", resultado$data)
      return(resultado)
    }
  }
  
  # ---- Tentativa 2: AgroLink indicadores ----
  url_agrolink <- "https://www.agrolink.com.br/cotacoes/insumos"
  
  resposta2 <- tryCatch(
    httr::GET(url_agrolink,
      httr::timeout(15),
      httr::user_agent("Mozilla/5.0 (compatible; GEMS_Fertilizer/1.0)")
    ),
    error = function(e) NULL
  )
  
  if (!is.null(resposta2) && httr::status_code(resposta2) == 200) {
    html_text2 <- httr::content(resposta2, "text", encoding = "UTF-8")
    precos_extraidos2 <- extrair_precos_html(html_text2, mapa_produtos)
    
    if (length(precos_extraidos2) > 0) {
      resultado$sucesso  <- TRUE
      resultado$precos   <- precos_extraidos2
      resultado$fonte    <- "AgroLink"
      resultado$mensagem <- paste0("✓ ", length(precos_extraidos2), " produto(s) atualizados via AgroLink em ", resultado$data)
      return(resultado)
    }
  }
  
  # ---- Falhou ----
  resultado$mensagem <- paste0(
    "⚠ Não foi possível conectar às fontes de preços online. ",
    "Verifique sua conexão com a internet. ",
    "Os preços do arquivo local continuarão sendo usados."
  )
  return(resultado)
}

# ------------------------------------------------------------------------------
# EXTRAIR PREÇOS DO HTML
# Tenta múltiplas estratégias: rvest → regex de padrões monetários
# ------------------------------------------------------------------------------
extrair_precos_html <- function(html_text, mapa_produtos) {
  precos <- list()
  
  # Estratégia 1: usar rvest se disponível
  if (requireNamespace("rvest", quietly = TRUE)) {
    tryCatch({
      pagina <- rvest::read_html(html_text)
      tabelas <- rvest::html_table(pagina, fill = TRUE)
      
      for (tab in tabelas) {
        if (ncol(tab) < 2) next
        tab_txt <- apply(tab, 2, tolower)
        
        for (mp in mapa_produtos) {
          for (termo in mp$cepea_termo) {
            # Procura linhas que contenham o produto
            linhas <- which(apply(tab_txt, 1, function(r) any(grepl(termo, r, fixed = TRUE))))
            if (length(linhas) == 0) next
            
            for (ln in linhas) {
              row_vals <- as.character(tab[ln, ])
              # Extrai valor numérico (padrão BR: 1.234,56 ou 1234.56)
              nums <- regmatches(row_vals, gregexpr("[0-9]+[.,][0-9]+", row_vals))
              nums_clean <- unlist(lapply(nums, function(x) {
                x <- gsub("\\.", "", x)   # remove separador de milhar
                x <- gsub(",", ".", x)    # decimal para ponto
                as.numeric(x)
              }))
              nums_clean <- nums_clean[!is.na(nums_clean) & nums_clean > 0.5 & nums_clean < 50]
              
              if (length(nums_clean) > 0) {
                preco <- nums_clean[1]
                # Normaliza: CEPEA publica por tonelada — converte para kg
                if (preco > 100) preco <- preco / 1000
                
                for (prod in mp$csv_produto) {
                  precos[[prod]] <- preco
                }
                break
              }
            }
            if (length(precos) > 0) break
          }
        }
      }
    }, error = function(e) NULL)
  }
  
  # Estratégia 2: regex direto no HTML (fallback)
  if (length(precos) == 0) {
    for (mp in mapa_produtos) {
      for (termo in mp$cepea_termo) {
        # Padrão: nome do produto seguido de valor monetário em até 200 chars
        padrao <- paste0("(?i)", termo, ".{0,200}?R\\$\\s*([0-9]+[.,][0-9]+)")
        m <- regmatches(html_text, regexpr(padrao, html_text, perl = TRUE))
        
        if (length(m) > 0 && nchar(m) > 0) {
          val_str <- regmatches(m, regexpr("[0-9]+[.,][0-9]+\\s*$", m))
          if (length(val_str) > 0) {
            val <- as.numeric(gsub(",", ".", gsub("\\.", "", val_str)))
            if (!is.na(val) && val > 0.5 && val < 50) {
              for (prod in mp$csv_produto) {
                precos[[prod]] <- val
              }
            }
          }
        }
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
