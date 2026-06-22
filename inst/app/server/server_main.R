# ==============================================================================
# SERVER PRINCIPAL - ADUBO CERTO
# ==============================================================================

server <- function(input, output, session) {
  
  # --------------------------------------------------------------------------
  # ESTADO REATIVO DE PREÇOS
  # --------------------------------------------------------------------------
  
  # Carrega preços do CSV na inicialização
  rv_precos <- reactiveVal({
    df <- carregar_precos_csv()
    df
  })
  
  # Status da última atualização (fonte + data)
  rv_preco_status <- reactiveVal(list(
    fonte    = "CSV local",
    data     = "",
    n_atualiz = 0L
  ))
  
  # Badge de status na toolbar
  output$preco_status_badge <- renderUI({
    st <- rv_preco_status()
    df <- rv_precos()
    
    # Detecta se algum produto foi atualizado pelo CEPEA
    tem_cepea <- "fonte" %in% names(df) &&
                 any(grepl("CEPEA|AgroLink", df$fonte, ignore.case = TRUE))
    
    data_mais_recente <- if ("data_ref" %in% names(df)) {
      datas <- sort(unique(df$data_ref), decreasing = TRUE)
      if (length(datas) > 0) formatar_data_ref(datas[1]) else ""
    } else ""
    
    if (st$n_atualiz > 0 && tem_cepea) {
      span(class = "badge-fonte badge-cepea",
        HTML(paste0('<i class="bi bi-cloud-check"></i> CEPEA — ',
                    st$n_atualiz, " produto(s) — ", st$data)))
    } else if (file.exists(PRECOS_CSV)) {
      span(class = "badge-fonte badge-local",
        HTML(paste0('<i class="bi bi-hdd"></i> Arquivo local — ', data_mais_recente)))
    } else {
      span(class = "badge-fonte badge-fallback",
        HTML('<i class="bi bi-database"></i> Preços embutidos'))
    }
  })
  
  # --------------------------------------------------------------------------
  # TABELA EDITÁVEL DE PREÇOS (DT)
  # --------------------------------------------------------------------------
  output$tabela_precos_edit <- renderDT({
    df <- rv_precos()
    
    # Colunas para exibir
    cols_show <- c("produto", "nutriente", "teor", "preco_ref", "fonte", "data_ref")
    cols_show <- cols_show[cols_show %in% names(df)]
    df_show <- df[, cols_show, drop = FALSE]
    
    # Renomear para exibição
    nomes_pt <- c(
      produto   = "Produto Comercial",
      nutriente = "Nutriente",
      teor      = "Teor (fração)",
      preco_ref = "Preço (R$/kg)",
      fonte     = "Fonte",
      data_ref  = "Ref."
    )
    names(df_show) <- nomes_pt[names(df_show)]
    
    # Índice da coluna editável (Preço)
    col_preco_idx <- which(names(df_show) == "Preço (R$/kg)") - 1  # DT é 0-indexed
    
    datatable(
      df_show,
      rownames  = FALSE,
      editable  = list(target = "cell", disable = list(columns = setdiff(0:(ncol(df_show)-1), col_preco_idx))),
      selection = "none",
      class     = "stripe hover compact",
      options   = list(
        pageLength  = 20,
        dom         = "fti",
        scrollX     = TRUE,
        language    = list(
          search      = "Filtrar:",
          lengthMenu  = "Mostrar _MENU_ linhas",
          info        = "Mostrando _START_ a _END_ de _TOTAL_ produtos",
          zeroRecords = "Nenhum produto encontrado"
        ),
        columnDefs  = list(
          list(className = "dt-center", targets = c(1, 2, 3, 5)),
          list(className = "dt-left",   targets = 0),
          # Destaca coluna editável
          list(className = "dt-body-right", targets = col_preco_idx)
        ),
        initComplete = JS("
          function(settings, json) {
            $(this.api().table().header()).css({'background-color': '#1b4332', 'color': 'white'});
          }
        ")
      )
    ) %>%
      formatCurrency("Preço (R$/kg)", currency = "R$ ", digits = 2,
                     mark = ".", dec.mark = ",") %>%
      formatPercentage("Teor (fração)", digits = 0)
  }, server = FALSE)
  
  # Capturar edições do usuário na tabela de preços
  observeEvent(input$tabela_precos_edit_cell_edit, {
    info <- input$tabela_precos_edit_cell_edit
    df   <- rv_precos()
    
    # Coluna 3 (0-indexed) = preco_ref (4ª coluna)
    col_preco_r <- 4  # 1-indexed no df real
    
    novo_val <- suppressWarnings(as.numeric(info$value))
    if (!is.na(novo_val) && novo_val >= 0) {
      df$preco_ref[info$row] <- novo_val
      df$fonte[info$row]     <- "Manual"
      df$data_ref[info$row]  <- format(Sys.Date(), "%Y-%m")
      rv_precos(df)
    }
  })
  
  # --------------------------------------------------------------------------
  # BOTÃO SALVAR PREÇOS
  # --------------------------------------------------------------------------
  observeEvent(input$btn_salvar_precos, {
    df  <- rv_precos()
    ok  <- salvar_precos_csv(df)
    
    showNotification(
      if (ok)
        HTML('<i class="bi bi-check-circle-fill"></i> Preços salvos em <b>data/precos_referencia.csv</b>')
      else
        HTML('<i class="bi bi-x-circle-fill"></i> Erro ao salvar. Verifique permissões de escrita.'),
      type    = if (ok) "message" else "error",
      duration = 4
    )
  })
  
  observeEvent(input$btn_salvar_precos, {
    df  <- rv_precos()
    ok  <- salvar_precos_csv(df)
    
    showNotification(
      if (ok)
        HTML('<i class="bi bi-check-circle-fill"></i> Preços salvos em <b>data/precos_referencia.csv</b>')
      else
        HTML('<i class="bi bi-x-circle-fill"></i> Erro ao salvar. Verifique permissões de escrita.'),
      type    = if (ok) "message" else "error",
      duration = 4
    )
  })

  # --------------------------------------------------------------------------
  # NÍVEL TECNOLÓGICO — componente compacto (resumo fixo + checklist retrátil)
  # --------------------------------------------------------------------------

  # Reativo: itens marcados
  rv_tratos_marcados <- reactive({
    ids <- sapply(CHECKLIST_TRATOS, function(x) x$id)
    ids[sapply(ids, function(id) {
      v <- input[[paste0("trato_", id)]]
      !is.null(v) && isTRUE(v)
    })]
  })

  # Reativo: classificação do nível
  rv_nivel_tec <- reactive({
    classificar_nivel(rv_tratos_marcados())
  })

  # Resumo fixo: nível + barra + contagem (sempre visível, compacto)
  output$resumo_nivel_tec <- renderUI({
    res  <- rv_nivel_tec()
    marc <- length(rv_tratos_marcados())
    tot  <- length(CHECKLIST_TRATOS)

    div(style = "display:flex; align-items:center; gap:10px;",
      # Selo circular do nível
      div(style = paste0(
            "flex-shrink:0; width:42px; height:42px; border-radius:50%; ",
            "background:", res$cor, "; display:flex; align-items:center; ",
            "justify-content:center; color:white; font-weight:700; font-size:13px; ",
            "box-shadow:0 2px 6px ", res$cor, "55;"),
        paste0(res$pct, "%")
      ),
      div(style = "flex:1; min-width:0;",
        div(style = paste0("font-size:12.5px; font-weight:700; color:", res$cor, ";"),
          res$nivel_label
        ),
        div(style = "background:#E8E8E8; border-radius:3px; height:5px; margin:3px 0 2px;",
          div(style = paste0("background:", res$cor, "; width:", res$pct, "%; ",
                              "height:5px; border-radius:3px; transition:width 0.35s;"))
        ),
        div(style = "font-size:10px; color:#999;",
          paste0(marc, " de ", tot, " pr\u00e1ticas adotadas")
        )
      )
    )
  })

  # Checklist completo — renderiza uma vez, fica dentro de <details> no UI
  output$checklist_tratos_ui <- renderUI({
    cats <- categorias_checklist()
    tagList(lapply(cats, function(cat_group) {
      div(style = "margin-bottom:8px;",
        div(style = paste0(
              "font-size:9.5px; font-weight:700; color:#7A8A7E; ",
              "text-transform:uppercase; letter-spacing:0.5px; ",
              "margin:8px 0 4px;"),
          cat_group$cat
        ),
        lapply(cat_group$itens, function(item) {
          tags$label(
            style = paste0("display:flex; align-items:flex-start; gap:7px; ",
                            "padding:3px 4px; margin-bottom:1px; border-radius:4px; ",
                            "cursor:pointer; font-weight:400;"),
            onmouseover = "this.style.background='#F4F6F4'",
            onmouseout  = "this.style.background='transparent'",
            tags$input(
              type = "checkbox",
              id   = paste0("trato_", item$id),
              style = "margin-top:2px; accent-color:#2D6A4F; cursor:pointer; flex-shrink:0;"
            ),
            tags$span(style = "font-size:11.5px; color:#444; line-height:1.35;",
              item$label, " ",
              tags$span(style = paste0(
                  "font-size:8.5px; font-weight:700; padding:0px 5px; ",
                  "border-radius:8px; white-space:nowrap; ",
                  "background:", if(item$nivel_min=="alto") "#E3F2FD" else "#E8F5E9", "; ",
                  "color:", if(item$nivel_min=="alto") "#1565C0" else "#2E7D32", ";"),
                if(item$nivel_min=="alto") "ALTA" else "M\u00c9DIA"
              )
            )
          )
        })
      )
    }))
  })

  # Badge no cabeçalho do card (compacto, ao lado do título)
  output$badge_nivel_tec <- renderUI({
    res <- rv_nivel_tec()
    span(style = paste0("margin-left:auto; padding:2px 9px; border-radius:10px; ",
                         "font-size:9.5px; font-weight:700; color:white; ",
                         "background:", res$cor, "; white-space:nowrap;"),
      res$nivel_label
    )
  })

  # Card de justificativa técnica — só aparece quando nível ≠ médio
  output$resultado_nivel_tec <- renderUI({
    res <- rv_nivel_tec()
    if (res$nivel == "medio") return(NULL)
    f <- FATORES_NIVEL
    div(style = paste0("font-size:10.5px; color:#666; background:", res$cor, "0D; ",
                        "border-left:3px solid ", res$cor, "; padding:6px 10px; ",
                        "border-radius:0 6px 6px 0; margin-top:8px; line-height:1.5;"),
      HTML(paste0(
        '<i class="bi bi-sliders"></i> <b>Ajuste de dose aplicado: </b>',
        'N plantio \u00d7', f$n_plantio[[res$nivel]],
        ', N cobertura \u00d7', f$n_cobertura[[res$nivel]],
        ', P\u2082O\u2085 \u00d7', f$p2o5[[res$nivel]],
        ', K\u2082O \u00d7', f$k2o[[res$nivel]]
      ))
    )
  })

  # --------------------------------------------------------------------------
  # TOGGLE MÓDULOS AVANÇADOS (cadeado)
  # --------------------------------------------------------------------------
  rv_avancado <- reactiveVal(FALSE)

  observeEvent(input$btn_toggle_avancado, {
    novo <- !rv_avancado()
    rv_avancado(novo)
    ids <- c("tab_lote","tab_pesquisa","tab_regional","tab_clima")
    if (novo) {
      for (id in ids) shinyjs::show(id)
      shinyjs::addClass("btn_toggle_avancado", "desbloqueado")
      shinyjs::html("btn_toggle_avancado", '<i class="bi bi-unlock-fill"></i>')
      showNotification(
        HTML('<i class="bi bi-unlock-fill"></i> M\u00f3dulos avan\u00e7ados liberados.'),
        type = "message", duration = 3
      )
    } else {
      for (id in ids) shinyjs::hide(id)
      shinyjs::removeClass("btn_toggle_avancado", "desbloqueado")
      shinyjs::html("btn_toggle_avancado", '<i class="bi bi-lock-fill"></i>')
    }
  })

  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # ANÁLISE EM LOTE — estratégia híbrida: template + upload flexível
  # --------------------------------------------------------------------------
  rv_lote_df         <- reactiveVal(NULL)
  rv_lote_resultados <- reactiveVal(NULL)

  # Download do template
  output$btn_download_template_lote <- downloadHandler(
    filename = function() paste0("template_analises_lote_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      tmpl <- file.path(getwd(), "template_analises_lote.xlsx")
      if (!file.exists(tmpl))
        tmpl <- system.file("app/template_analises_lote.xlsx", package="GEMSFertilizer")
      if (file.exists(tmpl)) file.copy(tmpl, file)
      else writeLines("Template não encontrado.", file)
    }
  )

  # Upload e leitura da planilha
  observeEvent(input$lote_file, {
    req(input$lote_file)
    rv_lote_resultados(NULL)

    res <- ler_lote(input$lote_file$datapath)
    if (!is.null(res$erro)) {
      rv_lote_df(NULL)
      showNotification(res$erro, type="error", duration=8)
      return()
    }
    rv_lote_df(res$dados)
    msg <- paste0(res$n, " amostra(s) importada(s).")
    if (length(res$avisos) > 0)
      msg <- paste0(msg, " Avisos: ", paste(res$avisos, collapse=" "))
    showNotification(HTML(paste0('<i class="bi bi-check-circle-fill"></i> ', msg)),
                     type = if(length(res$avisos)>0) "warning" else "message", duration=5)
  })

  # Status do upload e mapeamento de colunas detectadas
  output$lote_upload_status <- renderUI({
    df <- rv_lote_df()
    if (is.null(df)) return(NULL)
    cols_ok    <- intersect(c("ph","mo","p","k","ca","mg","al","h_al"), names(df))
    cols_falta <- setdiff(c("ph","p","k","ca","mg","al","h_al"), names(df))
    cor  <- if(length(cols_falta)==0) "#eafaf1" else "#fff8e1"
    bord <- if(length(cols_falta)==0) "#27ae60" else "#f39c12"
    ico  <- if(length(cols_falta)==0) "check-circle-fill" else "exclamation-triangle-fill"
    div(style=paste0("background:",cor,";border-left:3px solid ",bord,
                     ";padding:8px 12px;border-radius:4px;font-size:12px;margin-top:6px;"),
      HTML(paste0(
        '<i class="bi bi-', ico, '"></i> ',
        "<b>", nrow(df), " amostras</b> carregadas. ",
        if(length(cols_ok)>0) paste0("Colunas detectadas: <b>",
          paste(toupper(cols_ok), collapse=", "), "</b>. ") else "",
        if(length(cols_falta)>0) paste0(
          "<span style='color:#e67e22;'>Colunas não encontradas: <b>",
          paste(toupper(cols_falta), collapse=", "), "</b>. ",
          "Verifique os nomes no arquivo.</span>") else ""
      ))
    )
  })

  # Tabela de revisão editável
  output$lote_tabela_revisao <- renderUI({
    df <- rv_lote_df()
    if (is.null(df)) return(NULL)
    cols_show <- intersect(c("id_amostra","municipio","ph","mo","p","k",
                              "ca","mg","al","h_al","argila","p_rem",
                              "cultura","produtividade","prnt"), names(df))
    df_show <- df[, cols_show, drop=FALSE]
    div(class="result-card",
      div(class="result-card-title",
        HTML(paste0('<i class="bi bi-table"></i> Passo 3 — Revise os dados ',
                     '<span class="stat-badge-n">n = ', nrow(df_show), '</span>'))),
      p(style="font-size:11px;color:#888;margin-bottom:8px;",
        HTML('<i class="bi bi-pencil-square"></i> ',
             'Clique em qualquer c\u00e9lula para editar. ',
             'Valores em branco s\u00e3o tratados como n\u00e3o informados.')),
      div(style="overflow-x:auto;",
        DTOutput("lote_dt_editavel")
      )
    )
  })

  output$lote_dt_editavel <- renderDT({
    df <- rv_lote_df()
    if (is.null(df)) return(NULL)
    cols_show <- intersect(c("id_amostra","municipio","ph","mo","p","k",
                              "ca","mg","al","h_al","argila","p_rem",
                              "cultura","produtividade","prnt"), names(df))
    datatable(df[, cols_show, drop=FALSE],
      rownames=FALSE, selection="none", editable="cell",
      class="stripe hover compact",
      options=list(pageLength=15, dom="tip", scrollX=TRUE,
        language=list(paginate=list(previous="Anterior","next"="Pr\u00f3ximo"))))
  }, server=FALSE)

  # Salva edições na tabela
  observeEvent(input$lote_dt_editavel_cell_edit, {
    df   <- rv_lote_df()
    info <- input$lote_dt_editavel_cell_edit
    cols_show <- intersect(c("id_amostra","municipio","ph","mo","p","k",
                              "ca","mg","al","h_al","argila","p_rem",
                              "cultura","produtividade","prnt"), names(df))
    col_name <- cols_show[info$col + 1]
    novo_val <- info$value
    # Tenta converter para numérico se a coluna for numérica
    if (col_name %in% c("ph","mo","p","k","ca","mg","al","h_al","argila","p_rem","prnt"))
      novo_val <- suppressWarnings(as.numeric(gsub(",",".",novo_val)))
    df[info$row, col_name] <- novo_val
    rv_lote_df(df)
  })

  # Configuração da recomendação (aparece após upload)
  output$lote_config_rec <- renderUI({
    if (is.null(rv_lote_df())) return(NULL)
    df <- rv_lote_df()
    # Se a planilha já tem coluna "cultura", não pergunta
    tem_cultura <- "cultura" %in% names(df) && sum(!is.na(df$cultura)) > 0
    div(class="result-card",
      div(class="result-card-title",
        HTML('<i class="bi bi-sliders"></i> Passo 4 — Par\u00e2metros de recomenda\u00e7\u00e3o')),
      p(style="font-size:11.5px;color:#888;margin-bottom:10px;",
        if(tem_cultura)
          HTML('<i class="bi bi-check-circle-fill" style="color:#27ae60;"></i> ',
               'Cultura detectada na planilha. Os valores abaixo s\u00e3o usados apenas para ',
               'amostras sem cultura definida.')
        else
          HTML('<i class="bi bi-info-circle"></i> Defina os par\u00e2metros para as recomenda\u00e7\u00f5es:')
      ),
      div(class="form-row-2",
        div(class="form-group-custom",
          label_custom("Cultura padr\u00e3o", "bi-flower1"),
          selectInput("lote_cultura", NULL, width="100%",
            choices=c("Milho"="milho","Feij\u00e3o"="feijao","Mandioca"="mandioca",
                      "Cana-de-a\u00e7\u00facar"="cana","Arroz"="arroz",
                      "Amendoim"="amendoim","Sorgo"="sorgo",
                      "Pastagem"="pastagem","Abacaxi"="abacaxi"))
        ),
        div(class="form-group-custom",
          label_custom("Produtividade padr\u00e3o", "bi-graph-up"),
          selectInput("lote_produtividade", NULL, width="100%",
            choices=c("Baixa"="baixa","M\u00e9dia"="media","Alta"="alta"),
            selected="media")
        )
      ),
      div(class="form-row-2",
        div(class="form-group-custom",
          label_custom("PRNT do calc\u00e1rio (%)", "bi-calculator"),
          numericInput("lote_prnt", NULL, value=80, min=30, max=100, step=5, width="100%")
        ),
        div()
      ),
      div(class="btn-container",
        actionButton("btn_calcular_lote",
          HTML('<i class="bi bi-calculator-fill"></i>&nbsp; GERAR RECOMENDA\u00c7\u00d5ES'),
          class="btn-calcular", width="100%")
      )
    )
  })

  # Cálculo das recomendações
  observeEvent(input$btn_calcular_lote, {
    df <- rv_lote_df()
    req(df)
    withProgress(message="Calculando recomenda\u00e7\u00f5es...", {
      resultados <- lapply(seq_len(nrow(df)), function(i) {
        incProgress(1/nrow(df))
        am <- df[i, ]
        # Usa cultura/produtividade da linha se disponível
        cultura_am <- if (!is.null(am$cultura) && !is.na(am$cultura) && nchar(trimws(am$cultura))>0)
                        trimws(am$cultura) else input$lote_cultura
        prod_am    <- if (!is.null(am$produtividade) && !is.na(am$produtividade) && nchar(trimws(am$produtividade))>0)
                        trimws(am$produtividade) else input$lote_produtividade
        prnt_am    <- if (!is.null(am$prnt) && !is.na(am$prnt)) am$prnt else input$lote_prnt

        tryCatch({
          solo <- analisar_solo(
            ph     = am$ph     %||% NA_real_,
            mo     = am$mo     %||% NA_real_,
            p      = am$p      %||% NA_real_,
            k      = am$k      %||% NA_real_,
            ca     = am$ca     %||% NA_real_,
            mg     = am$mg     %||% NA_real_,
            al     = am$al     %||% NA_real_,
            h_al   = am$h_al   %||% NA_real_,
            argila = am$argila %||% NA_real_,
            p_rem  = am$p_rem  %||% NA_real_
          )
          rec <- recomendar_adubacao(
            cultura=cultura_am, p_nivel=solo$p$interp$nivel,
            k_nivel=solo$k$interp$nivel, produtividade=prod_am,
            mo=if(!is.null(solo$mo)) solo$mo$valor else NA_real_,
            fase="plantio", n_anterior="nenhum", toneladas=80
          )
          cal <- calcular_todas_calagen(
            v_atual=solo$v$valor, ctc=solo$ctc$valor,
            al=am$al %||% 0, ca=am$ca %||% 0, mg=am$mg %||% 0,
            v_alvo=(v_alvo_cultura[[cultura_am]]$se) %||% 60,
            prnt=prnt_am
          )
          data.frame(
            Amostra     = as.character(am$id_amostra %||% paste0("AM-",i)),
            Municipio   = as.character(am$municipio %||% NA_character_),
            Cultura     = cultura_am,
            pH          = am$ph %||% NA_real_,
            V_pct       = solo$v$valor,
            Classe_P    = solo$p$interp$nivel,
            Classe_K    = solo$k$interp$nivel,
            N_plantio   = rec$N_plantio   %||% NA_real_,
            N_cobertura = rec$N_cobertura %||% NA_real_,
            P2O5        = rec$P2O5        %||% NA_real_,
            K2O         = rec$K2O         %||% NA_real_,
            NC_V_tha    = round(cal[["V% (5\u00aa Aprox. MG)"]] %||% NA_real_, 2),
            NC_Al_tha   = round(cal[["Al\u00b3\u207a + Ca+Mg"]] %||% NA_real_, 2),
            Erro        = NA_character_,
            stringsAsFactors=FALSE
          )
        }, error=function(e) {
          data.frame(
            Amostra=as.character(am$id_amostra %||% paste0("AM-",i)),
            Municipio=NA_character_, Cultura=cultura_am,
            pH=NA_real_, V_pct=NA_real_, Classe_P=NA_character_, Classe_K=NA_character_,
            N_plantio=NA_real_, N_cobertura=NA_real_, P2O5=NA_real_, K2O=NA_real_,
            NC_V_tha=NA_real_, NC_Al_tha=NA_real_,
            Erro=conditionMessage(e), stringsAsFactors=FALSE
          )
        })
      })
      rv_lote_resultados(do.call(rbind, resultados))
    })
  })

  output$lote_resultados <- renderUI({
    df_res <- rv_lote_resultados()
    if (is.null(df_res)) return(NULL)
    nomes <- c(Amostra="Amostra",Municipio="Munic\u00edpio",Cultura="Cultura",
               pH="pH",V_pct="V%",Classe_P="Classe P",Classe_K="Classe K",
               N_plantio="N plant.\n(kg/ha)",N_cobertura="N cob.\n(kg/ha)",
               P2O5="P\u2082O\u2085\n(kg/ha)",K2O="K\u2082O\n(kg/ha)",
               NC_V_tha="Calc. V%\n(t/ha)",NC_Al_tha="Calc. Al\n(t/ha)",Erro="Erro")
    cols <- intersect(names(nomes), names(df_res))
    if ("Erro" %in% cols && all(is.na(df_res$Erro))) cols <- setdiff(cols,"Erro")
    # Remove Município se todo NA
    if ("Municipio" %in% cols && all(is.na(df_res$Municipio))) cols <- setdiff(cols,"Municipio")
    df_show <- setNames(df_res[,cols,drop=FALSE], nomes[cols])
    n_erros <- sum(!is.na(df_res$Erro))
    div(class="result-card",
      div(class="result-card-title",
        HTML(paste0('<i class="bi bi-clipboard-check-fill"></i> Recomenda\u00e7\u00f5es \u2014 ',
                     nrow(df_show), ' amostras',
                     if(n_erros>0) paste0(' <span style="color:#e74c3c;">| ',n_erros,' com erro</span>') else ''))),
      div(style="overflow-x:auto;",
        datatable(df_show, rownames=FALSE, selection="none",
          class="stripe hover compact", extensions="Buttons",
          options=list(pageLength=20, dom="Bfrtip", scrollX=TRUE,
            buttons=list(
              list(extend="excel",text="Exportar Excel",
                   filename=paste0("recomendacoes_lote_",Sys.Date())),
              list(extend="csv",text="Exportar CSV",
                   filename=paste0("recomendacoes_lote_",Sys.Date()))
            ),
            language=list(paginate=list(previous="Anterior","next"="Pr\u00f3ximo"),
                          search="Buscar:")))
      )
    )
  })

  # --------------------------------------------------------------------------
  # DADOS CLIMÁTICOS — Open-Meteo
  # --------------------------------------------------------------------------
  output$clima_municipios_banco <- renderUI({
    df_reg <- rv_regional_df()
    if (is.null(df_reg)) return(NULL)
    coords <- extrair_coords_municipios(df_reg)
    if (is.null(coords) || nrow(coords) == 0) return(NULL)
    choices_mun <- setNames(paste0(coords$latitude, "__", coords$longitude), coords$municipio)
    div(class = "form-group-custom", style = "margin-bottom:8px;",
      label_custom("Usar munic\u00edpio do Banco Regional", "bi-pin-map"),
      selectInput("clima_municipio_banco", NULL, width = "100%",
        choices = c("Inserir coordenadas manualmente" = "", choices_mun))
    )
  })

  observeEvent(input$clima_municipio_banco, {
    req(nchar(input$clima_municipio_banco) > 0)
    partes <- strsplit(input$clima_municipio_banco, "__")[[1]]
    if (length(partes) == 2) {
      updateNumericInput(session, "clima_lat", value = as.numeric(partes[1]))
      updateNumericInput(session, "clima_lon", value = as.numeric(partes[2]))
    }
  })

  rv_clima_dados <- reactiveVal(NULL)

  observeEvent(input$btn_buscar_clima, {
    req(input$clima_lat, input$clima_lon)
    withProgress(message = "Buscando dados clim\u00e1ticos...", {
      res <- buscar_clima_municipio(
        lat    = input$clima_lat,
        lon    = input$clima_lon,
        inicio = format(input$clima_inicio, "%Y-%m-%d"),
        fim    = format(input$clima_fim,    "%Y-%m-%d")
      )
      if (!is.null(res$erro)) {
        showNotification(res$erro, type = "error", duration = 8)
        rv_clima_dados(NULL)
      } else {
        rv_clima_dados(res)
      }
    })
  })

  output$clima_resultado <- renderUI({
    res <- rv_clima_dados()
    if (is.null(res)) return(NULL)
    df  <- res$dados
    men <- resumo_mensal(df)

    chuva_total <- if ("chuva_mm"    %in% names(df)) round(sum(df$chuva_mm, na.rm=TRUE), 1) else NA
    temp_media  <- if ("temp_media_c"%in% names(df)) round(mean(df$temp_media_c, na.rm=TRUE), 1) else NA
    et0_total   <- if ("et0_mm"      %in% names(df)) round(sum(df$et0_mm, na.rm=TRUE), 1) else NA
    balanco_tot <- if (!is.na(chuva_total) && !is.na(et0_total)) chuva_total - et0_total else NA
    class_bal   <- if (!is.na(balanco_tot)) classificar_balanco(balanco_tot) else "N/D"

    tagList(
      div(class = "result-card",
        div(class = "result-card-title",
          HTML(paste0('<i class="bi bi-cloud-sun-fill"></i> Per\u00edodo ',
                       res$inicio, ' a ', res$fim,
                       ' <span class="stat-badge-n">Lat: ', res$lat, ' | Lon: ', res$lon, '</span>'))),
        div(class = "metrics-grid",
          mk_metric(if(!is.na(chuva_total)) paste0(chuva_total,"mm") else "N/D","","Chuva total","",    "#1A5276"),
          mk_metric(if(!is.na(temp_media))  paste0(temp_media,"\u00b0C") else "N/D","","Temp. m\u00e9dia","","#C0392B"),
          mk_metric(if(!is.na(et0_total))   paste0(et0_total,"mm") else "N/D","","ET\u2080 total","",  "#27AE60"),
          mk_metric(if(!is.na(balanco_tot)) paste0(balanco_tot,"mm") else "N/D","","Balan\u00e7o h\u00eddrico","","#B7791F")
        ),
        div(style = paste0("font-size:12px;margin-top:10px;padding:8px 12px;border-radius:6px;",
            if(!is.na(balanco_tot) && balanco_tot < 0) "background:#fff8e1;color:#7d6608;"
            else "background:#eafaf1;color:#1e8449;"),
          HTML(paste0('<i class="bi bi-info-circle-fill"></i> ', class_bal))
        )
      ),
      div(class = "result-card",
        div(class = "result-card-title", HTML('<i class="bi bi-bar-chart-fill"></i> Chuva & Temperatura Mensais')),
        withSpinner(plotlyOutput("clima_grafico", height = "360px"), type = 8, color = "#1A5276")
      ),
      div(class = "result-card",
        div(class = "result-card-title", HTML('<i class="bi bi-table"></i> Dados Mensais')),
        datatable(men, rownames=FALSE, selection="none",
          class="stripe hover compact",
          options=list(pageLength=12, dom="tip",
            language=list(paginate=list(previous="Anterior","next"="Pr\u00f3ximo"))))
      )
    )
  })

  output$clima_grafico <- renderPlotly({
    res <- rv_clima_dados()
    if (is.null(res)) return(plotly_vazio("Busque dados clim\u00e1ticos acima"))
    men <- resumo_mensal(res$dados)
    g <- plot_ly(men)
    if ("chuva_total_mm" %in% names(men)) {
      g <- add_trace(g, x=~mes, y=~chuva_total_mm, type="bar",
        name="Chuva (mm)", marker=list(color="#1A5276"),
        hovertemplate="%{x}<br>Chuva: %{y:.1f} mm<extra></extra>")
    }
    if ("temp_media_c" %in% names(men)) {
      g <- add_trace(g, x=~mes, y=~temp_media_c, type="scatter", mode="lines+markers",
        name="Temp. m\u00e9dia (\u00b0C)", yaxis="y2",
        line=list(color="#C0392B",width=2),
        marker=list(color="#C0392B",size=6),
        hovertemplate="%{x}<br>Temp: %{y:.1f}\u00b0C<extra></extra>")
    }
    layout(g,
      xaxis  = list(title="M\u00eas", tickangle=-30),
      yaxis  = list(title="Chuva (mm)", gridcolor="#eee"),
      yaxis2 = list(title="Temperatura (\u00b0C)", overlaying="y", side="right"),
      legend = list(orientation="h", x=0, y=-0.25),
      paper_bgcolor="transparent", plot_bgcolor="transparent",
      margin=list(t=10, b=60)
    )
  })

  # --------------------------------------------------------------------------
  # BOTÃO BUSCAR PREÇOS CEPEA
  # --------------------------------------------------------------------------
  # Feedback reativo
  rv_cepea_msg <- reactiveVal(NULL)
  
  output$cepea_feedback <- renderUI({
    msg <- rv_cepea_msg()
    if (is.null(msg)) return(NULL)
    div(class = paste("cepea-feedback", msg$cls),
      HTML(msg$texto)
    )
  })
  
  observeEvent(input$btn_cepea, {
    # Mostra loading
    rv_cepea_msg(list(
      cls   = "cepea-loading",
      texto = '<i class="bi bi-hourglass-split"></i> Consultando CEPEA/ESALQ e AgroLink... Aguarde.'
    ))
    
    # Desabilita botão durante consulta
    shinyjs_available <- requireNamespace("shinyjs", quietly = TRUE)
    
    # Executa scraping (pode demorar ~5-15s)
    resultado <- tryCatch(
      scrape_cepea(),
      error = function(e) list(
        sucesso  = FALSE,
        precos   = list(),
        mensagem = paste0("Erro inesperado: ", conditionMessage(e)),
        data     = format(Sys.Date(), "%d/%m/%Y"),
        fonte    = "—"
      )
    )
    
    if (resultado$sucesso && length(resultado$precos) > 0) {
      # Aplica os preços novos ao dataframe
      df_atual  <- rv_precos()
      df_novo   <- aplicar_precos_cepea(df_atual, resultado$precos, resultado$fonte)
      rv_precos(df_novo)
      
      # Atualiza status
      rv_preco_status(list(
        fonte    = resultado$fonte,
        data     = resultado$data,
        n_atualiz = length(resultado$precos)
      ))
      
      # Feedback positivo
      produtos_str <- paste(names(resultado$precos), collapse = ", ")
      rv_cepea_msg(list(
        cls   = "cepea-ok",
        texto = paste0(
          '<i class="bi bi-cloud-check-fill"></i> <b>', resultado$mensagem, '</b><br>',
          '<span style="font-size:11px;opacity:0.8">Produtos: ', produtos_str, '</span><br>',
          '<span style="font-size:11px;opacity:0.7">Clique em <b>Salvar preços</b> para persistir os valores no arquivo local.</span>'
        )
      ))
    } else {
      rv_cepea_msg(list(
        cls   = "cepea-erro",
        texto = paste0(
          '<i class="bi bi-exclamation-triangle-fill"></i> ',
          resultado$mensagem,
          '<br><span style="font-size:11px;margin-top:6px;display:block;opacity:0.85;">',
          '<b>O que fazer:</b> ',
          'Os sites CEPEA e AgroLink usam JavaScript din\u00e2mico que bloqueiam consultas autom\u00e1ticas. ',
          'Acesse <a href="https://www.cepea.esalq.usp.br/br/indicador/insumos-agropecuarios.aspx" ',
          'target="_blank" style="color:#1a5276;">CEPEA/ESALQ</a> ou ',
          '<a href="https://www.agrolink.com.br/cotacoes/insumos" target="_blank" style="color:#1a5276;">AgroLink</a> ',
          'diretamente no navegador, copie os pre\u00e7os e edite a tabela abaixo manualmente.',
          '</span>'
        )
      ))
    }
  })
  
  # --------------------------------------------------------------------------
  # BOTÃO MENOR PREÇO BRASIL — exibe painel com links de busca
  # --------------------------------------------------------------------------
  rv_menor_preco_aberto <- reactiveVal(FALSE)

  observeEvent(input$btn_menor_preco_modal, {
    rv_menor_preco_aberto(!rv_menor_preco_aberto())
  })

  output$menor_preco_painel <- renderUI({
    if (!rv_menor_preco_aberto()) return(NULL)

    div(class = "menor-preco-panel",
      div(class = "mp-title",
        HTML('<i class="bi bi-search"></i> Consultar pre\u00e7os de fertilizantes')
      ),
      div(class = "mp-links",
        tags$a(class = "mp-link",
          href   = "https://play.google.com/store/search?q=menor+pre%C3%A7o+brasil&c=apps",
          target = "_blank",
          HTML('<i class="bi bi-phone-fill"></i> Menor Pre\u00e7o Brasil (app)')),
        tags$a(class = "mp-link",
          href   = "https://www.cepea.esalq.usp.br/br/indicador/insumos-agropecuarios.aspx",
          target = "_blank",
          style  = "color:#1a5276; border-color:#aed6f1;",
          HTML('<i class="bi bi-bar-chart-fill"></i> CEPEA/ESALQ')),
        tags$a(class = "mp-link",
          href   = "https://www.agrolink.com.br/cotacoes/insumos",
          target = "_blank",
          style  = "color:#27ae60; border-color:#a9dfbf;",
          HTML('<i class="bi bi-graph-up"></i> AgroLink'))
      ),
      p(style = "font-size:11px; color:#888; margin-top:8px; margin-bottom:0;",
        HTML('<i class="bi bi-info-circle"></i> Consulte o pre\u00e7o na fonte e insira na tabela abaixo. Clique em <b>Salvar pre\u00e7os</b> para persistir.')
      )
    )
  })
  dados_solo <- reactiveVal(NULL)
  dados_rec  <- reactiveVal(NULL)

  # --------------------------------------------------------------------------
  # CÁLCULO PRINCIPAL
  # --------------------------------------------------------------------------
  observeEvent(input$calcular, {
    
    req(input$ph, input$p, input$k, input$ca, input$mg, input$al, input$h_al)
    # MO, Argila e P-rem são opcionais
    # Argila é obrigatória apenas se P-rem também não foi informado
    has_argila <- !is.na(input$argila) && !is.null(input$argila)
    has_p_rem  <- !is.na(input$p_rem)  && !is.null(input$p_rem)
    if (!has_argila && !has_p_rem) {
      showNotification(
        "Informe a Argila (%) ou o P-rem (mg/L) \u2014 ao menos um \u00e9 necess\u00e1rio para interpretar o f\u00f3sforo.",
        type = "warning", duration = 6
      )
      return(NULL)
    }
    
    withProgress(message = "Calculando recomendações...", {
      
      # 1. Análise do solo
      solo <- analisar_solo(
        ph     = input$ph,
        mo     = if (is.na(input$mo) || is.null(input$mo)) NA_real_ else input$mo,
        p      = input$p,
        k      = input$k,
        ca     = input$ca,
        mg     = input$mg,
        al     = input$al,
        h_al   = input$h_al,
        argila = if (is.na(input$argila) || is.null(input$argila)) NA_real_ else input$argila,
        p_rem  = if (is.na(input$p_rem) || is.null(input$p_rem)) NA_real_ else input$p_rem,
        s     = if (is.na(input$s)) NA else input$s,
        b     = if (is.na(input$b)) NA else input$b,
        cu    = if (is.na(input$cu)) NA else input$cu,
        fe    = if (is.na(input$fe)) NA else input$fe,
        mn    = if (is.na(input$mn)) NA else input$mn,
        zn    = if (is.na(input$zn)) NA else input$zn
      )
      dados_solo(solo)
      
      # 2. Calagem
      manual_sel  <- input$manual
      cultura_sel <- input$cultura
      v_alvo      <- v_alvo_cultura[[cultura_sel]][[manual_sel]]
      
      calagen_resultados <- calcular_todas_calagen(
        v_atual  = solo$v$valor,
        ctc      = solo$ctc$valor,
        al       = input$al,
        ca       = input$ca,
        mg       = input$mg,
        ph_smp   = if (is.na(input$ph_smp)) NULL else input$ph_smp,
        v_alvo   = v_alvo,
        prnt     = input$prnt
      )
      
      gesso_rec <- gessagem(
        argila  = if (is.na(input$argila) || is.null(input$argila)) 30 else input$argila,
        al_sup  = input$al,
        ca_sup  = input$ca,
        ca_sub  = if (is.na(input$ca_sub)) NA else input$ca_sub,
        mg_sub  = if (is.na(input$mg_sub)) NA else input$mg_sub,
        al_sub  = if (is.na(input$al_sub)) NA else input$al_sub,
        k_sub   = if (is.na(input$k_sub))  NA else input$k_sub
      )

      # 3. Adubação — usa argila_efetiva do solo (já considera P-rem se informado)
      p_interp <- solo$p$interp  # já calculado com argila_efetiva
      k_interp <- interpretar_k(input$k, solo$ctc$valor)
      
      adub_rec <- recomendar_adubacao(
        cultura      = cultura_sel,
        p_nivel      = p_interp$nivel,
        k_nivel      = k_interp$nivel,
        produtividade = input$produtividade,
        mo           = if (is.na(input$mo) || is.null(input$mo)) NA_real_ else input$mo,
        fase         = input$fase,
        n_anterior   = input$n_anterior
      )

      # Ajuste pelo nível tecnológico
      nivel_res <- rv_nivel_tec()
      adub_rec  <- ajustar_doses_nivel(adub_rec, nivel_res$nivel)

      # 4. Custo - usar tabela comparativa com preços reativos
      tabela_custo <- tabela_comparativa(
        dose_n    = adub_rec$N_plantio + adub_rec$N_cobertura,
        dose_p    = adub_rec$P2O5,
        dose_k    = adub_rec$K2O,
        area      = input$area,
        df_fontes = rv_precos()
      )

      dados_rec(list(
        solo      = solo,
        calagem   = calagen_resultados,
        gesso     = gesso_rec,
        adubacao  = adub_rec,
        custos    = tabela_custo,
        v_alvo    = v_alvo,
        cultura   = cultura_sel,
        manual    = manual_sel,
        area      = input$area,
        nivel_tec = nivel_res   # salva para o relatório
      ))
      
      setProgress(1, message = "Pronto!")
    })
  })
  
  # --------------------------------------------------------------------------
  # RESULTADO ANÁLISE DE SOLO
  # --------------------------------------------------------------------------
  output$resultado_solo <- renderUI({
    rec <- dados_rec()
    if (is.null(rec)) return(placeholder_ui("layers", "Fertilidade do Solo", "Insira os dados da análise e clique em Calcular para ver o diagnóstico de fertilidade."))

    solo <- rec$solo

    tagList(
      # Parâmetros calculados
      div(class = "result-card",
        div(class = "result-card-title", HTML('<i class="bi bi-calculator"></i> Par\u00e2metros Calculados')),
        div(class = "metrics-grid",
          mk_metric(solo$ctc$valor, "cmolc/dm\u00b3", "CTC", "Calculado", "#333"),
          mk_metric(solo$sb$valor,  "cmolc/dm\u00b3", "SB",  "Calculado", "#333"),
          mk_metric(paste0(solo$v$valor, "%"), "", "V%", solo$v$interp$classe, solo$v$interp$cor),
          mk_metric(paste0(solo$m$valor, "%"), "", "m%", solo$m$interp$classe, solo$m$interp$cor)
        )
      ),
      # pH e MO
      div(class = "result-card",
        div(class = "result-card-title", HTML('<i class="bi bi-droplet"></i> Rea\u00e7\u00e3o & M.O.')),
        div(class = "metrics-grid",
          mk_metric(solo$ph$valor, "H\u2082O", "pH", solo$ph$interp$classe, solo$ph$interp$cor),
          if (!is.null(solo$mo)) {
            mk_metric(solo$mo$valor, "dag/kg", "M.O.", solo$mo$interp$classe, solo$mo$interp$cor)
          } else {
            div(class = "metric-item",
              div(class = "metric-value", style = "font-size:14px;color:#bbb;", "\u2014"),
              div(class = "metric-unit", ""),
              div(class = "metric-name", "M.O."),
              span(class = "metric-class", style = "background:#f5f5f5;color:#aaa;", "N/I")
            )
          }
        ),
        if (is.null(solo$mo)) {
          div(style = "font-size:11px;color:#e67e22;margin-top:6px;",
            HTML('<i class="bi bi-exclamation-circle"></i> M.O. n\u00e3o informada \u2014 desconto de N por M.O. alta n\u00e3o aplicado.')
          )
        }
      ),
      # Acidez
      div(class = "result-card",
        div(class = "result-card-title", HTML('<i class="bi bi-exclamation-triangle"></i> Acidez do Solo')),
        div(class = "metrics-grid",
          mk_metric(solo$al$valor,   "cmolc/dm\u00b3", "Al\u00b3\u207a", if (solo$al$valor > 0.5) "Alto" else "Baixo", "#333"),
          mk_metric(solo$h_al$valor, "cmolc/dm\u00b3", "H+Al", "Acid. Pot.", "#333")
        )
      ),
      # Macronutrientes
      div(class = "result-card",
        div(class = "result-card-title", HTML('<i class="bi bi-grid-3x3"></i> Macronutrientes')),
        div(class = "metrics-grid",
          mk_metric(solo$ca$valor, "cmolc/dm\u00b3", "Ca\u00b2\u207a", solo$ca$interp$classe, solo$ca$interp$cor),
          mk_metric(solo$mg$valor, "cmolc/dm\u00b3", "Mg\u00b2\u207a", solo$mg$interp$classe, solo$mg$interp$cor),
          mk_metric(solo$k$valor,  "mg/dm\u00b3",    "K\u207a",      solo$k$interp$classe,  solo$k$interp$cor),
          mk_metric(solo$p$valor,  "mg/dm\u00b3",    "P-Mehlich",   solo$p$interp$classe,  solo$p$interp$cor)
        ),
        if (isTRUE(solo$p$usou_p_rem)) {
          div(style = "font-size:11px;color:#1a5276;margin-top:6px;",
            HTML(paste0('<i class="bi bi-info-circle-fill"></i> P interpretado via <b>P-rem = ',
              solo$p$p_rem, ' mg/L</b> (argila equiv. \u2248 ', solo$p$argila_efetiva, '%).'))
          )
        } else if (isTRUE(solo$p$usou_fallback)) {
          div(style = "font-size:11px;color:#e67e22;margin-top:6px;",
            HTML('<i class="bi bi-exclamation-triangle-fill"></i> Argila e P-rem n\u00e3o informados \u2014 ',
                 'P interpretado com textura m\u00e9dia (~30% argila). ',
                 'Informe Argila ou P-rem para maior precis\u00e3o.')
          )
        } else {
          div(style = "font-size:10px;color:#ccc;margin-top:4px;",
            HTML('<i class="bi bi-lightbulb"></i> Informe o P-rem para interpreta\u00e7\u00e3o mais precisa.')
          )
        }
      ),
      # Micronutrientes (se houver)
      if (!all(sapply(solo$micro, is.null))) {
        div(class = "result-card",
          div(class = "result-card-title", HTML('<i class="bi bi-atom"></i> Micronutrientes')),
          div(class = "metrics-grid",
            lapply(names(solo$micro), function(nm) {
              m <- solo$micro[[nm]]
              if (!is.null(m)) {
                mk_metric(m$valor, "mg/dm\u00b3", nm, m$interp$classe, m$interp$cor)
              }
            })
          )
        )
      },

      # ── Botão Relatório ─────────────────────────────────────────────────
      div(class = "btn-container",
        downloadButton("btn_download_relatorio",
          label  = HTML('<i class="bi bi-file-earmark-text"></i>&nbsp; GERAR RELAT\u00d3RIO COMPLETO (PDF/HTML)'),
          class  = "btn-calcular",
          style  = "width:100%;background:#1A5276;border-color:#1A5276;"
        )
      )
    )
  })

  # --------------------------------------------------------------------------
  # DOWNLOAD DO RELATÓRIO HTML
  # --------------------------------------------------------------------------
  output$btn_download_relatorio <- downloadHandler(
    filename = function() {
      paste0("relatorio_fertilidade_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      rec  <- dados_rec()
      if (is.null(rec)) { writeLines("Sem dados.", file); return() }

      fenol <- if (rec$cultura == "milho")
        fenologia_milho(rec$adubacao$N_cobertura, rec$adubacao$N_plantio)
      else NULL

      html <- gerar_relatorio_html(
        solo          = rec$solo,
        rec           = rec$adubacao,
        cal_result    = rec$calagem,
        gesso_result  = rec$gessagem,
        cultura       = rec$cultura,
        produtividade = input$produtividade,
        prnt          = input$prnt,
        info_produtor = list(
          nome        = input$info_produtor    %||% "",
          propriedade = input$info_propriedade %||% "",
          municipio   = input$info_municipio   %||% "",
          amostra     = input$info_amostra     %||% ""
        ),
        fenol     = fenol,
        nivel_tec = rec$nivel_tec
      )
      writeLines(html, file, useBytes = TRUE)
    }
  )

  # --------------------------------------------------------------------------
  # RESULTADO CALAGEM
  # --------------------------------------------------------------------------
  output$resultado_calagem <- renderUI({
    rec <- dados_rec()
    if (is.null(rec)) return(placeholder_ui("droplet-fill", "Calagem & Gessagem", "Calcule primeiro para ver as recomendações de calagem."))
    
    cal  <- rec$calagem
    ges  <- rec$gesso
    valvo <- rec$v_alvo
    vsolo <- rec$solo$v$valor
    
    # Método principal selecionado
    met_sel <- input$metodo_calagem
    
    nc_principal <- if (met_sel == "todos") {
      mean(unlist(cal))
    } else if (met_sel == "v") {
      cal[["V% (5ª Aprox. MG)"]]
    } else if (met_sel == "al") {
      cal[["Al³⁺ + Ca+Mg"]]
    } else {
      cal[["Tampão SMP"]] %||% cal[[1]]
    }
    
    if (is.null(nc_principal)) nc_principal <- cal[[1]]
    
    precisa_calagem <- nc_principal > 0.1
    
    tagList(
      # Alerta principal
      div(class = paste("calagem-alerta", if (precisa_calagem) "calagem-necessaria" else "calagem-ok"),
        div(class = "calagem-icon", if (precisa_calagem) "⚠️" else "✅"),
        div(
          div(class = "calagem-titulo",
            if (precisa_calagem) paste0("Calagem Necessária: ", nc_principal, " t/ha")
            else "Solo com pH adequado - Calagem não necessária"
          ),
          div(class = "calagem-desc",
            paste0("V% atual: ", vsolo, "% → V% alvo (", rec$manual %>% toupper(), "): ", valvo, "% | PRNT do calcário: ", input$prnt, "%")
          )
        )
      ),
      
      # Comparação de métodos
      if (length(cal) > 0) {
        div(class = "result-card",
          div(class = "result-card-title", HTML('<i class="bi bi-sliders"></i> Comparação entre Métodos de Calagem')),
          div(class = "metodos-grid",
            lapply(names(cal), function(nm) {
              dose <- cal[[nm]]
              ativo <- (met_sel == "todos") || 
                (met_sel == "v"   && nm == "V% (5ª Aprox. MG)") ||
                (met_sel == "al"  && nm == "Al³⁺ + Ca+Mg") ||
                (met_sel == "smp" && nm == "Tampão SMP")
              
              div(class = paste("metodo-card", if (ativo) "destaque"),
                div(class = "metodo-nome", nm),
                div(class = "metodo-dose", dose),
                div(class = "metodo-unidade", "t/ha de calcário")
              )
            })
          ),
          p(style = "font-size:11px; color:#888; margin-top:12px; padding-top:10px; border-top:1px solid #eee;",
            HTML('<i class="bi bi-info-circle"></i> Os métodos podem gerar recomendações diferentes. O método por V% é o padrão da 5ª Aproximação de MG. Para o Nordeste, o método por Al³⁺ é amplamente utilizado.'))
        )
      },
      
      # Parcelamento
      if (precisa_calagem) {
        div(class = "result-card",
          div(class = "result-card-title", HTML('<i class="bi bi-calendar3"></i> Orientação de Aplicação')),
          tags$ul(style = "padding-left: 18px; font-size: 13px; line-height: 1.8; color: #444;",
            tags$li(HTML(paste0("<b>Dose total:</b> ", nc_principal, " t/ha de calcário (PRNT ", input$prnt, "%)"))),
            tags$li(HTML(paste0("<b>Parcelamento recomendado:</b>",
              if (nc_principal > 3) " 2 a 3 aplicações em anos consecutivos"
              else if (nc_principal > 1.5) " Aplicação única ou em 2 vezes"
              else " Aplicação única"))),
            tags$li(HTML("<b>Incorporação:</b> Mínimo 60 dias antes do plantio (gradagem/aração)")),
            tags$li(HTML("<b>Tipo de calcário:</b> Calcário Dolomítico preferencial (Ca + Mg)"))
          )
        )
      },
      
      # Gessagem
      div(class = "result-card",
        div(class = "result-card-title",
          HTML('<i class="bi bi-layers"></i> Gessagem Agr\u00edcola')),

        # Alerta de necessidade
        div(class = paste("calagem-alerta",
              if (ges$necessidade) "calagem-necessaria" else "calagem-ok"),
          style = "margin-bottom: 12px;",
          div(class = "calagem-icon",
              if (ges$necessidade) "\U0001F4E6" else "\u2705"),
          div(
            div(class = "calagem-titulo",
              if (ges$necessidade) "Gessagem recomendada"
              else "Gessagem n\u00e3o indicada"),
            div(class = "calagem-desc",
              HTML(paste0("<b>Crit\u00e9rio(s):</b> ", ges$justificativa)))
          )
        ),

        # Comparativo dos três métodos
        if (ges$necessidade) {
          tagList(
            div(class = "metodos-grid",

              # Método 1a — Argila (anual)
              div(class = "metodo-card",
                div(class = "metodo-nome",
                    "Textura — culturas anuais"),
                div(class = "metodo-dose",
                    if (!is.na(ges$dose_argila_anual)) ges$dose_argila_anual else "—"),
                div(class = "metodo-unidade", "t ha\u207b\u00b9 de gesso"),
                div(style = "font-size:9px; color:#aaa; margin-top:3px;",
                    "Sousa & Lobato, 2004")
              ),

              # Método 1b — Argila (perene)
              div(class = "metodo-card",
                div(class = "metodo-nome",
                    "Textura — culturas perenes"),
                div(class = "metodo-dose",
                    if (!is.na(ges$dose_argila_perene)) ges$dose_argila_perene else "—"),
                div(class = "metodo-unidade", "t ha\u207b\u00b9 de gesso"),
                div(style = "font-size:9px; color:#aaa; margin-top:3px;",
                    "Sousa & Lobato, 2004")
              ),

              # Método 2 — V% subsolo
              div(class = if (!is.na(ges$dose_v_sub)) "metodo-card" else "metodo-card",
                style = if (is.na(ges$dose_v_sub)) "opacity:0.5;" else "",
                div(class = "metodo-nome", HTML("V% subsolo (20\u201340 cm)")),
                div(class = "metodo-dose",
                    if (!is.na(ges$dose_v_sub)) ges$dose_v_sub else "—"),
                div(class = "metodo-unidade",
                    if (!is.na(ges$dose_v_sub)) "t ha\u207b\u00b9 de gesso"
                    else "informe dados 20\u201340 cm"),
                div(style = "font-size:9px; color:#aaa; margin-top:3px;",
                    if (!is.na(ges$v_sub))
                      paste0("V% subsolo = ", ges$v_sub, "%")
                    else "Demat\u00ea/Vitti, 2008")
              ),

              # Método 3 — Caires & Guimarães (destaque)
              div(class = paste("metodo-card",
                    if (!is.na(ges$dose_caires)) "destaque" else ""),
                style = if (is.na(ges$dose_caires)) "opacity:0.5;" else
                  "border-color:#1abc9c; background:#eafaf1;",
                div(class = "metodo-nome",
                    HTML("Sat. Ca\u00b2\u207a CTCef \u2014 \u2605 Recomendado")),
                div(class = "metodo-dose",
                    style = "color:#16a085;",
                    if (!is.na(ges$dose_caires)) ges$dose_caires else "—"),
                div(class = "metodo-unidade",
                    if (!is.na(ges$dose_caires)) "t ha\u207b\u00b9 de gesso"
                    else "informe dados 20\u201340 cm"),
                div(style = "font-size:9px; color:#1abc9c; margin-top:3px;",
                    if (!is.na(ges$sat_ca))
                      paste0("Sat. Ca = ", ges$sat_ca,
                             "% | CTCef = ", round(ges$ctcef_sub, 2),
                             " cmol\u1d04 dm\u207b\u00b3")
                    else "Caires & Guimar\u00e3es, 2018")
              )
            ),

            # Notas técnicas
            div(style = "margin-top:12px; padding-top:10px;
                         border-top:1px solid #eee; font-size:11px; color:#888;",
              HTML(paste0(
                '<i class="bi bi-info-circle"></i> ',
                '<b>Caires & Guimar\u00e3es (2018):</b> m\u00e9todo mais preciso ',
                'pois considera a natureza da argila via CTCef. ',
                'Use somente quando sat. Ca\u00b2\u207a < 54% na camada 20\u201340 cm. ',
                if (!is.na(ges$dose_caires) && is.na(ges$dose_argila_anual) == FALSE &&
                    ges$dose_caires > ges$dose_argila_anual * 2)
                  paste0('<br><b style="color:#e67e22;">\u26a0 Aten\u00e7\u00e3o: ',
                         'a nova f\u00f3rmula recomenda dose significativamente maior (',
                         ges$dose_caires, ' vs ', ges$dose_argila_anual,
                         ' t/ha). Isso \u00e9 esperado em solos com subsolo pobre em Ca\u00b2\u207a.</b>')
                  else '',
                '<br>Gesso n\u00e3o corrige acidez (pH) \u2014 aplique ',
                '<b>sempre junto ou ap\u00f3s a calagem</b>. ',
                'Doses excessivas podem lixiviar Mg\u00b2\u207a e K\u207a.'
              ))
            )
          )
        }
      )
    )
  })
  
  # --------------------------------------------------------------------------
  # RESULTADO ADUBAÇÃO
  # --------------------------------------------------------------------------
  output$resultado_adubacao <- renderUI({
    rec <- dados_rec()
    if (is.null(rec)) return(placeholder_ui("bag-fill", "Adubação", "Calcule primeiro para ver a recomendação de adubação."))
    
    adub  <- rec$adubacao
    cult  <- rec$cultura
    man   <- rec$manual
    n_tot <- adub$N_plantio + adub$N_cobertura
    
    tagList(
      # Cabeçalho cultura
      div(class = "result-card",
        style = "background: linear-gradient(135deg, #1b4332, #2d6a4f); color: white; margin-bottom: 16px;",
        div(class = "result-card-title", style = "color: rgba(255,255,255,0.7); border-bottom-color: rgba(255,255,255,0.15);",
          HTML(paste0('<i class="bi bi-seedling"></i> ', culturas_lista[[cult]], " | Manual: ", toupper(man),
                       " | N\u00edvel: <span style='color:",
                       rec$nivel_tec$cor %||% "#aaa", ";font-weight:700;'>",
                       rec$nivel_tec$nivel_label %||% "M\u00e9dia Tecnologia", "</span>"))
        ),
        div(class = "npk-display",
          div(class = "npk-card npk-n",
            div(class = "npk-label", "N — Nitrogênio Total"),
            div(class = "npk-value", n_tot),
            div(class = "npk-unit", HTML("kg N ha<sup>-1</sup>"))
          ),
          div(class = "npk-card npk-p",
            div(class = "npk-label", HTML("P<sub>2</sub>O<sub>5</sub> — Fósforo")),
            div(class = "npk-value", adub$P2O5),
            div(class = "npk-unit", HTML("kg P<sub>2</sub>O<sub>5</sub> ha<sup>-1</sup>"))
          ),
          div(class = "npk-card npk-k",
            div(class = "npk-label", HTML("K<sub>2</sub>O — Potássio")),
            div(class = "npk-value", adub$K2O),
            div(class = "npk-unit", HTML("kg K<sub>2</sub>O ha<sup>-1</sup>"))
          )
        )
      ),
      
      # Detalhamento N
      div(class = "result-card",
        div(class = "result-card-title", HTML('<i class="bi bi-card-list"></i> Detalhamento por Época de Aplicação')),
        tags$table(class = "table-financeira",
          tags$thead(
            tags$tr(
              tags$th("Nutriente"),
              tags$th("Plantio/Fundação"),
              tags$th("Cobertura/Soca"),
              tags$th("Total"),
              tags$th("Unidade")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(HTML("<b>N — Nitrogênio</b>")),
              tags$td(adub$N_plantio),
              tags$td(adub$N_cobertura),
              tags$td(HTML(paste0("<b>", n_tot, "</b>"))),
              tags$td("kg ha<sup>-1</sup>")
            ),
            tags$tr(
              tags$td(HTML("<b>P<sub>2</sub>O<sub>5</sub> — Fósforo</b>")),
              tags$td(adub$P2O5),
              tags$td("—"),
              tags$td(HTML(paste0("<b>", adub$P2O5, "</b>"))),
              tags$td("kg ha<sup>-1</sup>")
            ),
            tags$tr(
              tags$td(HTML("<b>K<sub>2</sub>O — Potássio</b>")),
              tags$td(round(adub$K2O * 0.5)),
              tags$td(round(adub$K2O * 0.5)),
              tags$td(HTML(paste0("<b>", adub$K2O, "</b>"))),
              tags$td("kg ha<sup>-1</sup>")
            )
          )
        ),
        p(style = "font-size:11px; color:#888; margin-top:10px;",
          HTML(paste0(
            '<i class="bi bi-info-circle"></i> ',
            if (rec$cultura == "abacaxi") {
              "Abacaxi: N parcelado em 3–4 coberturas via foliar ou solo (meses 2, 4, 6, 8 após plantio). K\u2082O: nutriente mais exigido pela cultura — aplicar parcelado junto ao N. P\u2082O\u2085: todo no plantio incorporado."
            } else if (rec$cultura == "amendoim") {
              "Amendoim realiza fixa\u00e7\u00e3o biol\u00f3gica de N\u2082 — N em cobertura somente em solos muito pobres. Aten\u00e7\u00e3o ao c\u00e1lcio (Ca): aplicar calcário dolomítico com antecedência."
            } else if (rec$cultura == "cana") {
              "Cana-de-a\u00e7\u00facar: parcelar N em 2 aplicações (plantio + 60 dias). K\u2082O em soca: aplicar ap\u00f3s cada corte. Considerar vinhaça como fonte de K e matéria orgânica."
            } else {
              "Recomenda\u00e7\u00f5es baseadas na interpreta\u00e7\u00e3o de P e K do solo e na produtividade esperada. N: aplicar 1/3 no plantio + 2/3 em cobertura (exceto culturas fixadoras de N<sub>2</sub>)."
            }
          ))
        )
      ),
      
      # Área total
      if (!is.null(rec$area) && rec$area > 1) {
        div(class = "result-card",
          div(class = "result-card-title", HTML('<i class="bi bi-map"></i> Quantidades para a Área Total')),
          tags$table(class = "table-financeira",
            tags$thead(
              tags$tr(
                tags$th("Nutriente"),
                tags$th(paste0("Total (", rec$area, " ha)")),
                tags$th("Unidade")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(HTML("<b>N</b>")),
                tags$td(n_tot * rec$area),
                tags$td("kg")
              ),
              tags$tr(
                tags$td(HTML("<b>P<sub>2</sub>O<sub>5</sub></b>")),
                tags$td(adub$P2O5 * rec$area),
                tags$td("kg")
              ),
              tags$tr(
                tags$td(HTML("<b>K<sub>2</sub>O</b>")),
                tags$td(adub$K2O * rec$area),
                tags$td("kg")
              )
            )
          )
        )
      }
    )
  })
  
  # --------------------------------------------------------------------------
  # RESULTADO FINANCEIRO
  # --------------------------------------------------------------------------
  output$resultado_financeiro <- renderUI({
    rec <- dados_rec()
    if (is.null(rec)) return(placeholder_ui("currency-dollar", "Análise de Custos", "Calcule primeiro para ver a análise financeira."))
    
    df <- rec$custos
    if (is.null(df) || nrow(df) == 0) {
      return(div(class = "placeholder-msg",
        div(class = "placeholder-icon", "💰"),
        h4("Sem dados financeiros disponíveis")
      ))
    }
    
    # Melhor opção por nutriente
    best <- df %>%
      group_by(Nutriente) %>%
      slice_min(Custo_ha, n = 1, with_ties = FALSE) %>%
      pull(Produto)
    
    tagList(
      div(class = "result-card",
        div(class = "result-card-title", HTML('<i class="bi bi-currency-dollar"></i> Comparativo de Custo por Produto Comercial')),
        p(style = "font-size:11.5px; color:#888; margin-bottom:12px;",
          HTML('<i class="bi bi-info-circle"></i> Preços em R$/kg de produto comercial. Use a aba <b>Custos → tabela de preços</b> para editar ou buscar via CEPEA. Recalcule após atualizar os preços.')),
        
        # Por nutriente
        lapply(c("N", "P2O5", "K2O"), function(nut) {
          sub_df <- df[df$Nutriente == nut, ]
          if (nrow(sub_df) == 0) return(NULL)
          
          nome_nut <- switch(nut,
            "N"    = "Nitrogênio (N)",
            "P2O5" = HTML("Fósforo (P<sub>2</sub>O<sub>5</sub>)"),
            "K2O"  = HTML("Potássio (K<sub>2</sub>O)")
          )
          
          div(style = "margin-bottom: 20px;",
            h5(style = "font-size:13px; font-weight:700; color:#2d6a4f; margin-bottom:8px; padding-bottom:6px; border-bottom:2px solid #d8f3dc;",
              nome_nut),
            tags$table(class = "table-financeira", style = "width:100%;",
              tags$thead(
                tags$tr(
                  tags$th("Produto Comercial"),
                  tags$th("Teor"),
                  tags$th(HTML("kg ha<sup>-1</sup>")),
                  tags$th(HTML("R$ kg<sup>-1</sup>")),
                  tags$th(HTML("Custo ha<sup>-1</sup>")),
                  tags$th(paste0("Total (", rec$area, " ha)")),
                  tags$th("Fonte / Data")
                )
              ),
              tags$tbody(
                lapply(seq_len(nrow(sub_df)), function(i) {
                  row    <- sub_df[i, ]
                  melhor <- row$Produto %in% best
                  
                  # Ícone de fonte
                  fonte_icone <- if (grepl("CEPEA|AgroLink", row$Fonte %||% "", ignore.case = TRUE)) {
                    HTML('<span style="color:#1abc9c;font-weight:700;font-size:10px;"><i class="bi bi-cloud-check"></i> CEPEA</span>')
                  } else if (grepl("Manual", row$Fonte %||% "", ignore.case = TRUE)) {
                    HTML('<span style="color:#e67e22;font-size:10px;"><i class="bi bi-pencil"></i> Manual</span>')
                  } else {
                    HTML('<span style="color:#95a5a6;font-size:10px;"><i class="bi bi-hdd"></i> Local</span>')
                  }
                  
                  tags$tr(class = if (melhor) "melhor-opcao" else "",
                    tags$td(HTML(paste0(
                      row$Produto,
                      if (melhor) '<span class="melhor-preco-badge">✓ Mais barato</span>'
                    ))),
                    tags$td(row$Teor_pct),
                    tags$td(row$Dose_kg_ha),
                    tags$td(paste0("R$ ", fmt_brl(row$Preco_kg))),
                    tags$td(HTML(paste0("<b>R$ ", fmt_brl(row$Custo_ha), "</b>"))),
                    tags$td(HTML(paste0("<b>R$ ", fmt_brl(row$Custo_total), "</b>"))),
                    tags$td(HTML(paste0(fonte_icone, '<br><span style="font-size:9px;color:#aaa;">', row$Data_ref %||% "—", "</span>")))
                  )
                })
              )
            )
          )
        })
      ),
      
      # Custo mínimo total
      div(class = "result-card",
        style = "background: var(--verde-palido); border: 2px solid var(--verde-claro);",
        div(class = "result-card-title", HTML('<i class="bi bi-trophy"></i> Menor Custo Total (Melhor Combinação)')),
        div(style = "display: flex; gap: 20px; align-items: center; flex-wrap: wrap;",
          lapply(c("N", "P2O5", "K2O"), function(nut) {
            sub_df <- df[df$Nutriente == nut, ]
            if (nrow(sub_df) == 0) return(NULL)
            best_row <- sub_df[which.min(sub_df$Custo_ha), ]
            
            div(style = "flex: 1; min-width: 150px; background: white; border-radius: 10px; padding: 12px; text-align: center;",
              div(style = "font-size:10px; color:#888; text-transform:uppercase; letter-spacing:0.5px;",
                HTML(switch(nut, "N" = "Nitrogênio (N)", "P2O5" = "Fósforo (P<sub>2</sub>O<sub>5</sub>)", "K2O" = "Potássio (K<sub>2</sub>O)"))),
              div(style = "font-family:'DM Serif Display'; font-size:20px; color:#1b4332; margin: 4px 0;",
                paste0("R$ ", fmt_brl(best_row$Custo_ha))),
              div(style = "font-size:11px; color:#555;", best_row$Produto),
              div(style = "font-size:10px; color:#888;", paste0("/ha"))
            )
          }),
          div(style = "flex: 1; min-width: 150px; background: var(--verde-escuro); color: white; border-radius: 10px; padding: 12px; text-align: center;",
            div(style = "font-size:10px; opacity:0.8; text-transform:uppercase; letter-spacing:0.5px;", "Custo Total NPK/ha"),
            div(style = "font-family:'DM Serif Display'; font-size:24px; margin: 4px 0;", {
              total <- sum(sapply(c("N", "P2O5", "K2O"), function(nut) {
                sub_df <- df[df$Nutriente == nut, ]
                if (nrow(sub_df) == 0) return(0)
                min(sub_df$Custo_ha)
              }))
              paste0("R$ ", fmt_brl(total))
            }),
            div(style = "font-size:10px; opacity:0.75;", paste0("Área: ", rec$area, " ha"))
          )
        )
      )
    )
  })
  
  # --------------------------------------------------------------------------
  # GRÁFICOS
  # --------------------------------------------------------------------------
  
  # GRÁFICO RADAR - Fertilidade geral
  output$grafico_radar <- renderPlotly({
    rec <- dados_rec()
    if (is.null(rec)) return(plotly_vazio("Aguardando cálculo..."))
    
    solo <- rec$solo
    
    # Normalizar valores 0-100 (% do valor ótimo)
    normalizar <- function(val, min_v, max_v, inv = FALSE) {
      n <- (val - min_v) / (max_v - min_v) * 100
      n <- pmax(0, pmin(100, n))
      if (inv) n <- 100 - n
      n
    }
    
    # pH: ótimo ~6.0, ruim <4.5 ou >7.5
    ph_norm <- 100 - abs(solo$ph$valor - 6.0) / 2.5 * 100
    ph_norm <- pmax(0, pmin(100, ph_norm))
    
    # Plotly não suporta HTML em theta — usar Unicode
    cats <- c(
      "pH (H\u2082O)",
      "M.O.\n(dag kg\u207b\u00b9)",
      "P-Mehlich\n(mg dm\u207b\u00b3)",
      "K\u207a\n(mg dm\u207b\u00b3)",
      "Ca\u00b2\u207a\n(cmol\u1d04 dm\u207b\u00b3)",
      "Mg\u00b2\u207a\n(cmol\u1d04 dm\u207b\u00b3)",
      "V%"
    )
    
    vals <- c(
      ph_norm,
      normalizar(if (!is.null(solo$mo)) solo$mo$valor else 2.0, 0, 5),
      normalizar(solo$p$valor, 0, 25),
      normalizar(solo$k$valor, 0, 200),
      normalizar(solo$ca$valor, 0, 6),
      normalizar(solo$mg$valor, 0, 2.5),
      normalizar(solo$v$valor, 0, 100)
    )
    
    cats_closed <- c(cats, cats[1])
    vals_closed <- c(vals, vals[1])
    
    plot_ly(
      type = 'scatterpolar',
      r = vals_closed,
      theta = cats_closed,
      fill = 'toself',
      fillcolor = 'rgba(45,106,79,0.2)',
      line = list(color = '#2d6a4f', width = 2),
      marker = list(color = '#1b4332', size = 6)
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE, range = c(0, 100),
            tickfont = list(size = 9),
            gridcolor = "#ddd"
          ),
          angularaxis = list(tickfont = list(size = 10, color = "#444"))
        ),
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        margin = list(t = 10, b = 10, l = 30, r = 30),
        showlegend = FALSE
      )
  })
  
  # GRÁFICO MACRONUTRIENTES - Barras com faixas de interpretação
  output$grafico_macro <- renderPlotly({
    rec <- dados_rec()
    if (is.null(rec)) return(plotly_vazio("Aguardando cálculo..."))
    
    solo <- rec$solo
    
    # Valores relativos ao ótimo
    # Plotly não renderiza HTML — usar notação Unicode para sub/superscrito
    dados <- data.frame(
      nutriente = c(
        "Ca\u00b2\u207a (cmol\u1d04 dm\u207b\u00b3)",
        "Mg\u00b2\u207a (cmol\u1d04 dm\u207b\u00b3)",
        "K\u207a (mg dm\u207b\u00b3 \u00f710)",
        "P-Mehlich (mg dm\u207b\u00b3)"
      ),
      valor     = c(solo$ca$valor, solo$mg$valor, solo$k$valor/10, solo$p$valor),
      otimo     = c(3.5, 1.2, 15, 12),
      cor       = c(
        solo$ca$interp$cor, solo$mg$interp$cor,
        solo$k$interp$cor,  solo$p$interp$cor
      ),
      classe    = c(
        solo$ca$interp$classe, solo$mg$interp$classe,
        solo$k$interp$classe,  solo$p$interp$classe
      ),
      unidade   = c(
        "cmol\u1d04 dm\u207b\u00b3",
        "cmol\u1d04 dm\u207b\u00b3",
        "mg dm\u207b\u00b3",
        "mg dm\u207b\u00b3"
      ),
      stringsAsFactors = FALSE
    )
    
    plot_ly(dados,
      x = ~nutriente, y = ~valor, type = "bar",
      marker = list(color = ~cor, line = list(color = "white", width = 1.5)),
      text = ~paste0(classe, "\n", round(valor, 1), " ", unidade),
      textposition = "outside",
      hovertemplate = "<b>%{x}</b><br>Valor: %{y:.2f} %{customdata}<br>Classe: %{text}<extra></extra>",
      customdata = ~unidade
    ) %>%
      add_trace(
        x = ~nutriente, y = ~otimo, type = "scatter", mode = "markers",
        marker = list(symbol = "line-ew", size = 20, color = "#333",
                      line = list(width = 3, color = "#333")),
        name = "N\u00edvel \u00f3timo", hoverinfo = "skip"
      ) %>%
      layout(
        xaxis = list(title = "", tickfont = list(size = 9)),
        yaxis = list(title = "Valor medido", gridcolor = "#eee"),
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        showlegend    = FALSE,
        margin = list(t = 20, b = 5)
      )
  })
  
  # GRÁFICO SATURAÇÕES
  output$grafico_saturacao <- renderPlotly({
    rec <- dados_rec()
    if (is.null(rec)) return(plotly_vazio("Aguardando cálculo..."))
    
    solo   <- rec$solo
    v_alvo <- rec$v_alvo
    
    fig <- plot_ly()
    
    # V% - gauge-like
    fig <- fig %>%
      add_trace(
        type = "indicator",
        mode = "gauge+number+delta",
        value = solo$v$valor,
        delta = list(reference = v_alvo, suffix = "%"),
        title = list(text = "Satura\u00e7\u00e3o por Bases (V%)", font = list(size = 13)),
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 1),
          bar   = list(color = solo$v$interp$cor, thickness = 0.3),
          steps = list(
            list(range = c(0, 25),  color = "#fde8e8"),
            list(range = c(25, 50), color = "#fff3e0"),
            list(range = c(50, 70), color = "#fffde7"),
            list(range = c(70, 100),color = "#e8f5e9")
          ),
          threshold = list(
            line  = list(color = "#1b4332", width = 3),
            thickness = 0.8,
            value = v_alvo
          )
        ),
        domain = list(x = c(0, 0.48), y = c(0, 1))
      ) %>%
      add_trace(
        type = "indicator",
        mode = "gauge+number",
        value = solo$m$valor,
        title = list(text = "Satura\u00e7\u00e3o por Al\u00b3\u207a (m%)", font = list(size = 13)),
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 1),
          bar   = list(color = solo$m$interp$cor, thickness = 0.3),
          steps = list(
            list(range = c(0, 15),  color = "#e8f5e9"),
            list(range = c(15, 30), color = "#fffde7"),
            list(range = c(30, 50), color = "#fff3e0"),
            list(range = c(50, 100),color = "#fde8e8")
          ),
          threshold = list(
            line  = list(color = "#c62828", width = 3),
            thickness = 0.8,
            value = 30
          )
        ),
        domain = list(x = c(0.52, 1), y = c(0, 1))
      ) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        margin = list(t = 30, b = 10)
      )
    
    fig
  })
  
  # GRÁFICO MICRONUTRIENTES
  output$grafico_micro <- renderPlotly({
    rec <- dados_rec()
    if (is.null(rec)) return(plotly_vazio("Aguardando cálculo..."))
    
    micro <- rec$solo$micro
    micro_vals <- Filter(Negate(is.null), micro)
    
    if (length(micro_vals) == 0) {
      return(plotly_vazio("Micronutrientes não informados"))
    }
    
    nomes   <- names(micro_vals)
    valores <- sapply(micro_vals, function(m) m$valor)
    classes <- sapply(micro_vals, function(m) m$interp$classe)
    cores   <- sapply(micro_vals, function(m) m$interp$cor)
    
    # Normalizar para % do ótimo
    otimos <- c(B = 0.40, Cu = 0.60, Fe = 12, Mn = 4.5, Zn = 1.5)
    pct_otimo <- sapply(nomes, function(n) {
      ot <- otimos[[n]]
      if (is.null(ot)) return(50)
      pmin(150, valores[[n]] / ot * 100)
    })
    
    plot_ly(
      x = nomes, y = pct_otimo, type = "bar",
      marker = list(
        color = cores,
        line = list(color = "white", width = 2),
        cornerradius = 6
      ),
      text = paste0(valores, " mg dm\u207b\u00b3 \u2014 ", classes),
      textposition = "outside",
      hovertemplate = "<b>%{x}</b><br>%{text}<br>%{y:.0f}% do n\u00edvel \u00f3timo<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Micronutriente", tickfont = list(size = 11)),
        yaxis = list(title = "% do N\u00edvel \u00d3timo", ticksuffix = "%", gridcolor = "#eee"),
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        margin = list(t = 10, b = 5),
        shapes = list(list(
          type = "line",
          xref = "paper", x0 = 0, x1 = 1,
          yref = "y",     y0 = 100, y1 = 100,
          line = list(color = "#2d6a4f", dash = "dash", width = 2)
        ))
      )
  })

  # --------------------------------------------------------------------------
  # ABA PESQUISA — sincronizar doses com o cálculo principal
  # --------------------------------------------------------------------------
  observeEvent(dados_rec(), {
    rec <- dados_rec()
    if (is.null(rec)) return()
    adub <- rec$adubacao
    n_tot <- adub$N_plantio + adub$N_cobertura
    updateNumericInput(session, "pesq_dose_n", value = n_tot)
    updateNumericInput(session, "pesq_dose_p", value = adub$P2O5)
    updateNumericInput(session, "pesq_dose_k", value = adub$K2O)
    # Calagem: pega o método selecionado
    cal <- rec$calagem
    if (!is.null(cal) && length(cal) > 0) {
      nc <- cal[[1]]
      updateNumericInput(session, "pesq_dose_cal", value = nc)
    }
    # Gesso: método argila anual como padrão
    ges <- rec$gesso
    if (!is.null(ges) && ges$necessidade) {
      updateNumericInput(session, "pesq_dose_gesso",
        value = ges$dose_argila_anual %||% 0)
    }
  })

  # --------------------------------------------------------------------------
  # CÁLCULO EXPERIMENTAL
  # --------------------------------------------------------------------------
  output$resultado_pesquisa <- renderUI({
    input$btn_calcular_pesq  # reativo ao botão

    isolate({
      dose_n    <- input$pesq_dose_n   %||% 0
      dose_p    <- input$pesq_dose_p   %||% 0
      dose_k    <- input$pesq_dose_k   %||% 0
      dose_cal  <- input$pesq_dose_cal %||% 0
      dose_ges  <- input$pesq_dose_gesso %||% 0
      unidade   <- input$pesq_unidade  %||% "metro_linear"
      espac     <- input$pesq_espacamento %||% 0.5
      pl_metro  <- input$pesq_plantas_metro %||% 5
      vol_vaso  <- input$pesq_vol_vaso %||% 10
      larg_parc <- input$pesq_largura_parcela %||% 3
      comp_parc <- input$pesq_comp_parcela %||% 5
      n_rep     <- input$pesq_repeticoes %||% 4
      n_trat    <- input$pesq_tratamentos %||% 5
      fonte_npk <- input$pesq_fonte_npk %||% "separado"

      # ---- CONVERSÕES BASE (ha = 10.000 m²) ----
      # kg/ha → g/m²
      n_gm2  <- dose_n   * 100 / 1000   # g/m²
      p_gm2  <- dose_p   * 100 / 1000
      k_gm2  <- dose_k   * 100 / 1000
      cal_gm2 <- dose_cal * 1e6 / 1e4   # t/ha → g/m²  = dose*100
      ges_gm2 <- dose_ges * 1e6 / 1e4

      # ---- UNIDADE: METRO LINEAR ----
      # Área efetiva por metro linear = espaçamento entre fileiras × 1 m
      area_ml <- espac * 1.0  # m²/m linear
      n_ml  <- round(dose_n  * area_ml / 10000 * 1000, 2)  # g/m linear
      p_ml  <- round(dose_p  * area_ml / 10000 * 1000, 2)
      k_ml  <- round(dose_k  * area_ml / 10000 * 1000, 2)
      cal_ml <- round(dose_cal * 1e6 * area_ml / 10000 / 1000, 1)  # g/m
      ges_ml <- round(dose_ges * 1e6 * area_ml / 10000 / 1000, 1)
      # Por planta (dado plantas/metro)
      n_pl  <- round(n_ml  / pl_metro, 2)
      p_pl  <- round(p_ml  / pl_metro, 2)
      k_pl  <- round(k_ml  / pl_metro, 2)

      # ---- UNIDADE: COVA ----
      # Assume espaçamento entre covas = espaçamento fileiras × espaçamento plantas
      # espaçamento plantas = 1/pl_metro
      dist_pl <- if (pl_metro > 0) 1 / pl_metro else 0.2
      area_cova <- espac * dist_pl  # m² por cova
      n_cova  <- round(dose_n  * area_cova / 10000 * 1000, 2)  # g/cova
      p_cova  <- round(dose_p  * area_cova / 10000 * 1000, 2)
      k_cova  <- round(dose_k  * area_cova / 10000 * 1000, 2)
      cal_cova <- round(dose_cal * 1e6 * area_cova / 10000 / 1000, 1)
      ges_cova <- round(dose_ges * 1e6 * area_cova / 10000 / 1000, 1)

      # ---- UNIDADE: VASO / LISÍMETRO ----
      # Densidade aparente típica = 1,2 kg/dm³ → 1 dm³ ≈ 1,2 kg solo
      # Volume → peso solo → dose proporcional
      # 1 ha × 20 cm prof. = 2.000.000 dm³ solo (dens. 1 kg/dm³ = 2.000.000 kg)
      # Fator de escala = vol_vaso / 2.000.000
      fator_vaso <- vol_vaso / 2e6
      n_vaso  <- round(dose_n  * 1000 * fator_vaso * 1000, 3)  # mg/vaso
      p_vaso  <- round(dose_p  * 1000 * fator_vaso * 1000, 3)
      k_vaso  <- round(dose_k  * 1000 * fator_vaso * 1000, 3)
      cal_vaso <- round(dose_cal * 1e9 * fator_vaso / 1000, 2)  # g/vaso
      ges_vaso <- round(dose_ges * 1e9 * fator_vaso / 1000, 2)

      # ---- UNIDADE: PARCELA ----
      area_parc <- larg_parc * comp_parc  # m²
      n_parc  <- round(dose_n  * area_parc / 10000 * 1000, 1)  # g/parcela
      p_parc  <- round(dose_p  * area_parc / 10000 * 1000, 1)
      k_parc  <- round(dose_k  * area_parc / 10000 * 1000, 1)
      cal_parc <- round(dose_cal * area_parc / 10000 * 1000, 0)  # g/parcela
      ges_parc <- round(dose_ges * area_parc / 10000 * 1000, 0)

      # ---- NPK FORMULADO (se fonte selecionada) ----
      npk_tabs <- NULL
      if (fonte_npk != "separado") {
        partes <- as.numeric(strsplit(fonte_npk, "-")[[1]]) / 100
        n_f <- partes[1]; p_f <- partes[2]; k_f <- partes[3]
        # Dose do formulado limitada pelo nutriente mais exigente
        dose_form_ha <- max(
          if (n_f > 0) dose_n / n_f else 0,
          if (p_f > 0) dose_p / p_f else 0,
          if (k_f > 0) dose_k / k_f else 0
        )
        form_ml   <- round(dose_form_ha * area_ml   / 10000 * 1000, 2)
        form_cova <- round(dose_form_ha * area_cova / 10000 * 1000, 2)
        form_parc <- round(dose_form_ha * area_parc / 10000 * 1000, 1)
        form_vaso <- round(dose_form_ha * 1000 * fator_vaso * 1000, 3)

        npk_tabs <- div(class = "result-card",
          div(class = "result-card-title",
            HTML(paste0('<i class="bi bi-box-seam"></i> Formulado NPK ',
                        toupper(fonte_npk), ' — Dose por Unidade'))),
          p(style = "font-size:11.5px; color:#888; margin-bottom:10px;",
            HTML(paste0('<i class="bi bi-info-circle"></i> Dose calculada pelo nutriente mais ',
              'limitante. Nutriente em excesso ser\u00e1 suprido acima da recomenda\u00e7\u00e3o.'))),
          div(class = "pesq-unidade-grid",
            pesq_card("Metro linear", "bi-dash-lg",
              list("Formulado" = list(v = form_ml,   u = "g m\u207b\u00b9")), destaque = TRUE),
            pesq_card("Cova / Ber\u00e7o", "bi-circle",
              list("Formulado" = list(v = form_cova, u = "g cova\u207b\u00b9")), destaque = TRUE),
            pesq_card("Vaso", "bi-box",
              list("Formulado" = list(v = form_vaso, u = "mg vaso\u207b\u00b9")), destaque = TRUE),
            pesq_card("Parcela", "bi-bounding-box",
              list("Formulado" = list(v = form_parc, u = "g parcela\u207b\u00b9")), destaque = TRUE)
          )
        )
      }

      # ---- TABELA DE TRATAMENTOS (gradientes de dose) ----
      doses_ref <- c(0, 25, 50, 75, 100, 125, 150)
      doses_trat <- doses_ref[seq_len(min(n_trat, length(doses_ref)))]
      if (length(doses_trat) < n_trat) {
        extras <- seq(175, by = 25, length.out = n_trat - length(doses_trat))
        doses_trat <- c(doses_trat, extras)
      }

      trat_rows <- lapply(seq_along(doses_trat), function(i) {
        pct   <- doses_trat[i] / 100
        n_t   <- round(dose_n   * pct, 0)
        p_t   <- round(dose_p   * pct, 0)
        k_t   <- round(dose_k   * pct, 0)
        n_ml_t <- round(n_t * area_ml / 10000 * 1000, 2)
        p_ml_t <- round(p_t * area_ml / 10000 * 1000, 2)
        k_ml_t <- round(k_t * area_ml / 10000 * 1000, 2)
        n_cova_t <- round(n_t * area_cova / 10000 * 1000, 2)
        p_cova_t <- round(p_t * area_cova / 10000 * 1000, 2)
        k_cova_t <- round(k_t * area_cova / 10000 * 1000, 2)

        tags$tr(
          tags$td(HTML(paste0("<b>T", i, "</b> — ", doses_trat[i], "%"))),
          tags$td(paste0(n_t, " / ", p_t, " / ", k_t)),
          tags$td(paste0(n_ml_t, " / ", p_ml_t, " / ", k_ml_t)),
          tags$td(paste0(n_cova_t, " / ", p_cova_t, " / ", k_cova_t))
        )
      })

      tagList(
        # Cards por unidade experimental
        div(class = "result-card",
          div(class = "result-card-title",
            HTML('<i class="bi bi-rulers"></i> Doses por Unidade Experimental')),
          div(class = "pesq-unidade-grid",

            pesq_card("Metro linear de sulco", "bi-dash-lg", list(
              "N"        = list(v = n_ml,   u = "g m\u207b\u00b9"),
              "P\u2082O\u2085" = list(v = p_ml,   u = "g m\u207b\u00b9"),
              "K\u2082O"  = list(v = k_ml,   u = "g m\u207b\u00b9"),
              "Calc\u00e1rio" = list(v = cal_ml, u = "g m\u207b\u00b9"),
              "Gesso"    = list(v = ges_ml,  u = "g m\u207b\u00b9")
            ), destaque = unidade == "metro_linear"),

            pesq_card("Por planta (metro linear)", "bi-flower1", list(
              "N"        = list(v = n_pl,  u = "g planta\u207b\u00b9"),
              "P\u2082O\u2085" = list(v = p_pl,  u = "g planta\u207b\u00b9"),
              "K\u2082O"  = list(v = k_pl,  u = "g planta\u207b\u00b9")
            ), destaque = FALSE),

            pesq_card("Cova / Ber\u00e7o", "bi-circle", list(
              "N"        = list(v = n_cova,   u = "g cova\u207b\u00b9"),
              "P\u2082O\u2085" = list(v = p_cova,   u = "g cova\u207b\u00b9"),
              "K\u2082O"  = list(v = k_cova,   u = "g cova\u207b\u00b9"),
              "Calc\u00e1rio" = list(v = cal_cova, u = "g cova\u207b\u00b9"),
              "Gesso"    = list(v = ges_cova,  u = "g cova\u207b\u00b9")
            ), destaque = unidade == "cova"),

            pesq_card("Vaso / Lis\u00edmetro", "bi-box", list(
              "N"        = list(v = n_vaso,   u = "mg vaso\u207b\u00b9"),
              "P\u2082O\u2085" = list(v = p_vaso,   u = "mg vaso\u207b\u00b9"),
              "K\u2082O"  = list(v = k_vaso,   u = "mg vaso\u207b\u00b9"),
              "Calc\u00e1rio" = list(v = cal_vaso, u = "g vaso\u207b\u00b9"),
              "Gesso"    = list(v = ges_vaso,  u = "g vaso\u207b\u00b9")
            ), destaque = unidade == "vaso"),

            pesq_card(paste0("Parcela (", larg_parc, "\u00d7", comp_parc, " m)"),
              "bi-bounding-box", list(
              "N"        = list(v = n_parc,   u = "g parcela\u207b\u00b9"),
              "P\u2082O\u2085" = list(v = p_parc,   u = "g parcela\u207b\u00b9"),
              "K\u2082O"  = list(v = k_parc,   u = "g parcela\u207b\u00b9"),
              "Calc\u00e1rio" = list(v = cal_parc, u = "g parcela\u207b\u00b9"),
              "Gesso"    = list(v = ges_parc,  u = "g parcela\u207b\u00b9")
            ), destaque = unidade == "parcela")
          ),

          # Parâmetros usados
          div(style = "margin-top:12px; font-size:11px; color:#888; padding-top:10px; border-top:1px solid #eee;",
            HTML(paste0(
              '<i class="bi bi-info-circle"></i> ',
              'Espa\u00e7amento entre fileiras: <b>', espac, ' m</b> | ',
              'Dist\u00e2ncia entre plantas: <b>', round(dist_pl, 2), ' m</b> | ',
              '\u00c1rea por metro linear: <b>', round(area_ml, 4), ' m\u00b2</b> | ',
              '\u00c1rea por cova: <b>', round(area_cova, 4), ' m\u00b2</b> | ',
              'Volume vaso: <b>', vol_vaso, ' dm\u00b3</b>'
            ))
          )
        ),

        # NPK formulado
        npk_tabs,

        # Tabela de gradiente de tratamentos
        div(class = "result-card",
          div(class = "result-card-title",
            HTML(paste0('<i class="bi bi-list-ol"></i> Gradiente de Tratamentos (',
                        n_trat, ' tratamentos \u00d7 ', n_rep, ' repeti\u00e7\u00f5es)'))),
          p(style = "font-size:11.5px; color:#888; margin-bottom:10px;",
            HTML(paste0(
              '<i class="bi bi-info-circle"></i> Doses em % da recomenda\u00e7\u00e3o. ',
              'Total de parcelas: <b>', n_trat * n_rep, '</b>. ',
              '\u00c1rea total do experimento: <b>',
              round(n_trat * n_rep * area_parc, 0), ' m\u00b2</b> ',
              '(excluindo bordaduras).'))),
          tags$table(class = "pesq-trat-table",
            tags$thead(tags$tr(
              tags$th("Tratamento"),
              tags$th(HTML("N / P\u2082O\u2085 / K\u2082O (kg ha\u207b\u00b9)")),
              tags$th(HTML("N / P\u2082O\u2085 / K\u2082O (g m\u207b\u00b9 sulco)")),
              tags$th(HTML("N / P\u2082O\u2085 / K\u2082O (g cova\u207b\u00b9)"))
            )),
            tags$tbody(trat_rows)
          )
        ),

        # Nota de conversões
        div(class = "result-card",
          style = "background:#f7f7fb;",
          div(class = "result-card-title",
            HTML('<i class="bi bi-book"></i> Metodologia de Convers\u00e3o')),
          tags$ul(style = "font-size:12px; color:#555; line-height:2; padding-left:18px;",
            tags$li(HTML("<b>Metro linear:</b> dose (kg ha\u207b\u00b9) \u00d7 espa\u00e7amento (m) \u00d7 1 m \u00f7 10.000 m\u00b2 ha\u207b\u00b9 \u00d7 1.000 g kg\u207b\u00b9")),
            tags$li(HTML("<b>Cova:</b> dose \u00d7 \u00e1rea por cova (espa\u00e7amento \u00d7 dist\u00e2ncia entre plantas) \u00f7 10.000")),
            tags$li(HTML("<b>Vaso:</b> dose \u00d7 volume vaso (dm\u00b3) \u00f7 2.000.000 dm\u00b3 ha\u207b\u00b9 (camada 0\u201320 cm, \u03c1\u00e3 = 1 kg dm\u207b\u00b3)")),
            tags$li(HTML("<b>Parcela:</b> dose \u00d7 \u00e1rea parcela (m\u00b2) \u00f7 10.000 m\u00b2 ha\u207b\u00b9")),
            tags$li(HTML("<b>Calcário e Gesso:</b> convertidos de t ha\u207b\u00b9 para g por unidade pelas mesmas rela\u00e7\u00f5es de \u00e1rea/volume"))
          )
        )
      )
    })
  })

  # --------------------------------------------------------------------------
  # ABA BANCO REGIONAL
  # --------------------------------------------------------------------------
  rv_regional_df <- reactiveVal(NULL)

  observeEvent(input$regional_file, {
    f <- input$regional_file
    if (is.null(f)) return()

    df <- tryCatch(
      carregar_banco_regional(f$datapath),
      error = function(e) {
        rv_regional_df(NULL)
        showNotification(
          HTML(paste0('<i class="bi bi-x-circle-fill"></i> Erro ao ler arquivo: ',
                       conditionMessage(e))),
          type = "error", duration = 8
        )
        NULL
      }
    )

    if (!is.null(df)) {
      rv_regional_df(df)
      showNotification(
        HTML(paste0('<i class="bi bi-check-circle-fill"></i> ',
                     nrow(df), " amostras carregadas com sucesso.")),
        type = "message", duration = 4
      )
    }
  })

  output$regional_status <- renderUI({
    df <- rv_regional_df()
    if (is.null(df)) return(NULL)

    resumo <- resumo_banco_regional(df)
    div(class = "regional-status-ok",
      HTML(paste0(
        '<i class="bi bi-check-circle-fill"></i> <b>', resumo$n_amostras,
        ' amostras</b> em <b>', resumo$n_municipios, ' munic\u00edpios</b>, anos: ',
        paste(resumo$anos, collapse = ", ")
      ))
    )
  })

  output$regional_conteudo <- renderUI({
    df <- rv_regional_df()
    if (is.null(df)) {
      return(placeholder_ui("map", "Banco Regional",
        "Envie o arquivo preenchido (template_banco_regional.xlsx) para ver estat\u00edsticas, mapas e benchmarking."))
    }

    resumo <- resumo_banco_regional(df)

    tagList(
      # Resumo geral
      div(class = "result-card",
        div(class = "result-card-title",
          HTML('<i class="bi bi-clipboard-data"></i> Resumo do Banco')),
        div(class = "regional-resumo-grid",
          div(class = "regional-resumo-card",
            div(class = "regional-resumo-valor", resumo$n_amostras),
            div(class = "regional-resumo-label", "Amostras")),
          div(class = "regional-resumo-card",
            div(class = "regional-resumo-valor", resumo$n_municipios),
            div(class = "regional-resumo-label", "Munic\u00edpios")),
          div(class = "regional-resumo-card",
            div(class = "regional-resumo-valor", length(resumo$anos)),
            div(class = "regional-resumo-label", "Anos")),
          div(class = "regional-resumo-card",
            div(class = "regional-resumo-valor",
                paste(range(resumo$anos), collapse = "\u2013")),
            div(class = "regional-resumo-label", "Per\u00edodo"))
        )
      ),

      # Seletor de atributo / ano / município para estatísticas e mapa
      div(class = "result-card",
        div(class = "result-card-title",
          HTML('<i class="bi bi-sliders"></i> Estat\u00edsticas e Mapa por Atributo')),
        div(class = "form-row-3",
          div(class = "form-group-custom",
            label_custom("Atributo", "bi-thermometer-half"),
            selectInput("regional_atributo", NULL, width = "100%",
              choices = setNames(names(nomes_atributos_regional), nomes_atributos_regional))
          ),
          div(class = "form-group-custom",
            label_custom("Ano", "bi-calendar3"),
            selectInput("regional_ano", NULL, width = "100%",
              choices = c("Todos", as.character(resumo$anos)))
          ),
          div(class = "form-group-custom",
            label_custom("Estat\u00edstica do mapa", "bi-map"),
            selectInput("regional_estatistica", NULL, width = "100%",
              choices = c("M\u00e9dia" = "media", "Mediana" = "mediana",
                          "Desvio padr\u00e3o" = "dp",
                          "Percentil 10" = "p10", "Percentil 90" = "p90"))
          )
        )
      ),

      # Tabela de estatísticas
      div(class = "result-card",
        div(class = "result-card-title",
          HTML('<i class="bi bi-table"></i> Estat\u00edsticas por Munic\u00edpio')),
        DTOutput("regional_tabela_stats")
      ),

      # Mapa coroplético
      div(class = "result-card",
        div(class = "result-card-title",
          HTML('<i class="bi bi-map-fill"></i> Mapa de Fertilidade por Munic\u00edpio')),
        uiOutput("regional_mapa_aviso"),
        div(class = "regional-map-container",
          leafletOutput("regional_mapa", height = "480px")
        ),
        p(style = "font-size:11px; color:#888; margin-top:8px;",
          HTML('<i class="bi bi-info-circle"></i> Mapa coropl\u00e9tico baseado nos limites ',
               'municipais do IBGE (via pacote <code>geobr</code>). Os nomes dos munic\u00edpios ',
               'na planilha devem corresponder \u00e0 grafia oficial do IBGE.'))
      ),

      # Série temporal
      div(class = "result-card",
        div(class = "result-card-title",
          HTML('<i class="bi bi-graph-up"></i> Tend\u00eancia Temporal')),
        div(class = "form-group-custom", style = "max-width:280px; margin-bottom:10px;",
          label_custom("Munic\u00edpio (ou Todos)", "bi-geo-alt"),
          selectInput("regional_municipio_serie", NULL, width = "100%",
            choices = c("Todos", resumo$municipios))
        ),
        withSpinner(plotlyOutput("regional_serie_temporal", height = "320px"),
                     type = 8, color = "#1a5276")
      ),

      # Benchmarking
      div(class = "result-card",
        style = "background:#f0f7fb;",
        div(class = "result-card-title",
          HTML('<i class="bi bi-bullseye"></i> Comparar Amostra Atual com a Regi\u00e3o')),
        p(style = "font-size:12px; color:#666; margin-bottom:10px;",
          HTML('<i class="bi bi-info-circle"></i> Compara os valores digitados na aba <b>Solo</b> ',
               'com a distribui\u00e7\u00e3o regional do munic\u00edpio selecionado.')),
        div(class = "form-group-custom", style = "max-width:280px; margin-bottom:10px;",
          label_custom("Munic\u00edpio de refer\u00eancia", "bi-geo-alt-fill"),
          selectInput("regional_municipio_bench", NULL, width = "100%",
            choices = resumo$municipios)
        ),
        uiOutput("regional_benchmark")
      ),

      # ── METAS PERSONALIZADAS ───────────────────────────────────────────────
      div(class = "result-card",
        div(class = "result-card-title",
          HTML('<i class="bi bi-flag-fill"></i> Metas de Fertilidade \u2014 Defina seus limites')),
        p(style = "font-size:12px;color:#666;margin-bottom:14px;",
          HTML('<i class="bi bi-info-circle"></i> Defina valores m\u00ednimos ou m\u00e1ximos para cada par\u00e2metro. ',
               'O sem\u00e1foro regional usa essas metas para classificar cada munic\u00edpio. ',
               'Deixe em branco para n\u00e3o avaliar o par\u00e2metro.')),
        div(class = "form-row-2",
          div(
            label_custom("pH m\u00ednimo", "bi-thermometer"),
            numericInput("meta_ph_min", NULL, value=5.5, min=4, max=7, step=0.1, width="100%")
          ),
          div(
            label_custom("V% m\u00ednimo (%)", "bi-percent"),
            numericInput("meta_v_min", NULL, value=60, min=0, max=100, step=5, width="100%")
          )
        ),
        div(class = "form-row-2",
          div(
            label_custom("P m\u00ednimo (mg/dm\u00b3)", "bi-arrow-up"),
            numericInput("meta_p_min", NULL, value=10, min=0, max=200, step=1, width="100%")
          ),
          div(
            label_custom("K m\u00ednimo (mg/dm\u00b3)", "bi-arrow-up"),
            numericInput("meta_k_min", NULL, value=60, min=0, max=500, step=10, width="100%")
          )
        ),
        div(class = "form-row-2",
          div(
            label_custom("Ca m\u00ednimo (cmolc/dm\u00b3)", "bi-arrow-up"),
            numericInput("meta_ca_min", NULL, value=1.5, min=0, max=15, step=0.5, width="100%")
          ),
          div(
            label_custom("Al m\u00e1ximo (cmolc/dm\u00b3)", "bi-arrow-down"),
            numericInput("meta_al_max", NULL, value=0.5, min=0, max=5, step=0.1, width="100%")
          )
        ),
        div(class = "form-row-2",
          div(
            label_custom("M.O. m\u00ednima (dag/kg)", "bi-arrow-up"),
            numericInput("meta_mo_min", NULL, value=1.5, min=0, max=10, step=0.5, width="100%")
          ),
          div(
            label_custom("m% m\u00e1ximo (%)", "bi-arrow-down"),
            numericInput("meta_m_max", NULL, value=20, min=0, max=100, step=5, width="100%")
          )
        ),
        div(class = "btn-container",
          actionButton("btn_gerar_semaforo",
            HTML('<i class="bi bi-play-fill"></i>&nbsp; GERAR SEM\u00c1FORO REGIONAL'),
            class = "btn-calcular", width = "100%")
        )
      ),

      # ── SEMÁFORO REGIONAL ─────────────────────────────────────────────────
      uiOutput("regional_semaforo"),

      # ── CRUZAMENTO CLIMA × FERTILIDADE ────────────────────────────────────
      div(class = "result-card",
        div(class = "result-card-title",
          HTML('<i class="bi bi-cloud-sun-fill"></i> Cruzamento Clima \u00d7 Fertilidade')),
        p(style = "font-size:12px;color:#666;margin-bottom:12px;",
          HTML('<i class="bi bi-info-circle"></i> Busca dados clim\u00e1ticos para cada munic\u00edpio ',
               'do banco (requer coordenadas na planilha) e calcula a correla\u00e7\u00e3o entre ',
               'chuva/temperatura e os atributos do solo. Per\u00edodo: \u00faltimos 12 meses.')),
        div(class = "form-row-2",
          div(class = "form-group-custom",
            label_custom("Atributo do solo (Y)", "bi-thermometer-half"),
            selectInput("clima_banco_atrib", NULL, width="100%",
              choices = setNames(names(nomes_atributos_regional), nomes_atributos_regional),
              selected = "ph")
          ),
          div(class = "form-group-custom",
            label_custom("Vari\u00e1vel clim\u00e1tica (X)", "bi-cloud-rain"),
            selectInput("clima_banco_var", NULL, width="100%",
              choices = c("Chuva total anual (mm)"="chuva_mm",
                          "Temperatura m\u00e9dia anual (\u00b0C)"="temp_media_c",
                          "ET\u2080 total anual (mm)"="et0_mm",
                          "Balan\u00e7o h\u00eddrico (mm)"="balanco_hidrico_mm"))
          )
        ),
        div(class = "btn-container",
          actionButton("btn_cruzar_clima",
            HTML('<i class="bi bi-cloud-download"></i>&nbsp; BUSCAR CLIMA DOS MUNIC\u00cdPIOS E CRUZAR'),
            class = "btn-calcular", width = "100%")
        ),
        p(style="font-size:10.5px;color:#aaa;margin-top:6px;",
          HTML('<i class="bi bi-clock"></i> A busca pode levar 10\u201330 segundos dependendo do n\u00famero de munic\u00edpios.'))
      ),
      uiOutput("regional_clima_cruzamento")
    )
  })

  # ---- Tabela de estatísticas ----
  output$regional_tabela_stats <- renderDT({
    df <- rv_regional_df()
    if (is.null(df) || is.null(input$regional_atributo)) return(NULL)

    stats <- estatisticas_regionais(df, input$regional_atributo, input$regional_ano)
    names(stats) <- c("Munic\u00edpio", "n", "M\u00e9dia", "Mediana", "Desvio Padr\u00e3o", "P10", "P90")

    datatable(stats, rownames = FALSE, selection = "none",
      class = "stripe hover compact",
      options = list(pageLength = 10, dom = "ftip",
        language = list(
          search = "Filtrar:",
          lengthMenu = "Mostrar _MENU_ linhas",
          info = "Mostrando _START_ a _END_ de _TOTAL_ munic\u00edpios",
          paginate = list(previous = "Anterior", "next" = "Pr\u00f3ximo")
        )))
  }, server = FALSE)

  # ---- Mapa coroplético ----
  rv_regional_nao_encontrados <- reactiveVal(character(0))

  output$regional_mapa <- renderLeaflet({
    df <- rv_regional_df()
    if (is.null(df) || is.null(input$regional_atributo)) {
      return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
               setView(lng = -37.4, lat = -10.3, zoom = 7))
    }

    stats <- estatisticas_regionais(df, input$regional_atributo, input$regional_ano)
    if (nrow(stats) == 0) {
      return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
               setView(lng = -37.4, lat = -10.3, zoom = 7))
    }

    estat_col <- input$regional_estatistica %||% "media"
    titulo    <- nomes_atributos_regional[[input$regional_atributo]]

    resultado <- tryCatch(
      mapa_coropletico_regional(stats, uf = "SE", coluna_valor = estat_col, titulo = titulo),
      error = function(e) {
        showNotification(
          HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ',
                       conditionMessage(e))),
          type = "error", duration = 8
        )
        NULL
      }
    )

    if (is.null(resultado)) {
      return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
               setView(lng = -37.4, lat = -10.3, zoom = 7))
    }

    rv_regional_nao_encontrados(resultado$nao_encontrados)
    resultado$mapa
  })

  output$regional_mapa_aviso <- renderUI({
    naoenc <- rv_regional_nao_encontrados()
    if (length(naoenc) == 0) return(NULL)
    div(class = "regional-status-warn",
      HTML(paste0(
        '<i class="bi bi-exclamation-triangle-fill"></i> ',
        '<b>', length(naoenc), ' munic\u00edpio(s) n\u00e3o encontrados</b> nos limites do IBGE: ',
        paste(naoenc, collapse = ", "),
        '. Verifique a grafia na planilha (deve ser igual \u00e0 grafia oficial do IBGE).'
      ))
    )
  })

  # ---- Série temporal ----
  output$regional_serie_temporal <- renderPlotly({
    df <- rv_regional_df()
    if (is.null(df) || is.null(input$regional_atributo)) return(plotly_vazio("Aguardando dados..."))

    serie <- serie_temporal_regional(df, input$regional_atributo,
                                      input$regional_municipio_serie %||% "Todos")
    if (nrow(serie) == 0) return(plotly_vazio("Sem dados para esta sele\u00e7\u00e3o"))

    titulo <- nomes_atributos_regional[[input$regional_atributo]]

    plot_ly(serie, x = ~ano, y = ~media, type = "scatter", mode = "lines+markers",
      line = list(color = "#1a5276", width = 3),
      marker = list(color = "#1a5276", size = 10),
      text = ~paste0("Ano: ", ano, "<br>M\u00e9dia: ", media, "<br>n = ", n),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Ano", dtick = 1, tickfont = list(size = 11)),
        yaxis = list(title = titulo, gridcolor = "#eee"),
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        margin = list(t = 20, b = 10)
      )
  })

  # ---- Benchmarking ----
  output$regional_benchmark <- renderUI({
    df <- rv_regional_df()
    if (is.null(df) || is.null(input$regional_municipio_bench)) return(NULL)

    solo_atual <- dados_solo()
    if (is.null(solo_atual)) {
      return(div(class = "regional-status-warn",
        HTML('<i class="bi bi-info-circle"></i> Calcule uma an\u00e1lise na aba <b>Solo</b> primeiro para habilitar a compara\u00e7\u00e3o.')
      ))
    }

    municipio <- input$regional_municipio_bench

    # Atributos comparáveis (presentes em solo_atual)
    atributos_bench <- list(
      ph = solo_atual$ph$valor, mo = solo_atual$mo$valor,
      p  = solo_atual$p$valor,  k  = solo_atual$k$valor,
      ca = solo_atual$ca$valor, mg = solo_atual$mg$valor,
      v_pct = solo_atual$v$valor, m_pct = solo_atual$m$valor
    )

    linhas <- lapply(names(atributos_bench), function(at) {
      valor <- atributos_bench[[at]]
      if (is.null(valor) || is.na(valor)) return(NULL)

      bm <- benchmark_valor(df, at, municipio, valor)
      if (!bm$disponivel) return(NULL)

      cor_pct <- if (bm$percentil < 25) "#d32f2f"
                 else if (bm$percentil < 50) "#f57c00"
                 else if (bm$percentil < 75) "#7cb342"
                 else "#2e7d32"

      div(class = "pesq-dose-row",
        span(class = "pesq-dose-label",
          HTML(paste0(nomes_atributos_regional[[at]], " = <b>", valor, "</b>"))),
        span(
          span(class = "pesq-dose-valor", style = paste0("color:", cor_pct, ";"),
               paste0("P", bm$percentil)),
          span(class = "pesq-dose-unit",
               paste0(" (m\u00e9dia regional: ", bm$media_regional, ", n=", bm$n, ")"))
        )
      )
    })

    linhas <- Filter(Negate(is.null), linhas)

    if (length(linhas) == 0) {
      return(div(class = "regional-status-warn",
        HTML(paste0('<i class="bi bi-info-circle"></i> Dados insuficientes em <b>',
                     municipio, '</b> para compara\u00e7\u00e3o (m\u00ednimo 3 amostras por atributo).'))
      ))
    }

    div(class = "benchmark-box",
      p(style="margin-bottom:8px;",
        HTML(paste0('Compara\u00e7\u00e3o com <b>', municipio, '</b> — percentil (P) indica ',
                     'a posi\u00e7\u00e3o do valor atual na distribui\u00e7\u00e3o hist\u00f3rica regional:'))),
      linhas
    )
  })

  # --------------------------------------------------------------------------
  # SEMÁFORO REGIONAL
  # --------------------------------------------------------------------------
  output$regional_semaforo <- renderUI({
    input$btn_gerar_semaforo
    isolate({
      df <- rv_regional_df()
      if (is.null(df)) return(NULL)

      # Metas definidas pelo usuário
      metas <- list(
        ph    = list(min = input$meta_ph_min, max = NULL),
        mo    = list(min = input$meta_mo_min, max = NULL),
        p     = list(min = input$meta_p_min,  max = NULL),
        k     = list(min = input$meta_k_min,  max = NULL),
        ca    = list(min = input$meta_ca_min, max = NULL),
        al    = list(min = NULL, max = input$meta_al_max),
        v_pct = list(min = input$meta_v_min,  max = NULL),
        m_pct = list(min = NULL, max = input$meta_m_max)
      )

      municipios <- sort(unique(df$municipio[!is.na(df$municipio)]))
      params     <- names(metas)

      # Calcula % de amostras fora da meta por município × parâmetro
      classificar_mun_param <- function(mun, param) {
        meta <- metas[[param]]
        col  <- df[[param]]
        if (is.null(col)) return(NA)
        vals <- col[df$municipio == mun & !is.na(col)]
        n    <- length(vals)
        if (n < 2) return(NA)
        if (!is.null(meta$min) && !is.na(meta$min)) {
          pct_ok <- mean(vals >= meta$min) * 100
        } else if (!is.null(meta$max) && !is.na(meta$max)) {
          pct_ok <- mean(vals <= meta$max) * 100
        } else {
          return(NA)
        }
        pct_ok
      }

      cor_semaforo <- function(pct_ok) {
        if (is.na(pct_ok)) return("#e0e0e0")
        if (pct_ok >= 75)  return("#27ae60")   # verde
        if (pct_ok >= 50)  return("#f39c12")   # amarelo
        return("#e74c3c")                        # vermelho
      }

      icone_semaforo <- function(pct_ok) {
        if (is.na(pct_ok)) return("\u2014")
        if (pct_ok >= 75)  return("\u2705")
        if (pct_ok >= 50)  return("\u26a0\ufe0f")
        return("\u274c")
      }

      # Nomes curtos dos parâmetros para o header
      nomes_curtos <- c(ph="pH",mo="M.O.",p="P",k="K",
                        ca="Ca",al="Al\u00b3\u207a",v_pct="V%",m_pct="m%")

      # Gera tabela visual (heatmap de semáforo)
      header_cells <- lapply(params, function(p) {
        tags$th(style="text-align:center;font-size:11px;padding:6px 10px;",
                HTML(nomes_curtos[p]))
      })

      linhas_tab <- lapply(municipios, function(mun) {
        celulas <- lapply(params, function(p) {
          pct <- classificar_mun_param(mun, p)
          cor <- cor_semaforo(pct)
          ico <- icone_semaforo(pct)
          lbl <- if (is.na(pct)) "\u2014" else paste0(round(pct), "%")
          title_txt <- if (is.na(pct)) "Sem dados ou meta n\u00e3o definida"
                       else paste0(round(pct,1), "% dentro da meta")
          tags$td(
            style = paste0("text-align:center;padding:5px 8px;",
                           "background:", cor, "22;",
                           "border-radius:6px;font-size:12px;"),
            title = title_txt,
            HTML(paste0(ico, "<br><span style='font-size:10px;color:#555;'>", lbl, "</span>"))
          )
        })
        tags$tr(
          tags$td(style="padding:5px 12px;font-weight:600;font-size:12px;white-space:nowrap;",
                  mun),
          celulas
        )
      })

      div(class = "result-card",
        div(class = "result-card-title",
          HTML('<i class="bi bi-traffic-light"></i> Sem\u00e1foro Regional \u2014 % de amostras dentro da meta')),
        p(style="font-size:11.5px;color:#666;margin-bottom:10px;",
          HTML('<i class="bi bi-info-circle"></i> ',
               '\u2705 &ge; 75% &nbsp; \u26a0\ufe0f 50\u201374% &nbsp; \u274c &lt; 50% &nbsp; \u2014 sem dados / meta n\u00e3o definida')),
        div(style="overflow-x:auto;",
          tags$table(
            style="border-collapse:separate;border-spacing:4px;width:100%;",
            tags$thead(
              tags$tr(
                tags$th(style="padding:6px 12px;font-size:12px;", "Munic\u00edpio"),
                header_cells
              )
            ),
            tags$tbody(linhas_tab)
          )
        )
      )
    })
  })

  # --------------------------------------------------------------------------
  # CRUZAMENTO CLIMA × FERTILIDADE
  # --------------------------------------------------------------------------
  rv_clima_banco <- reactiveVal(NULL)

  observeEvent(input$btn_cruzar_clima, {
    df <- rv_regional_df()
    req(df)
    coords <- extrair_coords_municipios(df)
    if (is.null(coords) || nrow(coords) == 0) {
      showNotification(
        "Nenhuma coordenada encontrada no banco. Preencha Latitude e Longitude na planilha.",
        type = "error", duration = 6
      )
      return()
    }

    fim    <- Sys.Date() - 1
    inicio <- fim - 365

    withProgress(message = "Buscando dados clim\u00e1ticos dos munic\u00edpios...",
                  detail = paste0("0/", nrow(coords), " munic\u00edpios"), {

      resultados <- lapply(seq_len(nrow(coords)), function(i) {
        incProgress(1/nrow(coords),
          detail = paste0(i, "/", nrow(coords), " — ", coords$municipio[i]))
        tryCatch({
          res <- buscar_clima_municipio(
            lat    = coords$latitude[i],
            lon    = coords$longitude[i],
            inicio = format(inicio, "%Y-%m-%d"),
            fim    = format(fim, "%Y-%m-%d"),
            variaveis = c("precipitation_sum","temperature_2m_max",
                          "temperature_2m_min","et0_fao_evapotranspiration")
          )
          if (!is.null(res$erro)) return(NULL)
          d <- res$dados
          # Agrega para totais/médias anuais
          chuva_total  <- if ("chuva_mm"    %in% names(d)) sum(d$chuva_mm, na.rm=TRUE) else NA
          temp_media   <- if ("temp_media_c"%in% names(d)) mean(d$temp_media_c, na.rm=TRUE) else NA
          et0_total    <- if ("et0_mm"      %in% names(d)) sum(d$et0_mm, na.rm=TRUE) else NA
          balanco      <- if (!is.na(chuva_total) && !is.na(et0_total))
                            chuva_total - et0_total else NA
          data.frame(
            municipio           = coords$municipio[i],
            chuva_mm            = round(chuva_total, 1),
            temp_media_c        = round(temp_media, 2),
            et0_mm              = round(et0_total, 1),
            balanco_hidrico_mm  = round(balanco, 1),
            stringsAsFactors    = FALSE
          )
        }, error = function(e) NULL)
      })

      df_clima_mun <- do.call(rbind, Filter(Negate(is.null), resultados))
      rv_clima_banco(df_clima_mun)
    })
  })

  output$regional_clima_cruzamento <- renderUI({
    df_reg   <- rv_regional_df()
    df_clima <- rv_clima_banco()
    if (is.null(df_reg) || is.null(df_clima)) return(NULL)

    atrib   <- input$clima_banco_atrib
    var_cli <- input$clima_banco_var

    # Calcula médias do atributo por município e junta com clima
    medias_solo <- tapply(df_reg[[atrib]], df_reg$municipio,
                           function(x) mean(x, na.rm=TRUE))
    df_join <- merge(
      data.frame(municipio=names(medias_solo),
                 valor_solo=round(unname(medias_solo), 3),
                 stringsAsFactors=FALSE),
      df_clima, by="municipio"
    )
    df_join <- df_join[!is.na(df_join$valor_solo) & !is.na(df_join[[var_cli]]), ]

    if (nrow(df_join) < 3) {
      return(div(class="result-card",
        div(class="result-card-title",
          HTML('<i class="bi bi-graph-up-arrow"></i> Clima \u00d7 Fertilidade')),
        div(style="color:#e67e22;font-size:12px;padding:12px;",
          HTML('<i class="bi bi-exclamation-triangle-fill"></i> Munic\u00edpios com dados clim\u00e1ticos insuficientes (m\u00ednimo 3).'))
      ))
    }

    r_corr  <- round(cor(df_join[[var_cli]], df_join$valor_solo, method="pearson"), 3)
    r_class <- if (abs(r_corr) >= 0.7) "forte" else if (abs(r_corr) >= 0.4) "moderada" else "fraca"
    r_dir   <- if (r_corr > 0) "positiva" else "negativa"

    nome_solo <- nomes_atributos_regional[[atrib]] %||% atrib
    nome_cli  <- c(chuva_mm="Chuva total anual (mm)",
                   temp_media_c="Temperatura m\u00e9dia anual (\u00b0C)",
                   et0_mm="ET\u2080 total anual (mm)",
                   balanco_hidrico_mm="Balan\u00e7o h\u00eddrico anual (mm)")[var_cli]

    g <- plot_ly(df_join,
        x    = ~get(var_cli),
        y    = ~valor_solo,
        text = ~municipio,
        type = "scatter", mode = "markers+text",
        textposition = "top center",
        marker = list(size = 14, color = "#1A5276",
                      line = list(color="white", width=1.5)),
        hovertemplate = paste0(
          "<b>%{text}</b><br>",
          nome_cli, ": %{x:.1f}<br>",
          nome_solo, ": %{y:.3f}<extra></extra>"
        )
      ) %>%
      add_trace(
        x    = ~get(var_cli),
        y    = ~fitted(lm(valor_solo ~ get(var_cli), data=df_join)),
        type = "scatter", mode = "lines",
        name = "Linha de tend\u00eancia",
        line = list(color="#C0392B", width=2, dash="dot"),
        hoverinfo = "skip", showlegend=FALSE
      ) %>%
      layout(
        xaxis  = list(title=nome_cli, gridcolor="#eee"),
        yaxis  = list(title=nome_solo, gridcolor="#eee"),
        paper_bgcolor="transparent", plot_bgcolor="transparent",
        margin=list(t=10)
      )

    div(class="result-card",
      div(class="result-card-title",
        HTML(paste0('<i class="bi bi-graph-up-arrow"></i> Clima \u00d7 Fertilidade \u2014 ',
                     nome_cli, ' \u00d7 ', nome_solo))),
      withSpinner(plotlyOutput("regional_clima_scatter", height="380px"),
                  type=8, color="#1A5276"),
      div(style=paste0("background:", if(abs(r_corr)>=0.4) "#eafaf1" else "#fff8e1",
                       ";border-left:3px solid ", if(abs(r_corr)>=0.4) "#27ae60" else "#f39c12",
                       ";padding:10px 14px;border-radius:4px;margin-top:10px;font-size:12px;"),
        HTML(paste0(
          '<b>r de Pearson = ', r_corr, '</b> \u2014 correla\u00e7\u00e3o ',
          r_class, ' e ', r_dir, ' entre ',
          nome_cli, ' e ', nome_solo, ' nos ', nrow(df_join), ' munic\u00edpios. ',
          if (abs(r_corr) >= 0.4)
            'Associa\u00e7\u00e3o clim\u00e1tica relevante \u2014 considere o regime pluviom\u00e9trico na interpreta\u00e7\u00e3o da fertilidade.'
          else
            'Correla\u00e7\u00e3o fraca \u2014 outros fatores (material de origem, manejo) dominam a varia\u00e7\u00e3o deste atributo.'
        ))
      ),
      div(style="margin-top:12px;",
        div(class="result-card-title",
          HTML('<i class="bi bi-table"></i> Dados por Munic\u00edpio')),
        datatable(
          setNames(
            df_join[, c("municipio","valor_solo",var_cli), drop=FALSE],
            c("Munic\u00edpio", nome_solo, nome_cli)
          ),
          rownames=FALSE, selection="none", class="stripe hover compact",
          options=list(pageLength=10, dom="tip",
            language=list(paginate=list(previous="Anterior","next"="Pr\u00f3ximo")))
        )
      )
    )
  })

  output$regional_clima_scatter <- renderPlotly({
    df_reg   <- rv_regional_df()
    df_clima <- rv_clima_banco()
    if (is.null(df_reg) || is.null(df_clima)) return(plotly_vazio("Busque os dados clim\u00e1ticos acima"))
    atrib   <- input$clima_banco_atrib
    var_cli <- input$clima_banco_var
    medias_solo <- tapply(df_reg[[atrib]], df_reg$municipio, function(x) mean(x, na.rm=TRUE))
    df_join <- merge(
      data.frame(municipio=names(medias_solo), valor_solo=round(unname(medias_solo),3),
                 stringsAsFactors=FALSE),
      df_clima, by="municipio"
    )
    df_join <- df_join[!is.na(df_join$valor_solo) & !is.na(df_join[[var_cli]]), ]
    if (nrow(df_join) < 2) return(plotly_vazio("Dados insuficientes para gr\u00e1fico"))
    nome_cli  <- c(chuva_mm="Chuva (mm)",temp_media_c="Temp (\u00b0C)",
                   et0_mm="ET\u2080 (mm)",balanco_hidrico_mm="Balan\u00e7o (mm)")[var_cli]
    nome_solo <- nomes_atributos_regional[[atrib]] %||% atrib
    plot_ly(df_join, x=~get(var_cli), y=~valor_solo, text=~municipio,
        type="scatter", mode="markers+text", textposition="top center",
        marker=list(size=14, color="#1A5276",
                    line=list(color="white", width=1.5)),
        hovertemplate=paste0("<b>%{text}</b><br>",nome_cli,": %{x:.1f}<br>",
                              nome_solo,": %{y:.3f}<extra></extra>")
      ) %>%
      add_trace(x=~get(var_cli),
        y=~fitted(lm(valor_solo ~ get(var_cli), data=df_join)),
        type="scatter", mode="lines",
        line=list(color="#C0392B", width=2, dash="dot"),
        hoverinfo="skip", showlegend=FALSE
      ) %>%
      layout(xaxis=list(title=nome_cli,gridcolor="#eee"),
             yaxis=list(title=nome_solo,gridcolor="#eee"),
             paper_bgcolor="transparent",plot_bgcolor="transparent",
             margin=list(t=10))
  })

  # --------------------------------------------------------------------------
  # SUB-ABA PESQUISA > ANÁLISES ESTATÍSTICAS
  # --------------------------------------------------------------------------

  # Fonte de dados: reaproveita rv_regional_df() se carregado; senão upload próprio
  rv_pesq_df_proprio <- reactiveVal(NULL)

  observeEvent(input$pesq_estat_file, {
    f <- input$pesq_estat_file
    if (is.null(f)) return()
    df <- tryCatch(carregar_banco_regional(f$datapath),
      error = function(e) {
        showNotification(
          HTML(paste0('<i class="bi bi-x-circle-fill"></i> Erro ao ler arquivo: ',
                       conditionMessage(e))), type = "error", duration = 8)
        NULL
      })
    if (!is.null(df)) {
      rv_pesq_df_proprio(df)
      showNotification(
        HTML(paste0('<i class="bi bi-check-circle-fill"></i> ', nrow(df), " amostras carregadas.")),
        type = "message", duration = 4)
    }
  })

  # Dataframe ativo para análises estatísticas
  pesq_df_ativo <- reactive({
    if (!is.null(rv_regional_df())) return(rv_regional_df())
    rv_pesq_df_proprio()
  })

  pesq_df_fonte <- reactive({
    if (!is.null(rv_regional_df())) "regional" else "proprio"
  })

  # Dataframe filtrado por cultura (usado em todas as análises)
  pesq_df_filtrado <- reactive({
    df <- pesq_df_ativo()
    if (is.null(df)) return(NULL)
    filtro <- input$pesq_filtro_cultura %||% "Todas"
    if (!is.null(filtro) && filtro != "Todas" &&
        "cultura" %in% names(df) && nchar(filtro) > 0) {
      df <- df[!is.na(df$cultura) & df$cultura == filtro, ]
    }
    df
  })

  # --- Conteúdo principal da sub-aba (UI dinâmica) ---
  output$pesq_estat_conteudo <- renderUI({
    df <- pesq_df_ativo()

    if (is.null(df)) {
      return(tagList(
        div(class = "result-card",
          div(class = "result-card-title",
            HTML('<i class="bi bi-database"></i> Fonte de Dados')),
          p(style = "font-size:12.5px; color:#666; margin-bottom:10px;",
            HTML('<i class="bi bi-info-circle"></i> Nenhum banco carregado. Use os dados j\u00e1 ',
                 'enviados na aba <b>Banco Regional</b>, ou envie seu pr\u00f3prio arquivo no ',
                 'formato do <code>template_banco_regional.xlsx</code>.')),
          a(href = "template_banco_regional.xlsx", download = NA, target = "_blank",
            class = "btn-cepea", style = "display:inline-flex; margin-bottom:10px; margin-right:8px;",
            HTML('<i class="bi bi-download"></i> Baixar template')),
          a(href = "banco_regional_EXEMPLO.xlsx", download = NA, target = "_blank",
            class = "btn-cepea", style = "display:inline-flex; margin-bottom:10px; background:#6c3483; border-color:#6c3483;",
            HTML('<i class="bi bi-stars"></i> Baixar planilha de EXEMPLO (140 amostras)')),
          fileInput("pesq_estat_file", NULL, accept = c(".xlsx"), width = "100%",
            buttonLabel = HTML('<i class="bi bi-folder2-open"></i> Escolher arquivo'),
            placeholder = "Nenhum arquivo selecionado")
        )
      ))
    }

    resumo <- resumo_banco_regional(df)
    atrib_choices <- atributos_numericos_disponiveis(df)
    grupo_choices  <- grupos_categoricos_disponiveis(df)
    tem_produtividade <- "produtividade" %in% names(df) && sum(!is.na(df$produtividade)) >= 8
    culturas_disp <- if ("cultura" %in% names(df)) {
      c("Todas", sort(unique(stats::na.omit(df$cultura))))
    } else "Todas"

    tagList(
      # Status da fonte
      div(class = "regional-status-ok",
        HTML(paste0(
          '<i class="bi bi-check-circle-fill"></i> Usando <b>', resumo$n_amostras,
          ' amostras</b> (', if (pesq_df_fonte() == "regional") "Banco Regional carregado" else "arquivo pr\u00f3prio",
          '). ',
          if (!tem_produtividade)
            '<span style="color:#a04000;"><i class="bi bi-exclamation-triangle"></i> Sem dados de produtividade suficientes \u2014 limiares cr\u00edticos (Cate-Nelson / Linear-Plat\u00f4) ficar\u00e3o indispon\u00edveis.</span>'
          else
            '<span style="color:#1e8449;"><i class="bi bi-check2"></i> Produtividade dispon\u00edvel \u2014 limiares cr\u00edticos habilitados.</span>'
        ))
      ),

      # Seletor de tipo de análise + filtro de cultura
      div(class = "result-card",
        div(class = "result-card-title",
          HTML('<i class="bi bi-list-check"></i> Tipo de An\u00e1lise')),

        # Filtro de cultura (só aparece se houver coluna "cultura" com >1 valor)
        if ("cultura" %in% names(df) && length(unique(stats::na.omit(df$cultura))) > 1) {
          div(class = "form-group-custom", style = "margin-bottom:10px;",
            label_custom("Filtrar por cultura", "bi-flower1"),
            selectInput("pesq_filtro_cultura", NULL, width = "100%",
              choices = c("Todas as culturas" = "Todas",
                          setNames(sort(unique(stats::na.omit(df$cultura))),
                                   sort(unique(stats::na.omit(df$cultura))))),
              selected = "Todas"
            )
          )
        },

        selectInput("pesq_tipo_analise", NULL, width = "100%",
          choices = c(
            "── Descritiva ──────────────────" = "header_descritiva",
            "Estat\u00edstica Descritiva + Normalidade" = "descritiva",
            "── Explorat\u00f3ria ────────────────" = "header_exploratoria",
            "Matriz de Correla\u00e7\u00e3o"              = "correlacao",
            "Dispers\u00e3o & Regress\u00e3o"             = "regressao",
            "Regress\u00e3o M\u00faltipla"                = "regressao_multipla",
            "An\u00e1lise de Trilha (Path Analysis)"      = "analise_trilha",
            "── Compara\u00e7\u00e3o de M\u00e9dias ─────" = "header_comp",
            "ANOVA \u2014 Param\u00e9trica"               = "anova",
            "Kruskal-Wallis \u2014 N\u00e3o-param\u00e9trico" = "kruskal",
            "Mann-Whitney (2 grupos)"            = "mann_whitney",
            "── Multivariada ───────────────" = "header_multi",
            "PCA \u2014 Componentes Principais"          = "pca",
            "Cluster \u2014 Tipologia de Solos"          = "cluster",
            "── Limiares Cr\u00edticos ──────────" = "header_limiares",
            "Limiar Cr\u00edtico \u2014 Cate-Nelson"      = "cate_nelson",
            "Limiar Cr\u00edtico \u2014 Linear-Plat\u00f4" = "linear_plato"
          )
        ),
        uiOutput("pesq_estat_inputs_dinamicos")
      ),

      # Resultado
      uiOutput("pesq_estat_resultado")
    )
  })

  # --- Inputs dinâmicos por tipo de análise ---
  output$pesq_estat_inputs_dinamicos <- renderUI({
    df <- pesq_df_filtrado()
    if (is.null(df) || is.null(input$pesq_tipo_analise)) return(NULL)

    atrib_choices <- atributos_numericos_disponiveis(df)
    grupo_choices <- grupos_categoricos_disponiveis(df)
    culturas_disp <- if ("cultura" %in% names(df)) {
      c("Todas", sort(unique(stats::na.omit(df$cultura))))
    } else "Todas"

    # Conjunto padrão de atributos para correlação/PCA/cluster
    default_multi <- intersect(
      c("ph","mo","p","k","ca","mg","al","h_al","argila","ctc","v_pct","m_pct"),
      atrib_choices
    )

    switch(input$pesq_tipo_analise,

      # Headers de separação (retornam NULL — apenas visuais no selectInput)
      "header_descritiva"   = NULL,
      "header_exploratoria" = NULL,
      "header_comp"         = NULL,
      "header_multi"        = NULL,
      "header_limiares"     = NULL,

      "descritiva" = tagList(
        div(class = "form-row-2",
          div(class = "form-group-custom",
            label_custom("Vari\u00e1veis a analisar", "bi-bar-chart"),
            selectizeInput("pesq_desc_vars", NULL, choices = atrib_choices,
              selected = head(default_multi, 5), multiple = TRUE, width = "100%")
          ),
          div(class = "form-group-custom",
            label_custom("Agrupar por (opcional)", "bi-collection"),
            selectInput("pesq_desc_grupo", NULL, width = "100%",
              choices = c("Nenhum" = "Nenhum", grupo_choices))
          )
        ),
        p(style = "font-size:11.5px; color:#888; margin-top:-4px;",
          HTML('<i class="bi bi-info-circle"></i> Gera: n, m\u00e9dia, mediana, DP, CV%, ',
               'assimetria, curtose, outliers (Tukey/IQR), teste de normalidade ',
               '(Shapiro-Wilk ou KS) e gr\u00e1ficos interativos (histograma + boxplot + Q-Q).')),
        div(class = "btn-container",
          actionButton("btn_rodar_estatistica",
            HTML('<i class="bi bi-play-fill"></i>&nbsp; GERAR RELAT\u00d3RIO DESCRITIVO'),
            class = "btn-calcular", width = "100%"))
      ),

      "kruskal" = tagList(
        div(class = "form-row-2",
          div(class = "form-group-custom",
            label_custom("Atributo (Y)", "bi-thermometer-half"),
            selectInput("pesq_kw_y", NULL, choices = atrib_choices, width = "100%")
          ),
          div(class = "form-group-custom",
            label_custom("Fator de grupo", "bi-collection"),
            selectInput("pesq_kw_grupo", NULL, choices = grupo_choices, width = "100%")
          )
        ),
        p(style = "font-size:11.5px; color:#888; margin-top:-4px;",
          HTML('<i class="bi bi-info-circle"></i> Alternativa n\u00e3o-param\u00e9trica \u00e0 ANOVA. ',
               'Compara medianas de 3+ grupos sem exigir normalidade. ',
               'P\u00f3s-hoc: teste de Dunn com corre\u00e7\u00e3o de Bonferroni.')),
        div(class = "btn-container",
          actionButton("btn_rodar_estatistica",
            HTML('<i class="bi bi-play-fill"></i>&nbsp; CALCULAR'),
            class = "btn-calcular", width = "100%"))
      ),

      "mann_whitney" = tagList(
        div(class = "form-row-2",
          div(class = "form-group-custom",
            label_custom("Atributo (Y)", "bi-thermometer-half"),
            selectInput("pesq_mw_y", NULL, choices = atrib_choices, width = "100%")
          ),
          div(class = "form-group-custom",
            label_custom("Fator de grupo (exatamente 2 n\u00edveis)", "bi-diagram-2"),
            selectInput("pesq_mw_grupo", NULL, choices = grupo_choices, width = "100%")
          )
        ),
        p(style = "font-size:11.5px; color:#888; margin-top:-4px;",
          HTML('<i class="bi bi-info-circle"></i> Alternativa n\u00e3o-param\u00e9trica ao teste t ',
               'para 2 amostras independentes. Compara medianas sem exigir normalidade. ',
               'Inclui estimativa de Hodges-Lehmann para a diferen\u00e7a de localiza\u00e7\u00e3o.')),
        div(class = "btn-container",
          actionButton("btn_rodar_estatistica",
            HTML('<i class="bi bi-play-fill"></i>&nbsp; CALCULAR'),
            class = "btn-calcular", width = "100%"))
      ),

      "correlacao" = tagList(
        div(class = "form-row-2",
          div(class = "form-group-custom",
            label_custom("Atributos (m\u00ednimo 2)", "bi-grid-3x3"),
            selectizeInput("pesq_corr_vars", NULL, choices = atrib_choices,
              selected = default_multi, multiple = TRUE, width = "100%")
          ),
          div(class = "form-group-custom",
            label_custom("M\u00e9todo", "bi-calculator"),
            selectInput("pesq_corr_metodo", NULL, width = "100%",
              choices = c("Pearson" = "pearson", "Spearman (n\u00e3o-param.)" = "spearman"))
          )
        ),
        div(class = "btn-container",
          actionButton("btn_rodar_estatistica", HTML('<i class="bi bi-play-fill"></i>&nbsp; CALCULAR'),
            class = "btn-calcular", width = "100%"))
      ),

      "regressao" = tagList(
        div(class = "form-row-3",
          div(class = "form-group-custom",
            label_custom("Vari\u00e1vel X", "bi-arrow-left-right"),
            selectInput("pesq_reg_x", NULL, choices = atrib_choices, width = "100%")
          ),
          div(class = "form-group-custom",
            label_custom("Vari\u00e1vel Y", "bi-arrow-up"),
            selectInput("pesq_reg_y", NULL, choices = atrib_choices,
              selected = if ("produtividade" %in% atrib_choices) "produtividade" else unname(atrib_choices[2]),
              width = "100%")
          ),
          div(class = "form-group-custom",
            label_custom("Grau do polin\u00f4mio", "bi-graph-up"),
            selectInput("pesq_reg_grau", NULL, width = "100%",
              choices = c("Linear" = 1, "Quadr\u00e1tico" = 2, "C\u00fabico" = 3))
          )
        ),
        div(class = "btn-container",
          actionButton("btn_rodar_estatistica", HTML('<i class="bi bi-play-fill"></i>&nbsp; CALCULAR'),
            class = "btn-calcular", width = "100%"))
      ),

      "regressao_multipla" = tagList(
        div(class = "form-row-2",
          div(class = "form-group-custom",
            label_custom("Vari\u00e1veis explicativas \u2014 X (m\u00ednimo 2)", "bi-grid-3x3"),
            selectizeInput("pesq_rm_x", NULL, choices = atrib_choices,
              selected = head(default_multi, 4), multiple = TRUE, width = "100%")
          ),
          div(class = "form-group-custom",
            label_custom("Vari\u00e1vel dependente \u2014 Y", "bi-arrow-up"),
            selectInput("pesq_rm_y", NULL, choices = atrib_choices,
              selected = if ("produtividade" %in% atrib_choices) "produtividade" else unname(atrib_choices[length(atrib_choices)]),
              width = "100%")
          )
        ),
        p(style = "font-size:11.5px; color:#888; margin-top:-4px;",
          HTML('<i class="bi bi-info-circle"></i> A vari\u00e1vel Y \u00e9 automaticamente removida ',
               'das op\u00e7\u00f5es de X. Coeficientes padronizados (\u03b2) e VIF (multicolinearidade) ',
               's\u00e3o calculados automaticamente.')),
        div(class = "btn-container",
          actionButton("btn_rodar_estatistica", HTML('<i class="bi bi-play-fill"></i>&nbsp; CALCULAR'),
            class = "btn-calcular", width = "100%"))
      ),

      "analise_trilha" = tagList(
        div(class = "form-row-2",
          div(class = "form-group-custom",
            label_custom("Vari\u00e1veis explicativas \u2014 X (m\u00ednimo 2)", "bi-grid-3x3"),
            selectizeInput("pesq_trilha_x", NULL, choices = atrib_choices,
              selected = head(default_multi, 4), multiple = TRUE, width = "100%")
          ),
          div(class = "form-group-custom",
            label_custom("Vari\u00e1vel principal \u2014 Y", "bi-bullseye"),
            selectInput("pesq_trilha_y", NULL, choices = atrib_choices,
              selected = if ("produtividade" %in% atrib_choices) "produtividade" else unname(atrib_choices[length(atrib_choices)]),
              width = "100%")
          )
        ),
        p(style = "font-size:11.5px; color:#888; margin-top:-4px;",
          HTML('<i class="bi bi-info-circle"></i> Decomp\u00f5e a correla\u00e7\u00e3o de cada vari\u00e1vel X ',
               'com Y em efeito DIRETO (controlando as demais) e INDIRETO (mediado pelas demais). ',
               'Inclui diagn\u00f3stico de multicolinearidade (Cruz &amp; Carneiro, 2003).')),
        div(class = "btn-container",
          actionButton("btn_rodar_estatistica", HTML('<i class="bi bi-play-fill"></i>&nbsp; CALCULAR'),
            class = "btn-calcular", width = "100%"))
      ),

      "anova" = tagList(
        div(class = "form-row-2",
          div(class = "form-group-custom",
            label_custom("Atributo (vari\u00e1vel num\u00e9rica)", "bi-thermometer-half"),
            selectInput("pesq_anova_y", NULL, choices = atrib_choices, width = "100%")
          ),
          div(class = "form-group-custom",
            label_custom("Agrupar por", "bi-collection"),
            selectInput("pesq_anova_grupo", NULL, choices = grupo_choices, width = "100%")
          )
        ),
        div(class = "btn-container",
          actionButton("btn_rodar_estatistica", HTML('<i class="bi bi-play-fill"></i>&nbsp; CALCULAR'),
            class = "btn-calcular", width = "100%"))
      ),

      "pca" = tagList(
        div(class = "form-group-custom",
          label_custom("Atributos (m\u00ednimo 3)", "bi-grid-3x3"),
          selectizeInput("pesq_pca_vars", NULL, choices = atrib_choices,
            selected = default_multi, multiple = TRUE, width = "100%")
        ),
        div(class = "btn-container",
          actionButton("btn_rodar_estatistica", HTML('<i class="bi bi-play-fill"></i>&nbsp; CALCULAR'),
            class = "btn-calcular", width = "100%"))
      ),

      "cluster" = tagList(
        div(class = "form-row-2",
          div(class = "form-group-custom",
            label_custom("Atributos (m\u00ednimo 2)", "bi-grid-3x3"),
            selectizeInput("pesq_cluster_vars", NULL, choices = atrib_choices,
              selected = default_multi, multiple = TRUE, width = "100%")
          ),
          div(class = "form-group-custom",
            label_custom("N\u00famero de grupos (k)", "bi-diagram-3"),
            numericInput("pesq_cluster_k", NULL, value = 3, min = 2, max = 6, step = 1, width = "100%")
          )
        ),
        div(class = "btn-container",
          actionButton("btn_rodar_estatistica", HTML('<i class="bi bi-play-fill"></i>&nbsp; CALCULAR'),
            class = "btn-calcular", width = "100%"))
      ),

      "cate_nelson" = if (!("produtividade" %in% names(df) && sum(!is.na(df$produtividade)) >= 8)) {
        div(class = "stat-erro",
          HTML('<i class="bi bi-exclamation-triangle-fill"></i> Requer a coluna <b>produtividade</b> ',
               'preenchida em pelo menos 8 amostras. Atualize o banco regional com o template ',
               'que inclui produtividade.'))
      } else tagList(
        div(class = "form-row-2",
          div(class = "form-group-custom",
            label_custom("Atributo do solo (X)", "bi-thermometer-half"),
            selectInput("pesq_cn_x", NULL,
              choices = atrib_choices[atrib_choices != "produtividade"], width = "100%")
          ),
          div(class = "form-group-custom",
            label_custom("Cultura", "bi-flower2"),
            selectInput("pesq_cn_cultura", NULL, choices = culturas_disp, width = "100%")
          )
        ),
        div(class = "form-row-2",
          div(class = "form-group-custom",
            label_custom("Produtividade relativa cr\u00edtica (%)", "bi-percent"),
            numericInput("pesq_cn_ycrit", NULL, value = 90, min = 50, max = 99, step = 1, width = "100%")
          ),
          div(class = "form-group-custom", style = "padding-top: 22px;",
            checkboxInput("pesq_cn_testemunha",
              "Usar apenas parcelas Testemunha (sem aduba\u00e7\u00e3o)", value = FALSE)
          )
        ),
        div(class = "btn-container",
          actionButton("btn_rodar_estatistica", HTML('<i class="bi bi-play-fill"></i>&nbsp; CALCULAR'),
            class = "btn-calcular", width = "100%"))
      ),

      "linear_plato" = if (!("produtividade" %in% names(df) && sum(!is.na(df$produtividade)) >= 8)) {
        div(class = "stat-erro",
          HTML('<i class="bi bi-exclamation-triangle-fill"></i> Requer a coluna <b>produtividade</b> ',
               'preenchida em pelo menos 8 amostras. Atualize o banco regional com o template ',
               'que inclui produtividade.'))
      } else tagList(
        div(class = "form-row-2",
          div(class = "form-group-custom",
            label_custom("Atributo do solo (X)", "bi-thermometer-half"),
            selectInput("pesq_lp_x", NULL,
              choices = atrib_choices[atrib_choices != "produtividade"], width = "100%")
          ),
          div(class = "form-group-custom",
            label_custom("Cultura", "bi-flower2"),
            selectInput("pesq_lp_cultura", NULL, choices = culturas_disp, width = "100%")
          )
        ),
        div(class = "form-group-custom",
          checkboxInput("pesq_lp_testemunha",
            "Usar apenas parcelas Testemunha (sem aduba\u00e7\u00e3o)", value = FALSE)
        ),
        div(class = "btn-container",
          actionButton("btn_rodar_estatistica", HTML('<i class="bi bi-play-fill"></i>&nbsp; CALCULAR'),
            class = "btn-calcular", width = "100%"))
      )
    )
  })

  # --- Resultado da análise (reativo a Calcular, não é output) ---
  pesq_clicado <- reactiveVal(FALSE)
  observeEvent(input$btn_rodar_estatistica, { pesq_clicado(TRUE) })
  # Ao trocar o tipo de análise, esconde resultado anterior (evita plot
  # de um tipo aparecer no container de outro até novo clique em Calcular)
  observeEvent(input$pesq_tipo_analise, { pesq_clicado(FALSE) }, ignoreInit = TRUE)

  pesq_resultado_calc <- eventReactive(input$btn_rodar_estatistica, {
    df <- pesq_df_filtrado()
    tipo <- input$pesq_tipo_analise
    if (is.null(df) || is.null(tipo)) return(list(ui = NULL, plot = NULL))

    if (nrow(df) < 3) return(list(
      ui = div(class = "stat-erro",
        HTML('<i class="bi bi-exclamation-triangle-fill"></i> Dados insuficientes ap\u00f3s filtro de cultura (n &lt; 3). Selecione "Todas as culturas" ou uma cultura com mais amostras.')),
      plot = NULL
    ))

    switch(tipo,

      # Headers — não calculam nada
      "header_descritiva"   = list(ui = NULL, plot = NULL),
      "header_exploratoria" = list(ui = NULL, plot = NULL),
      "header_comp"         = list(ui = NULL, plot = NULL),
      "header_multi"        = list(ui = NULL, plot = NULL),
      "header_limiares"     = list(ui = NULL, plot = NULL),

      # ── ESTATÍSTICA DESCRITIVA ──────────────────────────────────────────────
      "descritiva" = {
        vars  <- input$pesq_desc_vars
        grupo <- input$pesq_desc_grupo %||% "Nenhum"

        if (length(vars) == 0) return(list(
          ui = div(class="stat-erro", HTML('<i class="bi bi-exclamation-triangle-fill"></i> Selecione ao menos uma variável.')),
          plot = NULL))

        res <- calc_descritiva(df, vars, grupo)
        if (!is.null(res$erro)) return(list(
          ui = div(class="stat-erro", HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', res$erro))),
          plot = NULL))

        nomes <- nomes_atributos_regional

        # Monta cards por variável/grupo
        cards <- lapply(res$resultados, function(r) {
          nome_var <- nomes[[r$variavel]] %||% r$variavel
          titulo   <- if (r$grupo == "Todos") nome_var
                      else paste0(nome_var, " — ", r$grupo)

          # Cor do badge de normalidade
          if (is.na(r$normalidade$normal)) {
            badge_cls <- "pesq-badge pesq-badge-warn"; badge_txt <- "N/D"
          } else if (r$normalidade$normal) {
            badge_cls <- "pesq-badge pesq-badge-ok"; badge_txt <- "Normal"
          } else {
            badge_cls <- "pesq-badge pesq-badge-info"; badge_txt <- "N\u00e3o normal"
          }

          # Alerta de outliers
          alerta_out <- if (r$n_outliers > 0) {
            div(style = "background:#fff8e1; border-left:3px solid #ffc107; padding:8px 12px; border-radius:4px; margin-top:8px; font-size:11.5px; color:#6d4c00;",
              HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> <b>',
                r$n_outliers, ' outlier(s)</b> detectado(s) pelo m\u00e9todo de Tukey (IQR). ',
                'Limite: [', r$lim_inf_outlier, ', ', r$lim_sup_outlier, ']. ',
                'Valores: ', paste(round(r$outliers, 3), collapse=", ")))
            )
          } else NULL

          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-card-list"></i> ', titulo)),
              span(class = badge_cls, HTML(badge_txt))
            ),
            # Tabela de medidas
            tags$table(class = "pesq-trat-table", style = "margin-bottom:8px;",
              tags$thead(tags$tr(
                tags$th("Medida"), tags$th("Valor"),
                tags$th("Medida"), tags$th("Valor")
              )),
              tags$tbody(
                tags$tr(
                  tags$td("n"), tags$td(r$n),
                  tags$td("M\u00e9dia"), tags$td(r$media)
                ),
                tags$tr(
                  tags$td("Mediana"), tags$td(r$mediana),
                  tags$td("Moda"), tags$td(if(is.na(r$moda)) "—" else r$moda)
                ),
                tags$tr(
                  tags$td("M\u00ednimo"), tags$td(r$minimo),
                  tags$td("M\u00e1ximo"), tags$td(r$maximo)
                ),
                tags$tr(
                  tags$td("Amplitude"), tags$td(r$amplitude),
                  tags$td("IQR (Q3-Q1)"), tags$td(r$iqr)
                ),
                tags$tr(
                  tags$td("Q1 (P25)"), tags$td(r$q1),
                  tags$td("Q3 (P75)"), tags$td(r$q3)
                ),
                tags$tr(
                  tags$td("P10"), tags$td(r$p10),
                  tags$td("P90"), tags$td(r$p90)
                ),
                tags$tr(
                  tags$td("Desvio padr\u00e3o"), tags$td(r$dp),
                  tags$td("Vari\u00e2ncia"), tags$td(r$variancia)
                ),
                tags$tr(
                  tags$td("CV%"), tags$td(paste0(r$cv_pct, "% — ", r$cv_class)),
                  tags$td("Outliers"), tags$td(r$n_outliers)
                ),
                tags$tr(
                  tags$td("Assimetria"), tags$td(r$assimetria),
                  tags$td("Curtose"), tags$td(r$curtose)
                ),
                tags$tr(
                  tags$td(paste0("Normalidade (", r$normalidade$teste, ")")),
                  tags$td(paste0("W/D = ", r$normalidade$estatistica, ", p = ", r$normalidade$p)),
                  tags$td("Diagn\u00f3stico"),
                  tags$td(HTML(if(isTRUE(r$normalidade$normal))
                    '<span style="color:#27ae60;font-weight:600;">Normal ✓</span>'
                    else '<span style="color:#e74c3c;font-weight:600;">N\u00e3o normal ✗</span>'))
                )
              )
            ),
            div(class = "stat-interpretacao",
              HTML(paste0('<i class="bi bi-lightbulb"></i> ', r$normalidade$interpretacao))
            ),
            alerta_out
          )
        })

        # Gráfico: histograma da primeira variável selecionada
        r1    <- res$resultados[[1]]
        x_plt <- r1$x_vals
        nome1 <- nomes[[r1$variavel]] %||% r1$variavel

        hist_plot <- plot_ly(x = x_plt, type = "histogram",
            marker = list(color = "#2D6A4F", line = list(color="white", width=0.5)),
            name = "Frequ\u00eancia",
            hovertemplate = "Valor: %{x}<br>Freq: %{y}<extra></extra>"
          ) %>%
          layout(
            xaxis = list(title = nome1, gridcolor = "#eee"),
            yaxis = list(title = "Frequ\u00eancia", gridcolor = "#eee"),
            paper_bgcolor = "transparent", plot_bgcolor = "transparent",
            margin = list(t = 10)
          )

        # Q-Q plot
        q_emp  <- sort(x_plt)
        q_teo  <- qnorm(ppoints(length(x_plt)))
        qq_plot <- plot_ly(x = q_teo, y = q_emp, type = "scatter", mode = "markers",
            marker = list(color = "#1A5276", size = 7, opacity = 0.7),
            name = "Q-Q",
            hovertemplate = "Te\u00f3rico: %{x:.3f}<br>Emp\u00edrico: %{y:.3f}<extra></extra>"
          ) %>%
          add_trace(
            x = range(q_teo), y = range(q_teo),
            type = "scatter", mode = "lines",
            line = list(color = "#e74c3c", dash = "dot", width = 1.5),
            name = "Normal", showlegend = FALSE, hoverinfo = "skip"
          ) %>%
          layout(
            xaxis = list(title = "Quantis te\u00f3ricos (Normal)", gridcolor = "#eee"),
            yaxis = list(title = paste0("Quantis emp\u00edricos (", nome1, ")"), gridcolor = "#eee"),
            paper_bgcolor = "transparent", plot_bgcolor = "transparent",
            margin = list(t = 10)
          )

        combo <- subplot(hist_plot, qq_plot, nrows = 1, margin = 0.08,
                          titleX = TRUE, titleY = TRUE) %>%
          layout(showlegend = FALSE)

        ui_out <- tagList(
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-graph-up"></i> Histograma + Q-Q Plot \u2014 ',
                           nome1, ' <span class="stat-badge-n">n = ', r1$n, '</span>'))),
            withSpinner(plotlyOutput("pesq_plot_descritiva", height = "380px"), type = 8, color = "#2D6A4F")
          ),
          cards
        )
        list(ui = ui_out, plot = combo)
      },

      # ── KRUSKAL-WALLIS ──────────────────────────────────────────────────────
      "kruskal" = {
        atributo <- input$pesq_kw_y
        grupo    <- input$pesq_kw_grupo
        res <- calc_kruskal(df, atributo, grupo)
        if (!is.null(res$erro)) return(list(
          ui = div(class="stat-erro", HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', res$erro))),
          plot = NULL))

        nome_a <- nomes_atributos_regional[[atributo]] %||% atributo

        # Boxplot por grupo
        d_plot <- df[!is.na(df[[atributo]]) & !is.na(df[[grupo]]), ]
        boxplot <- plot_ly(d_plot, x = ~get(grupo), y = ~get(atributo),
            type = "box", boxpoints = "outliers",
            marker = list(color = "#1A5276"), fillcolor = "rgba(44,44,122,0.15)",
            line = list(color = "#2c2c7a"),
            hovertemplate = "%{x}<br>Mediana: %{median:.3f}<extra></extra>"
          ) %>%
          layout(
            xaxis = list(title = grupo),
            yaxis = list(title = nome_a, gridcolor = "#eee"),
            paper_bgcolor = "transparent", plot_bgcolor = "transparent",
            margin = list(t = 10), showlegend = FALSE
          )

        interp <- interpretar_kruskal(res, nome_a, grupo)

        # Tabela de medianas
        med_df <- data.frame(
          Grupo = names(res$medianas),
          n = as.integer(res$ns[names(res$medianas)]),
          Mediana = unname(res$medianas),
          stringsAsFactors = FALSE
        )
        names(med_df)[1] <- grupo

        ui_out <- tagList(
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-bar-chart-steps"></i> Kruskal-Wallis \u2014 ',
                nome_a, ' por ', grupo,
                '<span class="stat-badge-n">n = ', res$n, '</span>'))),
            withSpinner(plotlyOutput("pesq_plot_kruskal", height = "380px"), type = 8, color = "#2c2c7a"),
            div(class = "stat-interpretacao", HTML(interp))
          ),
          div(class = "result-card",
            div(class = "result-card-title", HTML('<i class="bi bi-table"></i> Medianas por Grupo')),
            datatable(med_df, rownames = FALSE, selection = "none",
              class = "stripe hover compact",
              options = list(pageLength = 15, dom = "tip",
                language = list(paginate = list(previous="Anterior", "next"="Pr\u00f3ximo"))))
          ),
          if (!is.null(res$dunn) && nrow(res$dunn) > 0) {
            dunn_show <- res$dunn
            dunn_show$significativo <- ifelse(dunn_show$significativo, "\u2705 Sim", "\u274c N\u00e3o")
            names(dunn_show) <- c("Grupo 1", "Grupo 2", "Z", "p (bruto)", "p (Bonferroni)", "Significativo")
            div(class = "result-card",
              div(class = "result-card-title",
                HTML('<i class="bi bi-grid-3x3"></i> P\u00f3s-hoc Dunn (Bonferroni)')),
              datatable(dunn_show, rownames = FALSE, selection = "none",
                class = "stripe hover compact",
                options = list(pageLength = 20, dom = "tip",
                  language = list(paginate = list(previous="Anterior", "next"="Pr\u00f3ximo"))))
            )
          }
        )
        list(ui = ui_out, plot = boxplot)
      },

      # ── MANN-WHITNEY ────────────────────────────────────────────────────────
      "mann_whitney" = {
        atributo <- input$pesq_mw_y
        grupo    <- input$pesq_mw_grupo
        res <- calc_mann_whitney(df, atributo, grupo)
        if (!is.null(res$erro)) return(list(
          ui = div(class="stat-erro", HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', res$erro))),
          plot = NULL))

        nome_a <- nomes_atributos_regional[[atributo]] %||% atributo

        d_plot <- df[!is.na(df[[atributo]]) & !is.na(df[[grupo]]), ]
        d_plot <- d_plot[d_plot[[grupo]] %in% c(res$grupo1, res$grupo2), ]

        boxplot <- plot_ly(d_plot, x = ~get(grupo), y = ~get(atributo),
            type = "box", boxpoints = "all", jitter = 0.3,
            marker = list(color = "#6C3483", size = 5, opacity = 0.5),
            fillcolor = "rgba(108,52,131,0.12)",
            line = list(color = "#6C3483"),
            hovertemplate = "%{x}<br>%{y:.3f}<extra></extra>"
          ) %>%
          layout(
            xaxis = list(title = grupo),
            yaxis = list(title = nome_a, gridcolor = "#eee"),
            paper_bgcolor = "transparent", plot_bgcolor = "transparent",
            margin = list(t = 10), showlegend = FALSE
          )

        interp <- interpretar_mann_whitney(res, nome_a)

        resumo_df <- data.frame(
          Grupo = c(res$grupo1, res$grupo2),
          n = c(res$n1, res$n2),
          Mediana = c(res$mediana1, res$mediana2),
          stringsAsFactors = FALSE
        )
        names(resumo_df)[1] <- grupo

        ui_out <- tagList(
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-distribute-vertical"></i> Mann-Whitney \u2014 ',
                nome_a, ' por ', grupo))),
            withSpinner(plotlyOutput("pesq_plot_mann_whitney", height = "360px"), type = 8, color = "#6C3483"),
            div(class = "stat-interpretacao", HTML(interp))
          ),
          div(class = "result-card",
            div(class = "result-card-title", HTML('<i class="bi bi-table"></i> Resumo por Grupo')),
            datatable(resumo_df, rownames = FALSE, selection = "none",
              class = "stripe hover compact",
              options = list(pageLength = 5, dom = "t"))
          )
        )
        list(ui = ui_out, plot = boxplot)
      },

      "correlacao" = {
        vars <- input$pesq_corr_vars
        if (length(vars) < 2) {
          return(list(ui = div(class = "stat-erro",
            HTML('<i class="bi bi-exclamation-triangle-fill"></i> Selecione ao menos 2 atributos.')),
            plot = NULL))
        }
        res <- calc_correlacao(df, vars, input$pesq_corr_metodo %||% "pearson")
        if (!is.null(res$erro)) return(list(
          ui = div(class = "stat-erro", HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', res$erro))),
          plot = NULL))

        labels <- nomes_atributos_regional[res$vars]

        heatmap <- plot_ly(
          x = labels, y = labels, z = res$cor, type = "heatmap",
          colorscale = list(c(0, "#c0392b"), c(0.5, "#ffffff"), c(1, "#1e8449")),
          zmin = -1, zmax = 1,
          text = round(res$cor, 2), texttemplate = "%{text}",
          hovertemplate = "%{x} \u00d7 %{y}<br>r = %{z:.3f}<extra></extra>"
        ) %>% layout(
          margin = list(l = 120, b = 120),
          xaxis = list(tickangle = -45, tickfont = list(size = 10)),
          yaxis = list(tickfont = list(size = 10)),
          paper_bgcolor = "transparent"
        )

        tabela_pares <- NULL
        if (nrow(res$pares) > 0) {
          pares_show <- res$pares
          pares_show$var1 <- nomes_atributos_regional[pares_show$var1]
          pares_show$var2 <- nomes_atributos_regional[pares_show$var2]
          names(pares_show) <- c("Atributo 1","Atributo 2","r","p")
          tabela_pares <- datatable(pares_show, rownames = FALSE, selection = "none",
            class = "stripe hover compact",
            options = list(pageLength = 8, dom = "tip",
              language = list(paginate = list(previous="Anterior", "next"="Pr\u00f3ximo"))))
        }

        interp <- if (nrow(res$pares) > 0) {
          melhor <- res$pares[1, ]
          paste0(
            'Par mais correlacionado: <b>', nomes_atributos_regional[melhor$var1], '</b> \u00d7 ',
            '<b>', nomes_atributos_regional[melhor$var2], '</b>. ',
            interpretar_correlacao_par(melhor$r, melhor$p)
          )
        } else "Nenhum par com |r| \u2265 0.4 encontrado."

        tagList(
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-grid-3x3-gap"></i> Matriz de Correla\u00e7\u00e3o',
                           '<span class="stat-badge-n">n = ', res$n, '</span>'))),
            withSpinner(plotlyOutput("pesq_plot_correlacao", height = "480px"), type = 8, color = "#2c2c7a"),
            div(class = "stat-interpretacao", HTML(interp))
          ),
          if (!is.null(tabela_pares))
            div(class = "result-card",
              div(class = "result-card-title",
                HTML('<i class="bi bi-table"></i> Pares com Correla\u00e7\u00e3o Relevante (|r| \u2265 0.4)')),
              tabela_pares
            )
        ) -> ui_out

        list(ui = ui_out, plot = heatmap)
      },

      "regressao" = {
        x <- input$pesq_reg_x; y <- input$pesq_reg_y
        grau <- as.integer(input$pesq_reg_grau %||% 1)
        res <- calc_regressao(df, x, y, grau)
        if (!is.null(res$erro)) return(list(
          ui = div(class = "stat-erro", HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', res$erro))),
          plot = NULL))

        nome_x <- nomes_atributos_regional[[x]]
        nome_y <- nomes_atributos_regional[[y]]

        scatter <- plot_ly(res$dados, x = ~x, y = ~y, type = "scatter", mode = "markers",
          marker = list(color = "#2c2c7a", size = 9, opacity = 0.65),
          name = "Amostras", hovertemplate = paste0(nome_x, ": %{x}<br>", nome_y, ": %{y}<extra></extra>")
        ) %>%
          add_trace(x = res$x_seq, y = res$pred, type = "scatter", mode = "lines",
            line = list(color = "#e67e22", width = 3), name = "Ajuste") %>%
          layout(
            xaxis = list(title = nome_x, gridcolor = "#eee"),
            yaxis = list(title = nome_y, gridcolor = "#eee"),
            paper_bgcolor = "transparent", plot_bgcolor = "transparent",
            showlegend = TRUE, margin = list(t = 10)
          )

        eq <- if (grau == 1) {
          paste0("y = ", round(res$coef[1],3), " + ", round(res$coef[2],4), " \u00d7 x")
        } else {
          paste0("Polinomial de grau ", grau, " (R\u00b2 = ", round(res$r2,3), ")")
        }

        interp <- paste0(
          'Equa\u00e7\u00e3o: <b>', eq, '</b>. R\u00b2 = ', round(res$r2, 3),
          if (!is.na(res$p)) paste0(' (p ', if (res$p < 0.001) '&lt; 0.001' else paste0('= ', round(res$p,4)), ')') else '',
          '. ', interpretar_r2(res$r2)
        )

        ui_out <- tagList(
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-graph-up"></i> ', nome_x, ' \u00d7 ', nome_y,
                           '<span class="stat-badge-n">n = ', res$n, '</span>'))),
            withSpinner(plotlyOutput("pesq_plot_regressao", height = "420px"), type = 8, color = "#2c2c7a"),
            div(class = "stat-interpretacao", HTML(interp))
          )
        )
        list(ui = ui_out, plot = scatter)
      },

      "regressao_multipla" = {
        x_vars <- input$pesq_rm_x
        y_var  <- input$pesq_rm_y
        x_vars <- setdiff(x_vars, y_var)  # Y nunca pode ser também X

        res <- calc_regressao_multipla(df, x_vars, y_var)
        if (!is.null(res$erro)) return(list(
          ui = div(class = "stat-erro", HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', res$erro))),
          plot = NULL))

        nome_y <- nomes_atributos_regional[[y_var]]

        # --- Painel 1: Observado x Predito (com linha 1:1) ---
        faixa <- range(c(res$observado, res$predito))
        p1 <- plot_ly(x = res$observado, y = res$predito, type = "scatter", mode = "markers",
            marker = list(color = "#2c2c7a", size = 9, opacity = 0.65),
            name = "Amostras",
            hovertemplate = "Observado: %{x}<br>Predito: %{y}<extra></extra>") %>%
          add_trace(x = faixa, y = faixa, type = "scatter", mode = "lines",
            line = list(color = "#aaa", dash = "dot", width = 1.5),
            name = "1:1", showlegend = FALSE,
            hoverinfo = "skip") %>%
          layout(
            xaxis = list(title = paste0(nome_y, " observado"), gridcolor = "#eee"),
            yaxis = list(title = paste0(nome_y, " predito"), gridcolor = "#eee"),
            showlegend = FALSE
          )

        # --- Painel 2: coeficientes padronizados (betas) ---
        betas_df <- data.frame(
          variavel = res$x_vars,
          nome = sapply(res$x_vars, function(v) nomes_atributos_regional[[v]]),
          beta = unname(res$betas[res$x_vars]),
          stringsAsFactors = FALSE
        )
        betas_df <- betas_df[order(abs(betas_df$beta)), ]
        p2 <- plot_ly(betas_df, x = ~beta, y = ~factor(nome, levels = nome),
            type = "bar", orientation = "h",
            marker = list(color = ifelse(betas_df$beta >= 0, "#27ae60", "#c0392b")),
            hovertemplate = "%{y}: %{x:.3f}<extra></extra>"
          ) %>%
          layout(
            xaxis = list(title = "Coeficiente padronizado (\u03b2)", zeroline = TRUE, zerolinecolor = "#999"),
            yaxis = list(title = "")
          )

        combo <- subplot(p1, p2, nrows = 1, margin = 0.09, titleX = TRUE, titleY = TRUE) %>%
          layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
                 margin = list(t = 10), showlegend = FALSE)

        # --- Tabela de coeficientes (não-padronizados + beta + VIF) ---
        coefs_x <- res$coefs[res$coefs$variavel != "(Intercepto)", ]
        coefs_show <- data.frame(
          variavel = sapply(coefs_x$variavel, function(v) nomes_atributos_regional[[v]]),
          estimativa = coefs_x$estimativa,
          erro_padrao = coefs_x$erro_padrao,
          beta_padronizado = round(unname(res$betas[coefs_x$variavel]), 3),
          p_valor = coefs_x$p_valor,
          vif = ifelse(is.infinite(res$vif[coefs_x$variavel]), "\u221e",
                        as.character(unname(res$vif[coefs_x$variavel]))),
          stringsAsFactors = FALSE
        )
        names(coefs_show) <- c("Vari\u00e1vel", "Estimativa", "Erro Padr\u00e3o",
                                "\u03b2 (padron.)", "p-valor", "VIF")

        interp <- interpretar_regressao_multipla(res, nomes_atributos_regional, nome_y)

        ui_out <- tagList(
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-graph-up-arrow"></i> Regress\u00e3o M\u00faltipla \u2014 ', nome_y,
                           '<span class="stat-badge-n">n = ', res$n, '</span>'))),
            withSpinner(plotlyOutput("pesq_plot_regressao_multipla", height = "420px"), type = 8, color = "#2c2c7a"),
            div(class = "stat-interpretacao", HTML(interp))
          ),
          div(class = "result-card",
            div(class = "result-card-title",
              HTML('<i class="bi bi-table"></i> Coeficientes do Modelo')),
            datatable(coefs_show, rownames = FALSE, selection = "none",
              class = "stripe hover compact",
              options = list(pageLength = 10, dom = "tip",
                language = list(paginate = list(previous = "Anterior", "next" = "Pr\u00f3ximo"))))
          )
        )
        list(ui = ui_out, plot = combo)
      },

      "analise_trilha" = {
        x_vars <- input$pesq_trilha_x
        y_var  <- input$pesq_trilha_y
        x_vars <- setdiff(x_vars, y_var)

        res <- calc_analise_trilha(df, x_vars, y_var)
        if (!is.null(res$erro)) return(list(
          ui = div(class = "stat-erro", HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', res$erro))),
          plot = NULL))

        nome_y <- nomes_atributos_regional[[y_var]]
        nomes_x_vars <- sapply(res$x_vars, function(v) nomes_atributos_regional[[v]])

        resumo_plot <- res$resumo
        resumo_plot$nome <- nomes_x_vars[resumo_plot$variavel]

        grafico <- plot_ly(resumo_plot, x = ~factor(nome, levels = nome), y = ~correlacao_total,
            type = "bar", name = "Correla\u00e7\u00e3o total (r)", marker = list(color = "#7f8c8d"),
            hovertemplate = "%{x}<br>r = %{y:.3f}<extra></extra>") %>%
          add_trace(y = ~efeito_direto, name = "Efeito direto", marker = list(color = "#2c2c7a"),
            hovertemplate = "%{x}<br>direto = %{y:.3f}<extra></extra>") %>%
          add_trace(y = ~efeito_indireto, name = "Efeito indireto", marker = list(color = "#e67e22"),
            hovertemplate = "%{x}<br>indireto = %{y:.3f}<extra></extra>") %>%
          layout(
            barmode = "group",
            xaxis = list(title = ""),
            yaxis = list(title = "Coeficiente", zeroline = TRUE, zerolinecolor = "#999", gridcolor = "#eee"),
            legend = list(orientation = "h", x = 0, y = -0.18),
            paper_bgcolor = "transparent", plot_bgcolor = "transparent",
            margin = list(t = 10)
          )

        # --- Tabela resumo ---
        resumo_show <- data.frame(
          variavel = nomes_x_vars[res$resumo$variavel],
          correlacao_total = res$resumo$correlacao_total,
          efeito_direto = res$resumo$efeito_direto,
          efeito_indireto = res$resumo$efeito_indireto,
          residuo = res$resumo$residuo,
          stringsAsFactors = FALSE
        )
        names(resumo_show) <- c("Vari\u00e1vel", "Correla\u00e7\u00e3o total (r)", "Efeito direto",
                                 "Efeito indireto (total)", "Res\u00edduo")

        # --- Matriz de efeitos indiretos detalhados (Xi via Xj) ---
        ind_mat <- res$indiretos_detalhe
        rownames(ind_mat) <- nomes_x_vars[rownames(ind_mat)]
        colnames(ind_mat) <- nomes_x_vars[colnames(ind_mat)]
        ind_df <- as.data.frame(ind_mat)
        ind_df <- cbind(variavel = rownames(ind_df), ind_df)
        rownames(ind_df) <- NULL
        names(ind_df)[1] <- "Vari\u00e1vel"

        interp <- interpretar_trilha(res, nomes_atributos_regional, nome_y)

        ui_out <- tagList(
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-diagram-3-fill"></i> An\u00e1lise de Trilha \u2014 ', nome_y,
                           '<span class="stat-badge-n">n = ', res$n, '</span>'))),
            withSpinner(plotlyOutput("pesq_plot_trilha", height = "400px"), type = 8, color = "#2c2c7a"),
            div(class = "stat-interpretacao", HTML(interp))
          ),
          div(class = "result-card",
            div(class = "result-card-title",
              HTML('<i class="bi bi-table"></i> Decomposi\u00e7\u00e3o da Correla\u00e7\u00e3o')),
            datatable(resumo_show, rownames = FALSE, selection = "none",
              class = "stripe hover compact",
              options = list(pageLength = 10, dom = "tip",
                language = list(paginate = list(previous = "Anterior", "next" = "Pr\u00f3ximo"))))
          ),
          div(class = "result-card",
            div(class = "result-card-title",
              HTML('<i class="bi bi-grid-3x3"></i> Efeitos Indiretos Detalhados (linha via coluna)')),
            p(style = "font-size:11.5px; color:#888; margin-bottom:8px;",
              HTML('<i class="bi bi-info-circle"></i> Cada c\u00e9lula = efeito indireto da vari\u00e1vel ',
                   'da LINHA sobre ', nome_y, ', mediado pela vari\u00e1vel da COLUNA (r entre as duas ',
                   '\u00d7 efeito direto da coluna).')),
            div(style = "overflow-x:auto;",
              datatable(ind_df, rownames = FALSE, selection = "none",
                class = "stripe hover compact",
                options = list(pageLength = 10, dom = "tip", scrollX = TRUE,
                  language = list(paginate = list(previous = "Anterior", "next" = "Pr\u00f3ximo"))))
            )
          )
        )
        list(ui = ui_out, plot = grafico)
      },

      "anova" = {
        atributo <- input$pesq_anova_y
        grupo    <- input$pesq_anova_grupo
        res <- calc_anova(df, atributo, grupo)
        if (!is.null(res$erro)) return(list(
          ui = div(class = "stat-erro", HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', res$erro))),
          plot = NULL))

        nome_y     <- nomes_atributos_regional[[atributo]] %||% atributo
        nome_grupo <- grupo

        # Cores por LETRA de agrupamento — grupos com a mesma letra = mesma cor
        # Isso comunica visualmente quem difere de quem no pós-hoc
        cores_letras <- c(
          a = "#2D6A4F", b = "#1A5276", c = "#6C3483",
          d = "#B7791F", e = "#922B21", f = "#0E6655",
          g = "#4D5656", h = "#784212", i = "#154360",
          j = "#512E5F", k = "#117A65", l = "#1F618D"
        )
        # A cor do grupo = cor da primeira letra de sua combinação (ex: "ab" → cor de "a")
        cor_grupo <- function(grp) {
          primeira <- substr(res$letras[[grp]], 1, 1)
          unname(cores_letras[primeira] %||% "#888888")
        }

        # Boxplot colorido por grupo com letras anotadas
        grupos_lvl <- levels(res$dados$g)
        letras     <- res$letras

        # Cria uma trace por grupo para ter legenda e cores individuais
        traces <- lapply(grupos_lvl, function(grp) {
          vals <- res$dados$y[res$dados$g == grp]
          cor  <- cor_grupo(grp)
          list(
            y       = vals, type = "box", name = grp, boxpoints = "outliers",
            marker  = list(color = cor),
            fillcolor = paste0(cor, "33"),
            line    = list(color = cor),
            hovertemplate = paste0("<b>", grp, "</b><br>",
              "Mediana: %{median:.3f}<br>M\u00e9dia: ", round(mean(vals), 3),
              "<br>Letra: ", letras[[grp]], "<extra></extra>")
          )
        })

        # Anotações das letras acima de cada boxplot
        max_y <- max(res$dados$y, na.rm = TRUE)
        range_y <- diff(range(res$dados$y, na.rm = TRUE))
        anotacoes <- lapply(seq_along(grupos_lvl), function(i) {
          grp <- grupos_lvl[i]
          list(
            x = grp, y = max_y + range_y * 0.06,
            text = paste0("<b>", letras[[grp]], "</b>"),
            showarrow = FALSE,
            font = list(size = 14, color = cor_grupo(grp)),
            xref = "x", yref = "y"
          )
        })

        boxplot <- do.call(plot_ly, c(
          list(x = NULL),
          list(data = res$dados)
        ))
        for (tr in traces) {
          boxplot <- add_trace(boxplot,
            y = tr$y, type = tr$type, name = tr$name,
            boxpoints = tr$boxpoints,
            marker = tr$marker, fillcolor = tr$fillcolor,
            line = tr$line, hovertemplate = tr$hovertemplate
          )
        }
        boxplot <- boxplot %>% layout(
          xaxis   = list(title = nome_grupo, tickangle = -30, tickfont = list(size = 10)),
          yaxis   = list(title = nome_y, gridcolor = "#eee",
                          range = list(min(res$dados$y, na.rm=TRUE) - range_y*0.05,
                                       max_y + range_y * 0.14)),
          annotations = anotacoes,
          boxmode = "group",
          showlegend = FALSE,
          paper_bgcolor = "transparent", plot_bgcolor = "transparent",
          margin = list(t = 20, b = 60)
        )

        # Tabela de médias com letras
        tab_medias <- res$medias
        names(tab_medias) <- c("Grupo", "n", "M\u00e9dia", "Desvio Padr\u00e3o", "Letra", "M\u00e9dia \u00b1 Letra")

        metodo_txt <- if (res$metodo_posthoc == "Tukey") {
          "Teste de Tukey (HSD, \u03b1 = 5%)"
        } else {
          "Teste de Scott-Knott (\u03b1 = 5%)"
        }

        interp <- paste0(
          "ANOVA: F(", res$gl_trat, ", ", res$gl_res, ") = ", res$f_val,
          ", p ", if (res$p_global < 0.001) "&lt; 0.001" else paste0("= ", res$p_global),
          " (", res$k, " grupos, n = ", res$n, "). ",
          if (res$p_global < 0.05)
            paste0("H\u00e1 diferen\u00e7a significativa entre os grupos para ", nome_y,
                   ". P\u00f3s-hoc: <b>", metodo_txt, "</b>. ",
                   "M\u00e9dias seguidas de mesma letra n\u00e3o diferem entre si.")
          else
            paste0("N\u00e3o h\u00e1 diferen\u00e7a significativa entre os grupos (F n\u00e3o significativo).")
        )

        ui_out <- tagList(
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-bar-chart-line"></i> ', nome_y, ' por ', nome_grupo,
                           ' <span class="stat-badge-n">n = ', res$n, '</span>',
                           ' &nbsp;<span class="pesq-badge pesq-badge-info" style="font-size:9px;">',
                           res$metodo_posthoc, '</span>'))),
            withSpinner(plotlyOutput("pesq_plot_anova", height = "440px"), type = 8, color = "#2c2c7a"),
            div(class = "stat-interpretacao", HTML(interp))
          ),
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-table"></i> M\u00e9dias por Grupo \u2014 ', metodo_txt))),
            p(style = "font-size:11.5px; color:#888; margin-bottom:8px;",
              HTML('<i class="bi bi-info-circle"></i> M\u00e9dias seguidas da mesma letra n\u00e3o ',
                   'diferem entre si ao n\u00edvel de 5% de probabilidade.')),
            datatable(
              tab_medias[, c("Grupo","n","M\u00e9dia","Desvio Padr\u00e3o","Letra")],
              rownames = FALSE, selection = "none",
              class = "stripe hover compact",
              options = list(pageLength = 20, dom = "tip",
                language = list(paginate = list(previous="Anterior", "next"="Pr\u00f3ximo"))))
          )
        )
        list(ui = ui_out, plot = boxplot)
      },

      "pca" = {
        vars <- input$pesq_pca_vars
        if (length(vars) < 3) {
          return(list(ui = div(class = "stat-erro",
            HTML('<i class="bi bi-exclamation-triangle-fill"></i> Selecione ao menos 3 atributos.')),
            plot = NULL))
        }
        res <- calc_pca(df, vars)
        if (!is.null(res$erro)) return(list(
          ui = div(class = "stat-erro", HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', res$erro))),
          plot = NULL))

        scatter <- plot_ly(res$scores, x = ~PC1, y = ~PC2, type = "scatter", mode = "markers",
          marker = list(color = "#2c2c7a", size = 9, opacity = 0.65)
        ) %>% layout(
          xaxis = list(title = paste0("PC1 (", res$var_exp[1], "%)"), gridcolor = "#eee"),
          yaxis = list(title = paste0("PC2 (", res$var_exp[2], "%)"), gridcolor = "#eee"),
          paper_bgcolor = "transparent", plot_bgcolor = "transparent",
          margin = list(t = 10)
        )

        load_show <- res$loadings
        load_show$var <- nomes_atributos_regional[load_show$var]
        load_show$PC1 <- round(load_show$PC1, 3)
        load_show$PC2 <- round(load_show$PC2, 3)
        names(load_show) <- c("PC1","PC2","Atributo")
        load_show <- load_show[, c("Atributo","PC1","PC2")]
        load_show <- load_show[order(-abs(load_show$PC1)), ]

        interp <- paste0(
          "PC1 explica ", res$var_exp[1], "% e PC2 explica ", res$var_exp[2],
          "% da variabilidade total (", round(sum(res$var_exp),1), "% combinados). ",
          "Atributos com maior contribui\u00e7\u00e3o no PC1: <b>",
          paste(head(load_show$Atributo, 3), collapse = ", "), "</b>."
        )

        ui_out <- tagList(
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-bullseye"></i> PCA \u2014 Dispers\u00e3o das Amostras',
                           '<span class="stat-badge-n">n = ', res$n, '</span>'))),
            withSpinner(plotlyOutput("pesq_plot_pca", height = "420px"), type = 8, color = "#2c2c7a"),
            div(class = "stat-interpretacao", HTML(interp))
          ),
          div(class = "result-card",
            div(class = "result-card-title", HTML('<i class="bi bi-table"></i> Cargas dos Atributos (Loadings)')),
            datatable(load_show, rownames = FALSE, selection = "none",
              class = "stripe hover compact",
              options = list(pageLength = 10, dom = "tip",
                language = list(paginate = list(previous="Anterior", "next"="Pr\u00f3ximo"))))
          )
        )
        list(ui = ui_out, plot = scatter)
      },

      "cluster" = {
        vars <- input$pesq_cluster_vars
        k <- as.integer(input$pesq_cluster_k %||% 3)
        if (length(vars) < 2) {
          return(list(ui = div(class = "stat-erro",
            HTML('<i class="bi bi-exclamation-triangle-fill"></i> Selecione ao menos 2 atributos.')),
            plot = NULL))
        }
        res <- calc_cluster(df, vars, k)
        if (!is.null(res$erro)) return(list(
          ui = div(class = "stat-erro", HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', res$erro))),
          plot = NULL))

        scatter <- plot_ly(res$scores, x = ~PC1, y = ~PC2, color = ~cluster,
          colors = c("#2c2c7a","#27ae60","#e67e22","#c0392b","#8e44ad","#16a085"),
          type = "scatter", mode = "markers",
          marker = list(size = 9, opacity = 0.7)
        ) %>% layout(
          xaxis = list(title = paste0("PC1 (", res$var_exp[1], "%)"), gridcolor = "#eee"),
          yaxis = list(title = paste0("PC2 (", res$var_exp[2], "%)"), gridcolor = "#eee"),
          paper_bgcolor = "transparent", plot_bgcolor = "transparent",
          margin = list(t = 10), legend = list(title = list(text = "Grupo"))
        )

        perfil_show <- res$perfil
        names(perfil_show)[names(perfil_show) %in% names(nomes_atributos_regional)] <-
          nomes_atributos_regional[names(perfil_show)[names(perfil_show) %in% names(nomes_atributos_regional)]]
        perfil_show <- perfil_show[, c("cluster","n", setdiff(names(perfil_show), c("cluster","n")))]
        names(perfil_show)[1:2] <- c("Grupo","n")

        interp <- paste0(
          "Amostras separadas em <b>", k, " grupos</b> (n = ", res$n, ") com base em ",
          length(res$vars), " atributos. Visualiza\u00e7\u00e3o via PCA: PC1+PC2 explicam ",
          round(sum(res$var_exp),1), "% da varia\u00e7\u00e3o. Use a tabela de perfil m\u00e9dio para ",
          "caracterizar cada grupo (ex: 'Grupo 1 = solos \u00e1cidos com baixo P')."
        )

        ui_out <- tagList(
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-diagram-3"></i> Cluster (k-means, k=', k, ')',
                           '<span class="stat-badge-n">n = ', res$n, '</span>'))),
            withSpinner(plotlyOutput("pesq_plot_cluster", height = "420px"), type = 8, color = "#2c2c7a"),
            div(class = "stat-interpretacao", HTML(interp))
          ),
          div(class = "result-card",
            div(class = "result-card-title", HTML('<i class="bi bi-table"></i> Perfil M\u00e9dio por Grupo')),
            div(style = "overflow-x:auto;",
              datatable(perfil_show, rownames = FALSE, selection = "none",
                class = "stripe hover compact",
                options = list(pageLength = 10, dom = "tip", scrollX = TRUE,
                  language = list(paginate = list(previous="Anterior", "next"="Pr\u00f3ximo"))))
            )
          )
        )
        list(ui = ui_out, plot = scatter)
      },

      "cate_nelson" = {
        x <- input$pesq_cn_x
        res <- calc_cate_nelson(df, x, "produtividade",
          cultura = input$pesq_cn_cultura, y_critico_pct = input$pesq_cn_ycrit %||% 90,
          apenas_testemunha = isTRUE(input$pesq_cn_testemunha))
        if (!is.null(res$erro)) return(list(
          ui = div(class = "stat-erro", HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', res$erro))),
          plot = NULL))

        nome_x <- nomes_atributos_regional[[x]]
        cores_quad <- c(
          "Resposta esperada (X baixo, Y baixo)" = "#1e8449",
          "Sem resposta (X alto, Y alto)" = "#2c2c7a",
          "Discordante" = "#c0392b"
        )

        scatter <- plot_ly(res$dados, x = res$dados[[x]], y = ~y_rel, type = "scatter", mode = "markers",
          color = ~quadrante, colors = cores_quad,
          marker = list(size = 9, opacity = 0.7),
          hovertemplate = paste0(nome_x, ": %{x}<br>Produtividade relativa: %{y:.1f}%<extra></extra>")
        ) %>%
          layout(
            xaxis = list(title = nome_x, gridcolor = "#eee"),
            yaxis = list(title = "Produtividade relativa (%)", gridcolor = "#eee", range = c(0,105)),
            shapes = list(
              list(type="line", x0=res$xc, x1=res$xc, y0=0, y1=105, yref="y",
                   line=list(color="#888", dash="dash", width=2)),
              list(type="line", x0=min(res$dados[[x]]), x1=max(res$dados[[x]]), y0=res$y_critico, y1=res$y_critico,
                   line=list(color="#888", dash="dash", width=2))
            ),
            annotations = list(
              list(x = res$xc, y = 102, text = paste0("Xc = ", round(res$xc,2)),
                   showarrow = FALSE, font = list(size = 11, color="#555"))
            ),
            paper_bgcolor = "transparent", plot_bgcolor = "transparent",
            margin = list(t = 30), legend = list(orientation = "h", y = -0.25)
          )

        ui_out <- tagList(
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-bullseye"></i> Cate-Nelson \u2014 ', nome_x,
                           '<span class="stat-badge-n">n = ', res$n, '</span>'))),
            withSpinner(plotlyOutput("pesq_plot_cate_nelson", height = "440px"), type = 8, color = "#2c2c7a"),
            div(class = "stat-interpretacao", HTML(interpretar_cate_nelson(res, nome_x)))
          )
        )
        list(ui = ui_out, plot = scatter)
      },

      "linear_plato" = {
        x <- input$pesq_lp_x
        res <- calc_linear_plato(df, x, "produtividade",
          cultura = input$pesq_lp_cultura,
          apenas_testemunha = isTRUE(input$pesq_lp_testemunha))
        if (!is.null(res$erro)) return(list(
          ui = div(class = "stat-erro", HTML(paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', res$erro))),
          plot = NULL))

        nome_x <- nomes_atributos_regional[[x]]

        scatter <- plot_ly(res$dados, x = res$dados[[x]], y = res$dados[["produtividade"]],
          type = "scatter", mode = "markers",
          marker = list(color = "#2c2c7a", size = 9, opacity = 0.65), name = "Amostras",
          hovertemplate = paste0(nome_x, ": %{x}<br>Produtividade: %{y} kg/ha<extra></extra>")
        ) %>%
          add_trace(x = res$x_seq, y = res$pred, type = "scatter", mode = "lines",
            line = list(color = "#e67e22", width = 3), name = "Linear-Plat\u00f4") %>%
          layout(
            xaxis = list(title = nome_x, gridcolor = "#eee"),
            yaxis = list(title = "Produtividade (kg/ha)", gridcolor = "#eee"),
            shapes = list(
              list(type="line", x0=res$breakpoint, x1=res$breakpoint, y0=0, y1=max(res$dados$produtividade)*1.05,
                   yref="y", line=list(color="#888", dash="dash", width=2))
            ),
            annotations = list(
              list(x = res$breakpoint, y = max(res$dados$produtividade)*1.02,
                   text = paste0("Limiar = ", res$breakpoint),
                   showarrow = FALSE, font = list(size = 11, color="#555"))
            ),
            paper_bgcolor = "transparent", plot_bgcolor = "transparent",
            margin = list(t = 30), showlegend = TRUE
          )

        ui_out <- tagList(
          div(class = "result-card",
            div(class = "result-card-title",
              HTML(paste0('<i class="bi bi-graph-up-arrow"></i> Linear-Plat\u00f4 \u2014 ', nome_x,
                           '<span class="stat-badge-n">n = ', res$n, '</span>'))),
            withSpinner(plotlyOutput("pesq_plot_linear_plato", height = "440px"), type = 8, color = "#2c2c7a"),
            div(class = "stat-interpretacao", HTML(interpretar_linear_plato(res, nome_x)))
          )
        )
        list(ui = ui_out, plot = scatter)
      }
    )
  })

  # --- Renderiza o resultado (UI textual/tabelas) ---
  output$pesq_estat_resultado <- renderUI({
    if (!pesq_clicado()) return(NULL)
    res <- pesq_resultado_calc()
    res$ui
  })

  # --- Renderiza os gráficos plotly correspondentes ao tipo de análise ---
  output$pesq_plot_correlacao <- renderPlotly({
    if (!pesq_clicado()) return(plotly_vazio("Configure os par\u00e2metros e clique em Calcular"))
    res <- pesq_resultado_calc()
    if (is.null(res$plot)) return(plotly_vazio("Sem gr\u00e1fico para esta an\u00e1lise"))
    res$plot
  })
  output$pesq_plot_descritiva <- renderPlotly({
    if (!pesq_clicado()) return(plotly_vazio("Configure e clique em Gerar Relat\u00f3rio"))
    res <- pesq_resultado_calc()
    if (is.null(res$plot)) return(plotly_vazio("Sem gr\u00e1fico"))
    res$plot
  })
  output$pesq_plot_kruskal <- renderPlotly({
    if (!pesq_clicado()) return(plotly_vazio("Configure e clique em Calcular"))
    res <- pesq_resultado_calc()
    if (is.null(res$plot)) return(plotly_vazio("Sem gr\u00e1fico"))
    res$plot
  })
  output$pesq_plot_mann_whitney <- renderPlotly({
    if (!pesq_clicado()) return(plotly_vazio("Configure e clique em Calcular"))
    res <- pesq_resultado_calc()
    if (is.null(res$plot)) return(plotly_vazio("Sem gr\u00e1fico"))
    res$plot
  })
  output$pesq_plot_regressao <- renderPlotly({
    if (!pesq_clicado()) return(plotly_vazio("Configure os par\u00e2metros e clique em Calcular"))
    res <- pesq_resultado_calc()
    if (is.null(res$plot)) return(plotly_vazio("Sem gr\u00e1fico para esta an\u00e1lise"))
    res$plot
  })
  output$pesq_plot_regressao_multipla <- renderPlotly({
    if (!pesq_clicado()) return(plotly_vazio("Configure os par\u00e2metros e clique em Calcular"))
    res <- pesq_resultado_calc()
    if (is.null(res$plot)) return(plotly_vazio("Sem gr\u00e1fico para esta an\u00e1lise"))
    res$plot
  })
  output$pesq_plot_trilha <- renderPlotly({
    if (!pesq_clicado()) return(plotly_vazio("Configure os par\u00e2metros e clique em Calcular"))
    res <- pesq_resultado_calc()
    if (is.null(res$plot)) return(plotly_vazio("Sem gr\u00e1fico para esta an\u00e1lise"))
    res$plot
  })
  output$pesq_plot_anova <- renderPlotly({
    if (!pesq_clicado()) return(plotly_vazio("Configure os par\u00e2metros e clique em Calcular"))
    res <- pesq_resultado_calc()
    if (is.null(res$plot)) return(plotly_vazio("Sem gr\u00e1fico para esta an\u00e1lise"))
    res$plot
  })
  output$pesq_plot_pca <- renderPlotly({
    if (!pesq_clicado()) return(plotly_vazio("Configure os par\u00e2metros e clique em Calcular"))
    res <- pesq_resultado_calc()
    if (is.null(res$plot)) return(plotly_vazio("Sem gr\u00e1fico para esta an\u00e1lise"))
    res$plot
  })
  output$pesq_plot_cluster <- renderPlotly({
    if (!pesq_clicado()) return(plotly_vazio("Configure os par\u00e2metros e clique em Calcular"))
    res <- pesq_resultado_calc()
    if (is.null(res$plot)) return(plotly_vazio("Sem gr\u00e1fico para esta an\u00e1lise"))
    res$plot
  })
  output$pesq_plot_cate_nelson <- renderPlotly({
    if (!pesq_clicado()) return(plotly_vazio("Configure os par\u00e2metros e clique em Calcular"))
    res <- pesq_resultado_calc()
    if (is.null(res$plot)) return(plotly_vazio("Sem gr\u00e1fico para esta an\u00e1lise"))
    res$plot
  })
  output$pesq_plot_linear_plato <- renderPlotly({
    if (!pesq_clicado()) return(plotly_vazio("Configure os par\u00e2metros e clique em Calcular"))
    res <- pesq_resultado_calc()
    if (is.null(res$plot)) return(plotly_vazio("Sem gr\u00e1fico para esta an\u00e1lise"))
    res$plot
  })

}  # fim server

# --------------------------------------------------------------------------
# HELPERS PESQUISA
# --------------------------------------------------------------------------
pesq_card <- function(titulo, icone, itens, destaque = FALSE) {
  div(class = paste("pesq-unidade-card", if (destaque) "destaque" else ""),
    div(class = "pesq-unidade-tipo",
      HTML(paste0('<i class="bi ', icone, '"></i>')), titulo),
    lapply(names(itens), function(nm) {
      it <- itens[[nm]]
      div(class = "pesq-dose-row",
        span(class = "pesq-dose-label", HTML(nm)),
        span(
          span(class = "pesq-dose-valor", it$v),
          span(class = "pesq-dose-unit",  HTML(it$u))
        )
      )
    })
  )
}
# --------------------------------------------------------------------------
# HELPERS GERAIS
# --------------------------------------------------------------------------
placeholder_ui <- function(icone, titulo, texto) {
  div(class = "placeholder-msg",
    div(class = "placeholder-icon", HTML(paste0('<i class="bi bi-', icone, '"></i>'))),
    h4(class = "placeholder-title", titulo),
    p(class = "placeholder-text", texto)
  )
}

plotly_vazio <- function(msg = "Sem dados") {
  plot_ly() %>%
    layout(
      annotations = list(list(
        text = msg, x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 14, color = "#aaa")
      )),
      paper_bgcolor = "transparent",
      plot_bgcolor  = "transparent"
    )
}

# Operador null-coalesce
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Converte classe agronômica em classe CSS para colorir os cards de métrica
cls_metric <- function(classe) {
  switch(trimws(tolower(gsub("[^a-z0-9 ]", "", classe))),
    "muito baixo"        = "cl-muito-baixo",
    "baixo"              = "cl-baixo",
    "medio"              = "cl-medio",
    "moderadamente acido"= "cl-medio",
    "levemente acido"    = "cl-bom",
    "alto"               = "cl-alto",
    "muito alto toxico"  = "cl-muito-baixo",
    "muito alto"         = "cl-alto",
    "alcalino"           = "cl-alcalino",
    "cl-bom"
  )
}

# Card de métrica individual (usado na aba Solo, Clima e Banco Regional)
mk_metric <- function(valor, unidade, nome, classe, cor) {
  cls <- cls_metric(classe)
  div(class = paste("metric-item", cls),
    div(class = "metric-value", valor),
    div(class = "metric-unit", HTML(unidade)),
    div(class = "metric-name", HTML(nome)),
    span(class = paste("metric-class", cls), classe)
  )
}
