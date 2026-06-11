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
        texto = paste0('<i class="bi bi-exclamation-triangle-fill"></i> ', resultado$mensagem)
      ))
    }
  })
  
  # --------------------------------------------------------------------------
  # DADOS REATIVOS PRINCIPAIS
  # --------------------------------------------------------------------------
  dados_solo <- reactiveVal(NULL)
  dados_rec  <- reactiveVal(NULL)

  # --------------------------------------------------------------------------
  # CÁLCULO PRINCIPAL
  # --------------------------------------------------------------------------
  observeEvent(input$calcular, {
    
    req(input$ph, input$mo, input$p, input$k, input$ca, input$mg, input$al, input$h_al, input$argila)
    
    withProgress(message = "Calculando recomendações...", {
      
      # 1. Análise do solo
      solo <- analisar_solo(
        ph    = input$ph,
        mo    = input$mo,
        p     = input$p,
        k     = input$k,
        ca    = input$ca,
        mg    = input$mg,
        al    = input$al,
        h_al  = input$h_al,
        argila = input$argila,
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
        argila  = input$argila,
        al_sup  = input$al,
        ca_sup  = input$ca,
        ca_sub  = if (is.na(input$ca_sub)) NA else input$ca_sub,
        mg_sub  = if (is.na(input$mg_sub)) NA else input$mg_sub,
        al_sub  = if (is.na(input$al_sub)) NA else input$al_sub,
        k_sub   = if (is.na(input$k_sub))  NA else input$k_sub
      )
      
      # 3. Adubação
      p_interp <- interpretar_p(input$p, input$argila)
      k_interp <- interpretar_k(input$k, solo$ctc$valor)
      
      adub_rec <- recomendar_adubacao(
        cultura      = cultura_sel,
        p_nivel      = p_interp$nivel,
        k_nivel      = k_interp$nivel,
        produtividade = input$produtividade,
        mo           = input$mo,
        fase         = input$fase,
        n_anterior   = input$n_anterior
      )
      
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
        area      = input$area
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
    
    # Determinar classe CSS
    cls_metric <- function(classe) {
      switch(trimws(tolower(gsub("[^a-z0-9 ]", "", classe))),
        "muito baixo" = "cl-muito-baixo",
        "baixo"       = "cl-baixo",
        "medio"       = "cl-medio",
        "moderadamente acido" = "cl-medio",
        "levemente acido" = "cl-bom",
        "alto"        = "cl-alto",
        "muito alto toxico" = "cl-muito-baixo",
        "muito alto"  = "cl-alto",
        "alcalino"    = "cl-alcalino",
        "cl-bom"
      )
    }
    
    mk_metric <- function(valor, unidade, nome, classe, cor) {
      cls <- cls_metric(classe)
      div(class = paste("metric-item", cls),
        div(class = "metric-value", valor),
        div(class = "metric-unit", HTML(unidade)),
        div(class = "metric-name", HTML(nome)),
        span(class = paste("metric-class", cls), classe)
      )
    }
    
    tagList(
      # Parâmetros calculados
      div(class = "result-card",
        div(class = "result-card-title", HTML('<i class="bi bi-calculator"></i> Parâmetros Calculados')),
        div(class = "metrics-grid",
          mk_metric(solo$ctc$valor, "cmol<sub>c</sub> dm<sup>-3</sup>", "CTC", "Calculado", "#333"),
          mk_metric(solo$sb$valor,  "cmol<sub>c</sub> dm<sup>-3</sup>", "SB", "Calculado", "#333"),
          mk_metric(paste0(solo$v$valor, "%"), "", "Sat. Bases V%", solo$v$interp$classe, solo$v$interp$cor),
          mk_metric(paste0(solo$m$valor, "%"), "", "Sat. Al<sup>3+</sup> m%", solo$m$interp$classe, solo$m$interp$cor)
        )
      ),
      # pH e MO
      div(class = "result-card",
        div(class = "result-card-title", HTML('<i class="bi bi-droplet"></i> Reação do Solo & Matéria Orgânica')),
        div(class = "metrics-grid",
          mk_metric(solo$ph$valor, "H<sub>2</sub>O", "pH", solo$ph$interp$classe, solo$ph$interp$cor),
          mk_metric(solo$mo$valor, "dag kg<sup>-1</sup>", "M.O.", solo$mo$interp$classe, solo$mo$interp$cor),
          mk_metric(solo$al$valor, "cmol<sub>c</sub> dm<sup>-3</sup>", "Al<sup>3+</sup>", if(solo$al$valor > 0.5) "Alto" else "Baixo", "#333"),
          mk_metric(solo$h_al$valor, "cmol<sub>c</sub> dm<sup>-3</sup>", "(H+Al)", "Acidez Pot.", "#333")
        )
      ),
      # Macronutrientes
      div(class = "result-card",
        div(class = "result-card-title", HTML('<i class="bi bi-grid-3x3"></i> Macronutrientes')),
        div(class = "metrics-grid",
          mk_metric(solo$ca$valor, "cmol<sub>c</sub> dm<sup>-3</sup>", "Ca<sup>2+</sup>", solo$ca$interp$classe, solo$ca$interp$cor),
          mk_metric(solo$mg$valor, "cmol<sub>c</sub> dm<sup>-3</sup>", "Mg<sup>2+</sup>", solo$mg$interp$classe, solo$mg$interp$cor),
          mk_metric(solo$k$valor,  "mg dm<sup>-3</sup>", "K<sup>+</sup>", solo$k$interp$classe, solo$k$interp$cor),
          mk_metric(solo$p$valor,  "mg dm<sup>-3</sup>", "P-Mehlich", solo$p$interp$classe, solo$p$interp$cor)
        )
      ),
      # Micronutrientes (se houver)
      if (!all(sapply(solo$micro, is.null))) {
        div(class = "result-card",
          div(class = "result-card-title", HTML('<i class="bi bi-atom"></i> Micronutrientes')),
          div(class = "metrics-grid",
            lapply(names(solo$micro), function(nm) {
              m <- solo$micro[[nm]]
              if (!is.null(m)) {
                mk_metric(m$valor, "mg dm<sup>-3</sup>", nm, m$interp$classe, m$interp$cor)
              }
            })
          )
        )
      }
    )
  })
  
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
          HTML(paste0('<i class="bi bi-seedling"></i> ', culturas_lista[[cult]], " | Manual: ", toupper(man)))
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
      normalizar(solo$mo$valor, 0, 5),
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
