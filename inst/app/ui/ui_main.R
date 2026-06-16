# ==============================================================================
# UI PRINCIPAL - ADUBO CERTO
# ==============================================================================

ui <- fluidPage(
  # Metadados e CSS
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", 
              href = "https://fonts.googleapis.com/css2?family=DM+Serif+Display:ital@0;1&family=DM+Sans:wght@300;400;500;600&family=DM+Mono:wght@400;500&display=swap"),
    tags$link(rel = "stylesheet", 
              href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"),
    tags$style(HTML(estilos_css()))
  ),
  
  # HEADER
  div(class = "app-header",
    div(class = "header-inner",
      div(class = "logo-group",
        {
          www_local <- file.path(getwd(), "www")
          www_pkg   <- tryCatch(
            system.file("app", "www", package = "GEMSFertilizer"),
            error = function(e) ""
          )
          tem_svg <- file.exists(file.path(www_local, "logo_gems.svg")) ||
                     (nchar(www_pkg) > 0 && file.exists(file.path(www_pkg, "logo_gems.svg")))
          tem_png <- file.exists(file.path(www_local, "logo_gems.png")) ||
                     (nchar(www_pkg) > 0 && file.exists(file.path(www_pkg, "logo_gems.png")))

          if (tem_svg || tem_png) {
            ext <- if (tem_svg) "svg" else "png"
            tags$img(src = paste0("logo_gems.", ext), class = "logo-img", alt = "GEMS Logo")
          } else {
            div(class = "logo-icon", HTML('<i class="bi bi-gem"></i>'))
          }
        },
        div(
          h1("GEMS_Fertilizer", class = "app-title"),
          p("Sistema de Recomendação de Adubação e Calagem", class = "app-subtitle")
        )
      ),
      div(class = "header-badges",
        span(class = "badge-manual", HTML('<i class="bi bi-book"></i> MG 5ª Aprox.')),
        span(class = "badge-manual", HTML('<i class="bi bi-book"></i> Manual Sergipe'))
      )
    )
  ),
  
  # CONTEÚDO PRINCIPAL
  div(class = "main-container",
    
    # PAINEL ESQUERDO - ENTRADA DE DADOS
    div(class = "panel-left",
      
      # ---- CULTURA E CONFIGURAÇÕES ----
      div(class = "card-section",
        div(class = "card-header-custom",
          HTML('<i class="bi bi-seedling"></i>'),
          span("Cultura & Configurações")
        ),
        div(class = "card-body-custom",
          
          div(class = "form-row-2",
            div(class = "form-group-custom",
              label_custom("Cultura", "bi-flower2"),
              selectInput("cultura", NULL,
                choices = c(
                  "Milho"          = "milho",
                  "Feijão"         = "feijao",
                  "Cana-de-açúcar" = "cana",
                  "Arroz"          = "arroz",
                  "Mandioca"       = "mandioca",
                  "Amendoim"       = "amendoim",
                  "Sorgo"          = "sorgo",
                  "Pastagem/Capim" = "pastagem",
                  "Abacaxi"        = "abacaxi"
                ),
                width = "100%"
              )
            ),
            div(class = "form-group-custom",
              label_custom("Produtividade Esperada", "bi-graph-up"),
              selectInput("produtividade", NULL,
                choices = c("Baixa" = "baixa", "Média" = "media", "Alta" = "alta"),
                selected = "media", width = "100%"
              )
            )
          ),
          
          div(class = "form-row-2",
            div(class = "form-group-custom",
              label_custom("Manual de Referência", "bi-journal-bookmark"),
              selectInput("manual", NULL,
                choices = c("Minas Gerais (5ª Aprox.)" = "mg", "Sergipe (EMBRAPA/UFS)" = "se"),
                width = "100%"
              )
            ),
            div(class = "form-group-custom",
              label_custom("Área (hectares)", "bi-map"),
              numericInput("area", NULL, value = 1, min = 0.1, max = 10000, step = 0.5, width = "100%")
            )
          ),
          
          div(class = "form-row-2",
            div(class = "form-group-custom",
              label_custom("Fase da Cultura", "bi-calendar3"),
              selectInput("fase", NULL,
                choices = c("Plantio/Implantação" = "plantio", "Cobertura/Soca" = "soca"),
                width = "100%"
              )
            ),
            div(class = "form-group-custom",
              label_custom("Cultivo Anterior", "bi-arrow-counterclockwise"),
              selectInput("n_anterior", NULL,
                choices = c("Nenhum/Pousio" = "nenhum", "Leguminosa" = "leguminosa", "Gramínea" = "graminea"),
                width = "100%"
              )
            )
          )
        )
      ),
      
      # ---- ANÁLISE DE SOLO ----
      div(class = "card-section",
        div(class = "card-header-custom",
          HTML('<i class="bi bi-layers"></i>'),
          span("Análise de Solo")
        ),
        div(class = "card-body-custom",
          
          p(class = "section-hint", HTML('<i class="bi bi-info-circle"></i> Informe os valores da análise de solo. Deixe em branco os não disponíveis.')),
          
          # LINHA 1 - pH e MO
          div(class = "nutrient-group",
            div(class = "nutrient-group-title", HTML('<i class="bi bi-droplet"></i> Reação do Solo')),
            div(class = "form-row-3",
              div(class = "form-group-custom",
                label_custom("pH (H<sub>2</sub>O)", ""),
                numericInput("ph", NULL, value = 5.5, min = 3.5, max = 9.0, step = 0.1, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("M.O. (dag kg<sup>-1</sup>)", ""),
                numericInput("mo", NULL, value = 2.5, min = 0, max = 15, step = 0.1, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("Argila (%)", ""),
                numericInput("argila", NULL, value = 30, min = 0, max = 100, step = 5, width = "100%")
              )
            )
          ),
          
          # LINHA 2 - Macronutrientes cátions
          div(class = "nutrient-group",
            div(class = "nutrient-group-title", HTML('<i class="bi bi-grid-3x3"></i> Macronutrientes — Cátions (cmol<sub>c</sub> dm<sup>-3</sup>)')),
            div(class = "form-row-3",
              div(class = "form-group-custom",
                label_custom("Ca<sup>2+</sup> (cmol<sub>c</sub> dm<sup>-3</sup>)", ""),
                numericInput("ca", NULL, value = 2.5, min = 0, max = 20, step = 0.1, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("Mg<sup>2+</sup> (cmol<sub>c</sub> dm<sup>-3</sup>)", ""),
                numericInput("mg", NULL, value = 0.8, min = 0, max = 10, step = 0.1, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("Al<sup>3+</sup> (cmol<sub>c</sub> dm<sup>-3</sup>)", ""),
                numericInput("al", NULL, value = 0.2, min = 0, max = 5, step = 0.1, width = "100%")
              )
            ),
            div(class = "form-row-3",
              div(class = "form-group-custom",
                label_custom("(H+Al) (cmol<sub>c</sub> dm<sup>-3</sup>)", ""),
                numericInput("h_al", NULL, value = 4.5, min = 0, max = 20, step = 0.1, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("K<sup>+</sup> (mg dm<sup>-3</sup>)", ""),
                numericInput("k", NULL, value = 80, min = 0, max = 1000, step = 5, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("P-Mehlich (mg dm<sup>-3</sup>)", ""),
                numericInput("p", NULL, value = 5.0, min = 0, max = 200, step = 0.5, width = "100%")
              )
            )
          ),
          
          # LINHA 3 - Enxofre e Micronutrientes
          div(class = "nutrient-group",
            div(class = "nutrient-group-title", HTML('<i class="bi bi-atom"></i> Enxofre & Micronutrientes (mg dm<sup>-3</sup>)')),
            div(class = "form-row-3",
              div(class = "form-group-custom",
                label_custom("S-SO<sub>4</sub><sup>2-</sup> (mg dm<sup>-3</sup>)", ""),
                numericInput("s", NULL, value = NA, min = 0, max = 100, step = 0.5, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("B — água quente (mg dm<sup>-3</sup>)", ""),
                numericInput("b", NULL, value = NA, min = 0, max = 5, step = 0.05, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("Cu-DTPA (mg dm<sup>-3</sup>)", ""),
                numericInput("cu", NULL, value = NA, min = 0, max = 20, step = 0.1, width = "100%")
              )
            ),
            div(class = "form-row-3",
              div(class = "form-group-custom",
                label_custom("Fe-DTPA (mg dm<sup>-3</sup>)", ""),
                numericInput("fe", NULL, value = NA, min = 0, max = 500, step = 1, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("Mn-DTPA (mg dm<sup>-3</sup>)", ""),
                numericInput("mn", NULL, value = NA, min = 0, max = 100, step = 0.5, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("Zn-DTPA (mg dm<sup>-3</sup>)", ""),
                numericInput("zn", NULL, value = NA, min = 0, max = 50, step = 0.1, width = "100%")
              )
            )
          )
        )
      ),
      
      # ---- CONFIGURAÇÃO DE CALAGEM ----
      div(class = "card-section",
        div(class = "card-header-custom",
          HTML('<i class="bi bi-calculator"></i>'),
          span("Calagem & Gessagem")
        ),
        div(class = "card-body-custom",
          div(class = "form-row-2",
            div(class = "form-group-custom",
              label_custom("Método de Calagem", "bi-gear"),
              selectInput("metodo_calagem", NULL,
                choices = c(
                  "Saturação por Bases (V%)" = "v",
                  "Neutralização Al³⁺ + Ca+Mg" = "al",
                  "Tampão SMP" = "smp",
                  "Comparar Todos" = "todos"
                ),
                width = "100%"
              )
            ),
            div(class = "form-group-custom",
              label_custom("PRNT do Calcário (%)", "bi-percent"),
              numericInput("prnt", NULL, value = 70, min = 30, max = 100, step = 5, width = "100%")
            )
          ),
          div(class = "form-row-2",
            div(class = "form-group-custom",
              label_custom("pH<sub>SMP</sub> (se disponível)", "bi-flask"),
              numericInput("ph_smp", NULL, value = NA, min = 4.0, max = 7.5, step = 0.1, width = "100%")
            ),
            div(class = "form-group-custom",
              label_custom("Profundidade (cm)", "bi-arrow-down"),
              selectInput("profundidade", NULL,
                choices = c("0–20 cm" = 20, "0–30 cm" = 30, "0–40 cm" = 40),
                width = "100%"
              )
            )
          ),

          # --- SUBSOLO 20-40 cm ---
          div(class = "nutrient-group", style = "margin-top: 8px;",
            div(class = "nutrient-group-title",
              HTML('<i class="bi bi-arrow-down-square"></i> Análise de Subsolo 20–40 cm'),
              span(style = "font-weight:400; color:#aaa; margin-left:6px;",
                   "(opcional — habilita Caires & Guimarães 2018)")
            ),
            p(style = "font-size:11px; color:#888; margin-bottom:10px;",
              HTML('<i class="bi bi-info-circle"></i> Preencha para usar a nova f\u00f3rmula de gessagem baseada na satura\u00e7\u00e3o por Ca\u00b2\u207a na CTC efetiva (m\u00e9todo mais preciso). Unidades: cmol\u1d04 dm\u207b\u00b3.')),
            div(class = "form-row-2",
              div(class = "form-group-custom",
                label_custom("Ca<sup>2+</sup> (20–40 cm)", ""),
                numericInput("ca_sub", NULL, value = NA, min = 0, max = 20, step = 0.1, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("Mg<sup>2+</sup> (20–40 cm)", ""),
                numericInput("mg_sub", NULL, value = NA, min = 0, max = 10, step = 0.1, width = "100%")
              )
            ),
            div(class = "form-row-2",
              div(class = "form-group-custom",
                label_custom("Al<sup>3+</sup> (20–40 cm)", ""),
                numericInput("al_sub", NULL, value = NA, min = 0, max = 5, step = 0.1, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("K<sup>+</sup> (20–40 cm) mg dm<sup>-3</sup>", ""),
                numericInput("k_sub", NULL, value = NA, min = 0, max = 500, step = 5, width = "100%")
              )
            )
          )
        )
      ),
      
      # BOTÃO CALCULAR
      div(class = "btn-container",
        actionButton("calcular", 
          HTML('<i class="bi bi-play-fill"></i>&nbsp; CALCULAR RECOMENDAÇÃO'),
          class = "btn-calcular",
          width = "100%"
        )
      )
    ),
    
    # PAINEL DIREITO - RESULTADOS
    div(class = "panel-right",
      
      # TABS DE RESULTADO
      div(class = "results-tabs",
        div(class = "tab-list",
          actionButton("tab_solo",     HTML('<i class="bi bi-layers"></i> Solo'),          class = "tab-btn active-tab"),
          actionButton("tab_calagem",  HTML('<i class="bi bi-droplet-fill"></i> Calagem'),  class = "tab-btn"),
          actionButton("tab_adubacao", HTML('<i class="bi bi-bag-fill"></i> Aduba\u00e7\u00e3o'), class = "tab-btn"),
          actionButton("tab_financeiro", HTML('<i class="bi bi-currency-dollar"></i> Custos'), class = "tab-btn"),
          actionButton("tab_graficos", HTML('<i class="bi bi-bar-chart-fill"></i> Gr\u00e1ficos'), class = "tab-btn"),
          actionButton("tab_pesquisa", HTML('<i class="bi bi-journal-medical"></i> Pesquisa'), class = "tab-btn"),
          actionButton("tab_regional", HTML('<i class="bi bi-map-fill"></i> Banco Regional'), class = "tab-btn")
        )
      ),
      
      # PAINEL ANÁLISE DE SOLO
      div(id = "painel_solo", class = "results-panel active-panel",
        uiOutput("resultado_solo")
      ),
      
      # PAINEL CALAGEM
      div(id = "painel_calagem", class = "results-panel",
        uiOutput("resultado_calagem")
      ),
      
      # PAINEL ADUBAÇÃO
      div(id = "painel_adubacao", class = "results-panel",
        uiOutput("resultado_adubacao")
      ),
      
      # PAINEL FINANCEIRO
      div(id = "painel_financeiro", class = "results-panel",
        # Barra de preços: CEPEA + status
        div(class = "price-toolbar",
          div(class = "price-toolbar-left",
            HTML('<i class="bi bi-tag-fill"></i>'),
            span(class = "price-toolbar-title", "Gestão de Preços de Fertilizantes"),
            uiOutput("preco_status_badge", inline = TRUE)
          ),
          div(class = "price-toolbar-right",
            actionButton("btn_cepea",
              HTML('<i class="bi bi-cloud-download"></i> Buscar preços CEPEA/AgroLink'),
              class = "btn-cepea"
            ),
            actionButton("btn_menor_preco_modal",
              HTML('<i class="bi bi-search"></i> Menor Pre\u00e7o Brasil'),
              class = "btn-menor-preco"
            ),
            actionButton("btn_salvar_precos",
              HTML('<i class="bi bi-floppy"></i> Salvar pre\u00e7os'),
              class = "btn-salvar-precos"
            )
          )
        ),
        # Feedback do scraping CEPEA
        uiOutput("cepea_feedback"),
        # Painel Menor Preço Brasil
        uiOutput("menor_preco_painel"),
        # Tabela editável de preços
        div(class = "result-card",
          div(class = "result-card-title",
            HTML('<i class="bi bi-pencil-square"></i> Preços por Produto — edite diretamente na tabela')
          ),
          p(style = "font-size:11.5px;color:#888;margin-bottom:10px;",
            HTML('<i class="bi bi-info-circle"></i> Valores em R$/kg de produto comercial. Clique em uma célula da coluna <b>Preço (R$/kg)</b> para editar. Clique em <b>Salvar preços</b> para persistir no arquivo local.')),
          DTOutput("tabela_precos_edit")
        ),
        # Resultado financeiro calculado
        uiOutput("resultado_financeiro")
      ),
      
      # PAINEL GRÁFICOS
      div(id = "painel_graficos", class = "results-panel",
        div(class = "charts-grid",
          div(class = "chart-card",
            h4(class = "chart-title", HTML('<i class="bi bi-activity"></i> Radar de Fertilidade')),
            withSpinner(plotlyOutput("grafico_radar", height = "320px"), type = 8, color = "#3a7d44")
          ),
          div(class = "chart-card",
            h4(class = "chart-title", HTML('<i class="bi bi-bar-chart"></i> Macronutrientes')),
            withSpinner(plotlyOutput("grafico_macro", height = "320px"), type = 8, color = "#3a7d44")
          ),
          div(class = "chart-card",
            h4(class = "chart-title", HTML('<i class="bi bi-speedometer2"></i> Saturações (V% e m%)')),
            withSpinner(plotlyOutput("grafico_saturacao", height = "320px"), type = 8, color = "#3a7d44")
          ),
          div(class = "chart-card",
            h4(class = "chart-title", HTML('<i class="bi bi-stars"></i> Micronutrientes')),
            withSpinner(plotlyOutput("grafico_micro", height = "320px"), type = 8, color = "#3a7d44")
          )
        )
      ),
      # PAINEL PESQUISA
      div(id = "painel_pesquisa", class = "results-panel",

        # Cabeçalho modo pesquisa
        div(class = "pesq-header",
          div(class = "pesq-header-icon", HTML('<i class="bi bi-journal-medical"></i>')),
          div(
            h3(class = "pesq-header-title", "Modo Pesquisa Agr\u00edcola"),
            p(class = "pesq-header-sub",
              "Doses experimentais, an\u00e1lises estat\u00edsticas e calibra\u00e7\u00e3o de limiares cr\u00edticos")
          )
        ),

        # Sub-navegação: Modo Experimental | Análises Estatísticas
        div(class = "pesq-subtabs",
          selectInput("pesq_submodulo", NULL, width = "320px",
            choices = c(
              "\U0001F9EA Modo Experimental (doses)" = "experimental",
              "\U0001F4CA An\u00e1lises Estat\u00edsticas"     = "estatistica"
            )
          )
        ),

        # ======================================================================
        # SUB-PAINEL: MODO EXPERIMENTAL (doses)
        # ======================================================================
        conditionalPanel(
          condition = "input.pesq_submodulo == 'experimental'",

        # Configurações do experimento
        div(class = "result-card",
          div(class = "result-card-title",
            HTML('<i class="bi bi-sliders"></i> Configura\u00e7\u00e3o do Experimento')),

          div(class = "pesq-config-grid",

            # Tipo de unidade
            div(class = "form-group-custom",
              label_custom("Unidade experimental", "bi-ui-checks-grid"),
              selectInput("pesq_unidade", NULL, width = "100%",
                choices = c(
                  "Metro linear de sulco"  = "metro_linear",
                  "Cova / Berço"           = "cova",
                  "Vaso / Lisímetro"       = "vaso",
                  "Parcela (m²)"           = "parcela"
                )
              )
            ),

            # Espaçamento entre fileiras
            div(class = "form-group-custom",
              label_custom("Espa\u00e7amento entre fileiras (m)", "bi-arrows-expand"),
              numericInput("pesq_espacamento", NULL,
                value = 0.5, min = 0.1, max = 5.0, step = 0.05, width = "100%")
            ),

            # Plantas por metro / cova
            div(class = "form-group-custom",
              label_custom("Plantas por metro linear", "bi-flower1"),
              numericInput("pesq_plantas_metro", NULL,
                value = 5, min = 1, max = 100, step = 1, width = "100%")
            ),

            # Volume do vaso
            div(class = "form-group-custom",
              label_custom("Volume do vaso / lisímetro (dm\u00b3)", "bi-box"),
              numericInput("pesq_vol_vaso", NULL,
                value = 10, min = 0.5, max = 500, step = 0.5, width = "100%")
            ),

            # Dimensões da parcela
            div(class = "form-group-custom",
              label_custom("Largura da parcela (m)", "bi-arrows"),
              numericInput("pesq_largura_parcela", NULL,
                value = 3, min = 0.5, max = 50, step = 0.5, width = "100%")
            ),
            div(class = "form-group-custom",
              label_custom("Comprimento da parcela (m)", "bi-arrows"),
              numericInput("pesq_comp_parcela", NULL,
                value = 5, min = 0.5, max = 100, step = 0.5, width = "100%")
            ),

            # N repetições
            div(class = "form-group-custom",
              label_custom("N\u00famero de repeti\u00e7\u00f5es (blocos)", "bi-grid"),
              numericInput("pesq_repeticoes", NULL,
                value = 4, min = 1, max = 20, step = 1, width = "100%")
            ),

            # N tratamentos
            div(class = "form-group-custom",
              label_custom("N\u00famero de tratamentos", "bi-list-ol"),
              numericInput("pesq_tratamentos", NULL,
                value = 5, min = 1, max = 100, step = 1, width = "100%")
            )
          ),

          # Doses personalizadas (permite entrar manualmente)
          div(style = "margin-top:14px; padding-top:12px; border-top:1px solid #eee;",
            div(class = "result-card-title", style = "margin-bottom:10px;",
              HTML('<i class="bi bi-pencil"></i> Doses NPK de refer\u00eancia (kg ha\u207b\u00b9)')),
            p(style = "font-size:11.5px; color:#888; margin-bottom:10px;",
              HTML('<i class="bi bi-info-circle"></i> Preenchido automaticamente pela aba Aduba\u00e7\u00e3o. Edite para simular tratamentos espec\u00edficos.')),
            div(class = "form-row-3",
              div(class = "form-group-custom",
                label_custom("N (kg ha\u207b\u00b9)", ""),
                numericInput("pesq_dose_n", NULL,
                  value = 100, min = 0, max = 1000, step = 10, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("P\u2082O\u2085 (kg ha\u207b\u00b9)", ""),
                numericInput("pesq_dose_p", NULL,
                  value = 80, min = 0, max = 1000, step = 10, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("K\u2082O (kg ha\u207b\u00b9)", ""),
                numericInput("pesq_dose_k", NULL,
                  value = 60, min = 0, max = 1000, step = 10, width = "100%")
              )
            ),
            div(class = "form-row-3",
              div(class = "form-group-custom",
                label_custom("Calcário (t ha\u207b\u00b9)", ""),
                numericInput("pesq_dose_cal", NULL,
                  value = 2, min = 0, max = 20, step = 0.5, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("Gesso (t ha\u207b\u00b9)", ""),
                numericInput("pesq_dose_gesso", NULL,
                  value = 0, min = 0, max = 10, step = 0.5, width = "100%")
              ),
              div(class = "form-group-custom",
                label_custom("Fonte NPK principal", ""),
                selectInput("pesq_fonte_npk", NULL, width = "100%",
                  choices = c(
                    "NPK 04-14-08"  = "04-14-08",
                    "NPK 05-25-15"  = "05-25-15",
                    "NPK 08-28-16"  = "08-28-16",
                    "NPK 10-10-10"  = "10-10-10",
                    "NPK 12-06-12"  = "12-06-12",
                    "NPK 15-15-15"  = "15-15-15",
                    "NPK 20-05-20"  = "20-05-20",
                    "Fontes separadas" = "separado"
                  )
                )
              )
            ),
            div(class = "btn-container", style = "padding: 8px 0 0;",
              actionButton("btn_calcular_pesq",
                HTML('<i class="bi bi-calculator-fill"></i>&nbsp; CALCULAR DOSES EXPERIMENTAIS'),
                class = "btn-calcular",
                width = "100%"
              )
            )
          )
        ),

        # Resultados pesquisa
        uiOutput("resultado_pesquisa")

        ),  # fim conditionalPanel experimental

        # ======================================================================
        # SUB-PAINEL: ANÁLISES ESTATÍSTICAS
        # ======================================================================
        conditionalPanel(
          condition = "input.pesq_submodulo == 'estatistica'",

          uiOutput("pesq_estat_conteudo")
        )
      ),

      # PAINEL BANCO REGIONAL
      div(id = "painel_regional", class = "results-panel",

        # Cabeçalho
        div(class = "pesq-header", style = "background: linear-gradient(135deg, #0d3349, #1a5276);",
          div(class = "pesq-header-icon", HTML('<i class="bi bi-map-fill"></i>')),
          div(
            h3(class = "pesq-header-title", "Banco Regional de Fertilidade"),
            p(class = "pesq-header-sub",
              "Estat\u00edsticas, mapas e benchmarking a partir de an\u00e1lises hist\u00f3ricas da regi\u00e3o")
          )
        ),

        # Upload do arquivo
        div(class = "result-card",
          div(class = "result-card-title",
            HTML('<i class="bi bi-cloud-upload"></i> Carregar Banco de Dados')),
          p(style = "font-size:12px; color:#888; margin-bottom:10px;",
            HTML('<i class="bi bi-info-circle"></i> Envie o arquivo <b>template_banco_regional.xlsx</b> preenchido ',
                 '(aba "Dados"). Requer os pacotes <code>readxl</code> e, para mapas, ',
                 '<code>geobr</code>, <code>sf</code> e <code>leaflet</code>.')),
          a(href = "template_banco_regional.xlsx", download = NA, target = "_blank",
            class = "btn-cepea", style = "display:inline-flex; margin-bottom:10px; margin-right:8px;",
            HTML('<i class="bi bi-download"></i> Baixar template em branco')),
          a(href = "banco_regional_EXEMPLO.xlsx", download = NA, target = "_blank",
            class = "btn-cepea", style = "display:inline-flex; margin-bottom:10px; background:#6c3483; border-color:#6c3483;",
            HTML('<i class="bi bi-stars"></i> Baixar planilha de EXEMPLO (140 amostras)')),
          fileInput("regional_file", NULL, accept = c(".xlsx"), width = "100%",
            buttonLabel = HTML('<i class="bi bi-folder2-open"></i> Escolher arquivo'),
            placeholder = "Nenhum arquivo selecionado"),
          uiOutput("regional_status")
        ),

        # Conteúdo (aparece após upload)
        uiOutput("regional_conteudo")
      )
    )
  ),
  div(class = "app-footer",
    p(HTML('<i class="bi bi-info-circle"></i> Baseado no <b>Manual de Adubação e Calagem para Minas Gerais - 5ª Aproximação (2023)</b> e no <b>Manual de Recomendações de Adubação e Calagem do Estado de Sergipe (EMBRAPA/UFS)</b>. Para uso técnico e orientativo. Consulte sempre um Engenheiro Agrônomo.'))
  ),
  
  # JavaScript para controle de tabs
  tags$script(HTML(js_tabs()))
)

# Funcoes auxiliares (label_custom, estilos_css, js_tabs) estao em ui/ui_helpers.R
