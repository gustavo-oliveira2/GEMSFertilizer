# ==============================================================================
# UI HELPERS - Funções auxiliares (devem ser carregadas ANTES de ui_main.R)
# ==============================================================================

# Helper para labels customizados — aceita HTML para sub/superscrito
label_custom <- function(texto_html, icone) {
  div(class = "input-label",
    if (nchar(icone) > 0) HTML(paste0('<i class="bi ', icone, '"></i> ')),
    HTML(texto_html)
  )
}

# CSS completo do app
estilos_css <- function() {
  "
  :root {
    --verde-escuro: #1b4332;
    --verde-medio: #2d6a4f;
    --verde-claro: #52b788;
    --verde-palido: #d8f3dc;
    --terra: #8b5e3c;
    --terra-claro: #f0e6d3;
    --amarelo: #f4a261;
    --vermelho: #e63946;
    --cinza-1: #1a1a2e;
    --cinza-2: #2d2d44;
    --cinza-3: #3d3d5c;
    --cinza-4: #5a5a7a;
    --cinza-5: #8888aa;
    --cinza-6: #b8b8d0;
    --cinza-7: #e8e8f0;
    --branco: #f8f9fa;
    --card-bg: #ffffff;
    --shadow: 0 2px 12px rgba(27,67,50,0.08);
    --shadow-hover: 0 6px 24px rgba(27,67,50,0.14);
  }

  * { box-sizing: border-box; margin: 0; padding: 0; }

  body {
    font-family: 'DM Sans', sans-serif;
    background: linear-gradient(135deg, #f0f7f4 0%, #e8f0eb 50%, #f5f0eb 100%);
    min-height: 100vh;
    color: var(--cinza-1);
  }

  .shiny-input-container { margin-bottom: 0 !important; }

  /* HEADER */
  .app-header {
    background: linear-gradient(135deg, var(--verde-escuro) 0%, var(--verde-medio) 100%);
    padding: 16px 32px;
    box-shadow: 0 4px 20px rgba(27,67,50,0.3);
    position: sticky;
    top: 0;
    z-index: 1000;
  }
  .header-inner {
    max-width: 1440px;
    margin: 0 auto;
    display: flex;
    align-items: center;
    justify-content: space-between;
  }
  .logo-group { display: flex; align-items: center; gap: 14px; }
  .logo-icon {
    width: 48px; height: 48px;
    background: rgba(255,255,255,0.15);
    border-radius: 12px;
    display: flex; align-items: center; justify-content: center;
    font-size: 24px; color: #fff;
    backdrop-filter: blur(10px);
  }
  .logo-img {
    height: 48px;
    width: auto;
    max-width: 160px;
    object-fit: contain;
    border-radius: 8px;
    filter: brightness(0) invert(1);  /* torna branco sobre fundo escuro */
  }
  .app-title {
    font-family: 'DM Serif Display', serif;
    font-size: 26px;
    color: #fff;
    letter-spacing: -0.5px;
    line-height: 1.1;
  }
  .app-subtitle {
    font-size: 11px;
    color: rgba(255,255,255,0.7);
    letter-spacing: 0.5px;
    text-transform: uppercase;
  }
  .header-badges { display: flex; gap: 8px; }
  .badge-manual {
    background: rgba(255,255,255,0.15);
    color: rgba(255,255,255,0.9);
    padding: 5px 10px;
    border-radius: 20px;
    font-size: 11px;
    font-weight: 500;
    backdrop-filter: blur(5px);
    border: 1px solid rgba(255,255,255,0.2);
  }

  /* LAYOUT PRINCIPAL */
  .main-container {
    max-width: 1440px;
    margin: 24px auto;
    padding: 0 24px;
    display: grid;
    grid-template-columns: 400px 1fr;
    gap: 20px;
    align-items: start;
  }

  /* PAINEL ESQUERDO */
  .panel-left { display: flex; flex-direction: column; gap: 16px; }

  /* CARDS */
  .card-section {
    background: var(--card-bg);
    border-radius: 16px;
    box-shadow: var(--shadow);
    border: 1px solid rgba(27,67,50,0.06);
    transition: box-shadow 0.2s;
  }
  .card-section:hover { box-shadow: var(--shadow-hover); }

  .card-header-custom {
    background: linear-gradient(135deg, var(--verde-escuro) 0%, var(--verde-medio) 100%);
    color: white;
    padding: 12px 18px;
    font-weight: 600;
    font-size: 13px;
    display: flex;
    align-items: center;
    gap: 8px;
    letter-spacing: 0.3px;
    border-radius: 16px 16px 0 0;
  }
  .card-header-custom i { font-size: 16px; }

  .card-body-custom { padding: 16px; }

  /* FORM GROUPS */
  .form-row-2 { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 10px; }
  .form-row-3 { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 8px; margin-bottom: 8px; }
  .form-group-custom { display: flex; flex-direction: column; gap: 4px; }

  .input-label {
    font-size: 11.5px;
    font-weight: 600;
    color: var(--cinza-4);
    text-transform: uppercase;
    letter-spacing: 0.4px;
    display: flex;
    align-items: center;
    gap: 4px;
  }

  .nutrient-group {
    background: var(--cinza-7);
    border-radius: 10px;
    padding: 12px;
    margin-bottom: 10px;
    border: 1px solid rgba(27,67,50,0.06);
  }
  .nutrient-group-title {
    font-size: 11px;
    font-weight: 700;
    color: var(--verde-medio);
    text-transform: uppercase;
    letter-spacing: 0.5px;
    margin-bottom: 10px;
    display: flex;
    align-items: center;
    gap: 6px;
  }

  .section-hint {
    font-size: 11.5px;
    color: var(--cinza-5);
    background: #fffbeb;
    border: 1px solid #fbbf24;
    border-radius: 8px;
    padding: 8px 10px;
    margin-bottom: 12px;
    display: flex;
    align-items: center;
    gap: 6px;
  }

  /* Inputs estilizados */
  .form-control, .selectize-input, select.form-control {
    border: 1.5px solid var(--cinza-6) !important;
    border-radius: 8px !important;
    font-size: 13px !important;
    font-family: 'DM Mono', monospace !important;
    color: var(--cinza-1) !important;
    transition: border-color 0.2s, box-shadow 0.2s !important;
    background: white !important;
    min-height: 34px !important;
    height: auto !important;
    padding: 4px 8px !important;
  }
  .form-control:focus, .selectize-input.focus {
    border-color: var(--verde-claro) !important;
    box-shadow: 0 0 0 3px rgba(82,183,136,0.15) !important;
    outline: none !important;
  }

  /* BOTÃO CALCULAR */
  .btn-container { padding: 4px 0 8px; }
  .btn-calcular {
    background: linear-gradient(135deg, var(--verde-escuro), var(--verde-claro)) !important;
    color: white !important;
    border: none !important;
    border-radius: 12px !important;
    font-size: 14px !important;
    font-weight: 700 !important;
    letter-spacing: 0.5px !important;
    padding: 14px !important;
    width: 100% !important;
    cursor: pointer !important;
    transition: all 0.3s !important;
    box-shadow: 0 4px 15px rgba(27,67,50,0.3) !important;
    text-transform: uppercase !important;
  }
  .btn-calcular:hover {
    transform: translateY(-2px) !important;
    box-shadow: 0 8px 25px rgba(27,67,50,0.4) !important;
  }
  .btn-calcular:active { transform: translateY(0) !important; }

  /* PAINEL DIREITO */
  .panel-right {
    background: var(--card-bg);
    border-radius: 16px;
    box-shadow: var(--shadow);
    border: 1px solid rgba(27,67,50,0.06);
  }

  /* TABS */
  .results-tabs {
    background: linear-gradient(135deg, var(--verde-escuro), var(--verde-medio));
    padding: 12px 16px 0;
    border-radius: 16px 16px 0 0;
  }
  .tab-list {
    display: flex;
    gap: 4px;
    overflow-x: auto;
  }
  .tab-btn {
    background: rgba(255,255,255,0.1) !important;
    color: rgba(255,255,255,0.8) !important;
    border: 1px solid rgba(255,255,255,0.15) !important;
    border-bottom: none !important;
    border-radius: 8px 8px 0 0 !important;
    padding: 8px 14px !important;
    font-size: 12px !important;
    font-weight: 600 !important;
    cursor: pointer !important;
    transition: all 0.2s !important;
    white-space: nowrap !important;
    letter-spacing: 0.2px !important;
  }
  .tab-btn:hover {
    background: rgba(255,255,255,0.2) !important;
    color: white !important;
  }
  .active-tab {
    background: white !important;
    color: var(--verde-escuro) !important;
    border-color: rgba(255,255,255,0.3) !important;
  }

  /* PAINÉIS DE RESULTADO */
  .results-panel { display: none; padding: 24px; }
  .active-panel { display: block; }

  /* CARDS DE RESULTADO */
  .result-card {
    background: var(--cinza-7);
    border-radius: 12px;
    padding: 16px;
    margin-bottom: 16px;
    border: 1px solid rgba(27,67,50,0.06);
  }
  .result-card-title {
    font-size: 12px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    color: var(--cinza-4);
    margin-bottom: 12px;
    display: flex;
    align-items: center;
    gap: 6px;
    padding-bottom: 8px;
    border-bottom: 2px solid rgba(27,67,50,0.08);
  }

  /* MÉTRICAS */
  .metrics-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(120px, 1fr)); gap: 10px; }
  .metric-item {
    background: white;
    border-radius: 10px;
    padding: 12px 10px;
    text-align: center;
    border: 2px solid transparent;
    transition: all 0.2s;
  }
  .metric-item:hover { transform: translateY(-2px); box-shadow: 0 4px 12px rgba(0,0,0,0.1); }
  .metric-value {
    font-family: 'DM Serif Display', serif;
    font-size: 22px;
    font-weight: bold;
    line-height: 1.1;
    color: var(--cinza-1);
  }
  .metric-unit { font-size: 10px; color: var(--cinza-5); margin-top: 1px; }
  .metric-name { font-size: 11px; font-weight: 600; color: var(--cinza-4); margin-top: 4px; text-transform: uppercase; letter-spacing: 0.3px; }
  .metric-class {
    display: inline-block;
    font-size: 10px;
    font-weight: 700;
    padding: 2px 7px;
    border-radius: 20px;
    margin-top: 5px;
    text-transform: uppercase;
    letter-spacing: 0.3px;
  }

  /* Cores de classe */
  .cl-muito-baixo { background: #fde8e8; color: #c62828; border-color: #f44336; }
  .cl-baixo { background: #fff3e0; color: #e65100; border-color: #ff9800; }
  .cl-medio { background: #fffde7; color: #f57f17; border-color: #ffc107; }
  .cl-bom { background: #e8f5e9; color: #2e7d32; border-color: #4caf50; }
  .cl-muito-bom, .cl-adequado, .cl-excelente { background: #e8f5e9; color: #1b5e20; border-color: #2e7d32; }
  .cl-alcalino, .cl-alto { background: #e3f2fd; color: #1565c0; border-color: #2196f3; }

  /* REC ADUBAÇÃO */
  .npk-display { display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px; margin: 12px 0; }
  .npk-card {
    border-radius: 12px;
    padding: 16px;
    text-align: center;
    color: white;
  }
  .npk-n { background: linear-gradient(135deg, #1b5e20, #388e3c); }
  .npk-p { background: linear-gradient(135deg, #7b1fa2, #ab47bc); }
  .npk-k { background: linear-gradient(135deg, #e65100, #ff9800); }
  .npk-label { font-size: 11px; opacity: 0.85; text-transform: uppercase; letter-spacing: 0.5px; }
  .npk-value { font-family: 'DM Serif Display', serif; font-size: 32px; line-height: 1.1; }
  .npk-unit { font-size: 11px; opacity: 0.75; }

  /* TABELA FINANCEIRA */
  .table-financeira { width: 100%; border-collapse: collapse; font-size: 12.5px; }
  .table-financeira th {
    background: var(--verde-escuro);
    color: white;
    padding: 10px 12px;
    text-align: left;
    font-weight: 600;
    font-size: 11px;
    text-transform: uppercase;
    letter-spacing: 0.4px;
  }
  .table-financeira td {
    padding: 9px 12px;
    border-bottom: 1px solid var(--cinza-7);
    font-family: 'DM Mono', monospace;
    font-size: 12px;
  }
  .table-financeira tr:hover td { background: var(--verde-palido); }
  .table-financeira tr.melhor-opcao td { background: #e8f5e9; font-weight: 700; }

  /* ALERTA CALAGEM */
  .calagem-alerta {
    border-radius: 12px;
    padding: 16px;
    margin-bottom: 16px;
    display: flex;
    align-items: flex-start;
    gap: 12px;
    border: 2px solid;
  }
  .calagem-necessaria { background: #fff3e0; border-color: #ff9800; }
  .calagem-ok { background: #e8f5e9; border-color: #4caf50; }
  .calagem-icon { font-size: 28px; line-height: 1; }
  .calagem-titulo { font-weight: 700; font-size: 15px; margin-bottom: 4px; }
  .calagem-desc { font-size: 13px; color: var(--cinza-4); }

  /* CHARTS GRID */
  .charts-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 16px;
  }
  .chart-card {
    background: var(--cinza-7);
    border-radius: 12px;
    padding: 16px;
    border: 1px solid rgba(27,67,50,0.06);
  }
  .chart-title {
    font-size: 12px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    color: var(--cinza-4);
    margin-bottom: 10px;
    display: flex;
    align-items: center;
    gap: 6px;
  }

  /* PLACEHOLDER */
  .placeholder-msg {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    padding: 60px 20px;
    text-align: center;
    color: var(--cinza-5);
  }
  .placeholder-icon { font-size: 56px; margin-bottom: 16px; opacity: 0.4; }
  .placeholder-title { font-family: 'DM Serif Display', serif; font-size: 20px; color: var(--cinza-3); margin-bottom: 8px; }
  .placeholder-text { font-size: 13px; color: var(--cinza-5); max-width: 300px; line-height: 1.5; }

  /* MÉTODOS CALAGEM */
  .metodos-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(140px, 1fr)); gap: 10px; }
  .metodo-card {
    background: white;
    border-radius: 10px;
    padding: 14px;
    text-align: center;
    border: 2px solid var(--verde-claro);
  }
  .metodo-card.destaque { background: var(--verde-palido); border-color: var(--verde-escuro); }
  .metodo-nome { font-size: 10px; color: var(--cinza-4); text-transform: uppercase; letter-spacing: 0.3px; margin-bottom: 6px; }
  .metodo-dose { font-family: 'DM Serif Display', serif; font-size: 26px; color: var(--verde-escuro); }
  .metodo-unidade { font-size: 11px; color: var(--cinza-5); }

  /* FOOTER */
  .app-footer {
    max-width: 1440px;
    margin: 12px auto 24px;
    padding: 12px 24px;
    background: rgba(255,255,255,0.6);
    border-radius: 10px;
    border: 1px solid rgba(27,67,50,0.08);
    font-size: 11.5px;
    color: var(--cinza-5);
    backdrop-filter: blur(5px);
  }

  /* SELECTIZE */
  .selectize-control.single .selectize-input { height: 34px !important; padding: 6px 8px !important; }
  .selectize-dropdown { font-size: 13px !important; z-index: 9999 !important; }
  .selectize-dropdown-content .option:hover { background: var(--verde-palido) !important; }
  /* Multi-select: permite que os tags selecionados quebrem linha sem cortar */
  .selectize-control.multi .selectize-input { flex-wrap: wrap !important; }
  .selectize-control.multi .selectize-input > div {
    margin: 2px 4px 2px 0 !important;
  }

  /* DESTAQUE MELHOR PRECO */
  .melhor-preco-badge {
    background: var(--verde-escuro);
    color: white;
    padding: 2px 7px;
    border-radius: 10px;
    font-size: 9px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.3px;
    margin-left: 6px;
  }

  /* RESPONSIVE */
  @media (max-width: 1100px) {
    .main-container { grid-template-columns: 1fr; }
    .charts-grid { grid-template-columns: 1fr; }
  }

  /* BARRA DE PREÇOS */
  .price-toolbar {
    display: flex;
    align-items: center;
    justify-content: space-between;
    flex-wrap: wrap;
    gap: 10px;
    padding: 12px 16px;
    background: linear-gradient(135deg, #0d3349, #1a5276);
    border-radius: 12px 12px 0 0;
    margin-bottom: 0;
  }
  .price-toolbar-left {
    display: flex;
    align-items: center;
    gap: 8px;
    color: white;
    font-size: 13px;
  }
  .price-toolbar-left i { font-size: 16px; opacity: 0.9; }
  .price-toolbar-title { font-weight: 700; letter-spacing: 0.3px; }
  .price-toolbar-right {
    display: flex;
    gap: 8px;
    flex-wrap: wrap;
  }

  /* BADGE STATUS PREÇO */
  .badge-fonte {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    padding: 3px 9px;
    border-radius: 20px;
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 0.3px;
    text-transform: uppercase;
    border: 1px solid rgba(255,255,255,0.3);
  }
  .badge-cepea   { background: #1abc9c; color: white; }
  .badge-local   { background: #f39c12; color: white; }
  .badge-fallback { background: #95a5a6; color: white; }

  /* BOTÃO CEPEA */
  .btn-cepea {
    background: linear-gradient(135deg, #1abc9c, #16a085) !important;
    color: white !important;
    border: none !important;
    border-radius: 8px !important;
    font-size: 12px !important;
    font-weight: 600 !important;
    padding: 7px 14px !important;
    cursor: pointer !important;
    transition: all 0.25s !important;
    box-shadow: 0 2px 8px rgba(26,188,156,0.35) !important;
    white-space: nowrap !important;
  }
  .btn-cepea:hover {
    transform: translateY(-1px) !important;
    box-shadow: 0 4px 14px rgba(26,188,156,0.5) !important;
  }
  .btn-cepea:disabled {
    opacity: 0.6 !important;
    transform: none !important;
    cursor: wait !important;
  }

  /* BOTÃO SALVAR PREÇOS */
  .btn-salvar-precos {
    background: rgba(255,255,255,0.15) !important;
    color: white !important;
    border: 1px solid rgba(255,255,255,0.3) !important;
    border-radius: 8px !important;
    font-size: 12px !important;
    font-weight: 600 !important;
    padding: 7px 14px !important;
    cursor: pointer !important;
    transition: all 0.2s !important;
    white-space: nowrap !important;
  }
  .btn-salvar-precos:hover {
    background: rgba(255,255,255,0.25) !important;
  }

  /* BOTÃO MENOR PREÇO BRASIL */
  .btn-menor-preco {
    background: linear-gradient(135deg, #c0392b, #e74c3c) !important;
    color: white !important;
    border: none !important;
    border-radius: 8px !important;
    font-size: 12px !important;
    font-weight: 600 !important;
    padding: 7px 14px !important;
    cursor: pointer !important;
    transition: all 0.25s !important;
    box-shadow: 0 2px 8px rgba(192,57,43,0.35) !important;
    white-space: nowrap !important;
    text-decoration: none !important;
    display: inline-flex !important;
    align-items: center !important;
    gap: 6px !important;
  }
  .btn-menor-preco:hover {
    transform: translateY(-1px) !important;
    box-shadow: 0 4px 14px rgba(192,57,43,0.5) !important;
    color: white !important;
    text-decoration: none !important;
  }

  /* Painel de links rápidos do Menor Preço */
  .menor-preco-panel {
    background: #fdf2f0;
    border: 1px solid #f5c6c0;
    border-radius: 10px;
    padding: 12px 16px;
    margin-bottom: 12px;
    font-size: 12px;
  }
  .menor-preco-panel .mp-title {
    font-weight: 700;
    color: #922b21;
    margin-bottom: 8px;
    display: flex;
    align-items: center;
    gap: 6px;
    font-size: 12.5px;
  }
  .menor-preco-panel .mp-links {
    display: flex;
    flex-wrap: wrap;
    gap: 6px;
    margin-top: 8px;
  }
  .menor-preco-panel .mp-link {
    background: white;
    border: 1px solid #e8b4af;
    border-radius: 6px;
    padding: 4px 10px;
    font-size: 11px;
    color: #922b21;
    text-decoration: none;
    font-weight: 600;
    transition: all 0.2s;
  }
  .menor-preco-panel .mp-link:hover {
    background: #c0392b;
    color: white;
    border-color: #c0392b;
    text-decoration: none;
  }

  /* FEEDBACK CEPEA */
  .cepea-feedback {
    padding: 10px 16px;
    font-size: 12.5px;
    font-weight: 500;
    border-radius: 0;
    display: flex;
    align-items: center;
    gap: 8px;
  }
  .cepea-ok      { background: #eafaf1; color: #1e8449; border-left: 4px solid #27ae60; }
  .cepea-erro    { background: #fdf2e9; color: #a04000; border-left: 4px solid #e67e22; }
  .cepea-loading { background: #eaf2fb; color: #1a5276; border-left: 4px solid #2980b9; }

  /* TABELA DE PREÇOS DT */
  .dataTables_wrapper { font-size: 12.5px; }
  table.dataTable thead th {
    background: var(--verde-escuro) !important;
    color: white !important;
    font-size: 11px !important;
    text-transform: uppercase !important;
    letter-spacing: 0.4px !important;
    border: none !important;
    padding: 10px 12px !important;
  }
  table.dataTable tbody td { padding: 8px 12px !important; }
  table.dataTable tbody tr:hover td { background: var(--verde-palido) !important; }
  .cell-edit-fld input {
    border: 2px solid var(--verde-claro) !important;
    border-radius: 6px !important;
    padding: 3px 6px !important;
    font-family: 'DM Mono', monospace !important;
    width: 80px !important;
  }
  /* Linha destacada quando preço vem do CEPEA */
  table.dataTable tbody tr.preco-cepea td { background: #eafaf1 !important; }
  table.dataTable tbody tr.preco-manual td { background: #fef9e7 !important; }

  /* ================================================================
     ABA PESQUISA
     ================================================================ */
  .pesq-header {
    display: flex; align-items: center; gap: 16px;
    background: linear-gradient(135deg, #1a1a4e, #2c2c7a);
    border-radius: 12px; padding: 18px 20px; margin-bottom: 18px; color: white;
  }
  .pesq-header-icon { font-size: 36px; opacity: 0.9; flex-shrink: 0; }
  .pesq-header-title { font-family: 'DM Serif Display',serif; font-size: 20px; margin: 0 0 4px; color: white; }
  .pesq-header-sub { font-size: 12px; opacity: 0.75; margin: 0; }

  .pesq-subtabs {
    margin-bottom: 14px;
  }
  .pesq-subtabs .selectize-input {
    border: 2px solid #2c2c7a !important;
    font-weight: 600 !important;
    color: #1a1a4e !important;
  }

  /* Caixa de interpretação automática */
  .stat-interpretacao {
    background: #f0f0fa; border-left: 4px solid #2c2c7a;
    border-radius: 0 8px 8px 0; padding: 10px 14px; font-size: 12.5px;
    color: #2c2c7a; margin-top: 10px; line-height: 1.6;
  }
  .stat-erro {
    background: #fdf2e9; border-left: 4px solid #e67e22;
    border-radius: 0 8px 8px 0; padding: 10px 14px; font-size: 12.5px;
    color: #a04000;
  }
  .stat-badge-n {
    display: inline-block; background: #2c2c7a; color: white;
    padding: 2px 9px; border-radius: 12px; font-size: 10px;
    font-weight: 700; letter-spacing: 0.3px; margin-left: 6px;
  }

  .pesq-config-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
    gap: 10px;
  }

  .pesq-unidade-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(195px, 1fr));
    gap: 12px; margin-top: 12px;
  }
  .pesq-unidade-card {
    background: white; border: 2px solid #e0e0f0; border-radius: 12px; padding: 16px; transition: all 0.2s;
  }
  .pesq-unidade-card:hover { border-color: #2c2c7a; box-shadow: 0 4px 12px rgba(28,28,78,0.12); }
  .pesq-unidade-card.destaque { border-color: #2c2c7a; background: #f0f0fa; }
  .pesq-unidade-tipo {
    font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.5px;
    color: #555; margin-bottom: 8px; display: flex; align-items: center; gap: 5px;
  }
  .pesq-dose-row {
    display: flex; justify-content: space-between; align-items: baseline;
    padding: 3px 0; border-bottom: 1px solid #eee;
  }
  .pesq-dose-row:last-child { border-bottom: none; }
  .pesq-dose-label { font-size: 11px; color: #777; }
  .pesq-dose-valor { font-family: 'DM Mono',monospace; font-weight: 700; font-size: 13px; color: #1a1a4e; }
  .pesq-dose-unit  { font-size: 9px; color: #aaa; margin-left: 3px; }

  .pesq-trat-table { width: 100%; border-collapse: collapse; font-size: 12px; margin-top: 8px; }
  .pesq-trat-table th {
    background: #2c2c7a; color: white; padding: 8px 10px;
    font-size: 10px; text-transform: uppercase; letter-spacing: 0.4px; text-align: center;
  }
  .pesq-trat-table th:first-child { text-align: left; }
  .pesq-trat-table td {
    padding: 7px 10px; border-bottom: 1px solid #eee;
    text-align: center; font-family: 'DM Mono',monospace; font-size: 11px;
  }
  .pesq-trat-table td:first-child { text-align: left; font-weight: 600; }
  .pesq-trat-table tr:nth-child(even) td { background: #f7f7fb; }
  .pesq-trat-table tr:hover td { background: #eeeef8; }

  .pesq-badge {
    display: inline-flex; align-items: center; gap: 4px;
    padding: 3px 8px; border-radius: 12px; font-size: 10px;
    font-weight: 700; text-transform: uppercase; letter-spacing: 0.3px;
  }
  .pesq-badge-info { background: #e8eaf6; color: #3949ab; }
  .pesq-badge-ok   { background: #e8f5e9; color: #2e7d32; }
  .pesq-badge-warn { background: #fff3e0; color: #e65100; }

  /* Alerta de subestimação calagem */
  .pesq-alerta-calagem {
    background: #fff8e1; border-left: 4px solid #ffc107;
    border-radius: 0 8px 8px 0; padding: 10px 14px; font-size: 12px;
    color: #6d4c00; margin-top: 10px;
  }

  /* ================================================================
     BANCO REGIONAL
     ================================================================ */
  .regional-status-ok {
    background: #eafaf1; border-left: 4px solid #27ae60;
    border-radius: 0 8px 8px 0; padding: 10px 14px; font-size: 12.5px;
    color: #1e8449; margin-top: 10px;
  }
  .regional-status-erro {
    background: #fdf2e9; border-left: 4px solid #e67e22;
    border-radius: 0 8px 8px 0; padding: 10px 14px; font-size: 12.5px;
    color: #a04000; margin-top: 10px;
  }
  .regional-status-warn {
    background: #fff8e1; border-left: 4px solid #ffc107;
    border-radius: 0 8px 8px 0; padding: 10px 14px; font-size: 12px;
    color: #6d4c00; margin-top: 10px;
  }
  .regional-resumo-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
    gap: 10px;
    margin-top: 10px;
  }
  .regional-resumo-card {
    background: white; border: 2px solid #d6eaf8; border-radius: 10px;
    padding: 12px; text-align: center;
  }
  .regional-resumo-valor {
    font-family: 'DM Serif Display', serif; font-size: 24px; color: #1a5276;
  }
  .regional-resumo-label {
    font-size: 10px; color: #888; text-transform: uppercase; letter-spacing: 0.4px; margin-top: 2px;
  }
  .regional-map-container {
    border-radius: 12px; overflow: hidden; border: 1px solid #d6eaf8;
  }
  .benchmark-box {
    background: #f0f7fb; border: 2px solid #aed6f1; border-radius: 10px;
    padding: 12px 14px; font-size: 12.5px; color: #1a5276; margin-top: 10px;
  }
  .benchmark-box .bm-pct {
    font-family: 'DM Serif Display', serif; font-size: 28px; color: #0d3349;
  }

  /* ================================================================
     ANÁLISES ESTATÍSTICAS
     ================================================================ */
  .estat-card {
    background: white; border: 1px solid #e8d5f0; border-radius: 12px;
    padding: 16px; margin-bottom: 16px;
  }
  .estat-titulo {
    font-size: 12px; font-weight: 700; text-transform: uppercase;
    letter-spacing: 0.5px; color: #6c3483; margin-bottom: 10px;
    display: flex; align-items: center; gap: 6px;
    padding-bottom: 8px; border-bottom: 2px solid #f0e6f6;
  }
  .estat-interpretacao {
    background: #f8f0fc; border-left: 4px solid #6c3483;
    border-radius: 0 8px 8px 0; padding: 10px 14px; font-size: 12.5px;
    color: #4a235a; margin-top: 10px; line-height: 1.6;
  }
  .estat-resultado-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(130px, 1fr));
    gap: 10px; margin: 10px 0;
  }
  .estat-resultado-card {
    background: #f8f0fc; border: 2px solid #e8d5f0; border-radius: 10px;
    padding: 12px; text-align: center;
  }
  .estat-resultado-valor {
    font-family: 'DM Serif Display', serif; font-size: 22px; color: #4a235a;
  }
  .estat-resultado-label {
    font-size: 10px; color: #888; text-transform: uppercase;
    letter-spacing: 0.4px; margin-top: 2px;
  }
  .estat-resultado-card.destaque {
    background: #4a235a; border-color: #4a235a;
  }
  .estat-resultado-card.destaque .estat-resultado-valor { color: white; }
  .estat-resultado-card.destaque .estat-resultado-label { color: rgba(255,255,255,0.7); }
  "
}

# JavaScript para navegação entre tabs — inclui pesquisa
js_tabs <- function() {
  "
  $(document).on('click', '.tab-btn', function() {
    var btn = $(this);
    var id = btn.attr('id');
    var panelMap = {
      'tab_solo':      'painel_solo',
      'tab_calagem':   'painel_calagem',
      'tab_adubacao':  'painel_adubacao',
      'tab_financeiro':'painel_financeiro',
      'tab_graficos':  'painel_graficos',
      'tab_pesquisa':  'painel_pesquisa',
      'tab_regional':  'painel_regional'
    };
    $('.tab-btn').removeClass('active-tab');
    btn.addClass('active-tab');
    $('.results-panel').removeClass('active-panel');
    $('#' + panelMap[id]).addClass('active-panel');
    setTimeout(function() {
      $(window).trigger('resize');
    }, 100);
  });
  "
}
