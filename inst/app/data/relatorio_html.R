# ==============================================================================
# GERADOR DE RELATÓRIO HTML — fertilidade + recomendações + fenologia
# Exportável como PDF via window.print() com CSS @media print
# ==============================================================================

COR_CLASSE <- list(
  "Muito Baixo"  = list(bg="#FFEBEE", borda="#E53935", texto="#B71C1C", icone="🔴"),
  "Baixo"        = list(bg="#FFF8E1", borda="#FB8C00", texto="#E65100", icone="🟠"),
  "Médio"        = list(bg="#E8F5E9", borda="#43A047", texto="#1B5E20", icone="🟡"),
  "Alto"         = list(bg="#E3F2FD", borda="#1E88E5", texto="#0D47A1", icone="🟢"),
  "Muito Alto"   = list(bg="#F3E5F5", borda="#8E24AA", texto="#4A148C", icone="🔵"),
  "Adequado"     = list(bg="#E8F5E9", borda="#43A047", texto="#1B5E20", icone="🟢"),
  "Moderadamente Ácido" = list(bg="#FFF9C4", borda="#F9A825", texto="#F57F17", icone="🟡"),
  "Levemente Ácido"     = list(bg="#F1F8E9", borda="#7CB342", texto="#33691E", icone="🟡"),
  "Ácido"        = list(bg="#FFE0B2", borda="#EF6C00", texto="#BF360C", icone="🟠"),
  "Muito Ácido"  = list(bg="#FFEBEE", borda="#E53935", texto="#B71C1C", icone="🔴"),
  "Alcalino"     = list(bg="#E8EAF6", borda="#3949AB", texto="#1A237E", icone="🔵")
)

# Retorna CSS+HTML do badge colorido por classe
badge_classe <- function(classe, valor, unidade = "") {
  cor <- COR_CLASSE[[classe]]
  if (is.null(cor)) cor <- list(bg="#F5F5F5", borda="#9E9E9E", texto="#616161", icone="⚪")
  paste0(
    '<span style="display:inline-flex;align-items:center;gap:5px;',
    'background:', cor$bg, ';border:1.5px solid ', cor$borda, ';',
    'border-radius:6px;padding:3px 8px;font-size:12px;font-weight:600;',
    'color:', cor$texto, ';">',
    cor$icone, ' ', classe,
    if (nchar(valor) > 0) paste0(' <span style="font-weight:400;opacity:0.75;">(', valor,
                                   if(nchar(unidade)>0) paste0(' ', unidade) else '', ')</span>') else '',
    '</span>'
  )
}

# Linha da tabela de parâmetros
linha_parametro <- function(nome, valor, unidade, classe, referencia = "") {
  cor <- COR_CLASSE[[classe]]
  if (is.null(cor)) cor <- list(bg="#F9F9F9", borda="#E0E0E0", texto="#333", icone="⚪")
  paste0(
    '<tr style="border-bottom:1px solid #F0F0F0;">',
    '<td style="padding:8px 12px;font-weight:500;color:#333;">', nome, '</td>',
    '<td style="padding:8px 12px;text-align:center;font-weight:700;font-size:14px;color:#1B3A2D;">',
    valor, ' <span style="font-size:10px;font-weight:400;color:#888;">', unidade, '</span></td>',
    '<td style="padding:8px 12px;text-align:center;">',
    badge_classe(classe, "", ""), '</td>',
    '<td style="padding:8px 12px;font-size:11px;color:#666;">', referencia, '</td>',
    '</tr>\n'
  )
}

gerar_relatorio_html <- function(
  solo, rec, cal_result, gesso_result,
  cultura, produtividade, prnt,
  info_produtor = list(),
  fenol     = NULL,
  nivel_tec = NULL
) {

  data_rel  <- format(Sys.Date(), "%d/%m/%Y")
  nome_prod <- info_produtor$nome      %||% "—"
  prop      <- info_produtor$propriedade %||% "—"
  mun       <- info_produtor$municipio  %||% "—"
  amos      <- info_produtor$amostra    %||% "—"

  # ── Seção 1: Identidade ────────────────────────────────────────────────────
  sec_identidade <- paste0('
  <div class="secao">
    <div class="secao-titulo"><span class="num">1</span> Identificação</div>
    <div class="grid-2">
      <div class="campo"><span class="campo-label">Produtor</span><span class="campo-valor">', nome_prod, '</span></div>
      <div class="campo"><span class="campo-label">Propriedade</span><span class="campo-valor">', prop, '</span></div>
      <div class="campo"><span class="campo-label">Município / UF</span><span class="campo-valor">', mun, '</span></div>
      <div class="campo"><span class="campo-label">Identificação da amostra</span><span class="campo-valor">', amos, '</span></div>
      <div class="campo"><span class="campo-label">Cultura</span><span class="campo-valor">',
        tools::toTitleCase(cultura), ' — Produtividade esperada: ', tools::toTitleCase(produtividade), '</span></div>
      <div class="campo"><span class="campo-label">Data do relatório</span><span class="campo-valor">', data_rel, '</span></div>
    </div>
  </div>')

  # ── Seção 2: Resultados da análise de solo ─────────────────────────────────
  linhas_solo <- ""

  # pH
  if (!is.null(solo$ph))
    linhas_solo <- paste0(linhas_solo, linha_parametro(
      "pH (H₂O)", solo$ph$valor, "", solo$ph$interp$classe,
      "Faixa ideal: 5.5–6.5"))

  # MO
  if (!is.null(solo$mo))
    linhas_solo <- paste0(linhas_solo, linha_parametro(
      "Matéria Orgânica", solo$mo$valor, "dag/kg", solo$mo$interp$classe,
      "Baixo < 1.5 | Médio 1.5–3.0 | Alto > 3.0"))

  # P
  if (!is.null(solo$p))
    linhas_solo <- paste0(linhas_solo, linha_parametro(
      "Fósforo (P-Mehlich)", solo$p$valor, "mg/dm³", solo$p$interp$nivel,
      "Varia com textura do solo"))

  # K
  if (!is.null(solo$k))
    linhas_solo <- paste0(linhas_solo, linha_parametro(
      "Potássio (K)", solo$k$valor, "mg/dm³", solo$k$interp$nivel, ""))

  # Ca
  if (!is.null(solo$ca))
    linhas_solo <- paste0(linhas_solo, linha_parametro(
      "Cálcio (Ca²⁺)", solo$ca$valor, "cmolc/dm³", solo$ca$interp$classe, ""))

  # Mg
  if (!is.null(solo$mg))
    linhas_solo <- paste0(linhas_solo, linha_parametro(
      "Magnésio (Mg²⁺)", solo$mg$valor, "cmolc/dm³", solo$mg$interp$classe, ""))

  # Al
  if (!is.null(solo$al)) {
    classe_al <- if (solo$al$valor > 0.5) "Alto" else "Baixo"
    linhas_solo <- paste0(linhas_solo, linha_parametro(
      "Alumínio (Al³⁺)", solo$al$valor, "cmolc/dm³", classe_al,
      "Tóxico > 0.5 cmolc/dm³"))
  }

  # H+Al, CTC, V%, m%
  if (!is.null(solo$h_al))
    linhas_solo <- paste0(linhas_solo, linha_parametro(
      "H+Al (acidez potencial)", solo$h_al$valor, "cmolc/dm³", "Médio", ""))
  if (!is.null(solo$ctc))
    linhas_solo <- paste0(linhas_solo, linha_parametro(
      "CTC (capacidade de troca)", solo$ctc$valor, "cmolc/dm³", "Médio", ""))
  if (!is.null(solo$v))
    linhas_solo <- paste0(linhas_solo, linha_parametro(
      "Saturação de bases (V%)", paste0(solo$v$valor, "%"), "", solo$v$interp$classe,
      "Ideal > 60% para a maioria das culturas"))
  if (!is.null(solo$m))
    linhas_solo <- paste0(linhas_solo, linha_parametro(
      "Saturação por Al (m%)", paste0(solo$m$valor, "%"), "", solo$m$interp$classe,
      "Crítico > 20-25%"))

  sec_solo <- paste0('
  <div class="secao">
    <div class="secao-titulo"><span class="num">2</span> Resultado da Análise de Solo</div>
    <table class="tabela-solo">
      <thead>
        <tr>
          <th>Parâmetro</th><th>Valor</th><th>Classificação</th><th>Referência</th>
        </tr>
      </thead>
      <tbody>', linhas_solo, '</tbody>
    </table>
  </div>')

  # ── Seção 3: Calagem e Gessagem ────────────────────────────────────────────
  linhas_cal <- ""
  if (!is.null(cal_result)) {
    for (metodo in names(cal_result)) {
      dose <- cal_result[[metodo]]
      if (!is.null(dose) && !is.na(dose)) {
        linhas_cal <- paste0(linhas_cal,
          '<tr><td style="padding:7px 12px;color:#333;">', metodo, '</td>',
          '<td style="padding:7px 12px;text-align:center;font-weight:700;color:#1B3A2D;">',
          round(dose, 2), ' t/ha</td>',
          '<td style="padding:7px 12px;font-size:11px;color:#666;">PRNT = ', prnt, '%</td></tr>\n')
      }
    }
  }

  linhas_gesso <- ""
  if (!is.null(gesso_result)) {
    for (metodo in names(gesso_result)) {
      dose <- gesso_result[[metodo]]
      if (!is.null(dose) && !is.na(dose)) {
        linhas_gesso <- paste0(linhas_gesso,
          '<tr><td style="padding:7px 12px;color:#333;">', metodo, '</td>',
          '<td style="padding:7px 12px;text-align:center;font-weight:700;color:#1B3A2D;">',
          round(dose, 2), ' t/ha</td>',
          '<td style="padding:7px 12px;font-size:11px;color:#666;">Gesso Agrícola</td></tr>\n')
      }
    }
  }

  sec_corretivos <- paste0('
  <div class="secao">
    <div class="secao-titulo"><span class="num">3</span> Calagem e Gessagem Recomendadas</div>
    <div class="grid-2">',
    if (nchar(linhas_cal) > 0)
      paste0('<div>
        <div class="subtitulo">Calcário (dose por método)</div>
        <table class="tabela-solo"><thead><tr><th>Método</th><th>Dose</th><th>Obs.</th></tr></thead>
        <tbody>', linhas_cal, '</tbody></table></div>') else
      '<div><p style="color:#888;font-size:12px;">Sem necessidade de calagem.</p></div>',
    if (nchar(linhas_gesso) > 0)
      paste0('<div>
        <div class="subtitulo">Gessagem</div>
        <table class="tabela-solo"><thead><tr><th>Método</th><th>Dose</th><th>Obs.</th></tr></thead>
        <tbody>', linhas_gesso, '</tbody></table></div>') else
      '<div><p style="color:#888;font-size:12px;">Gessagem não indicada.</p></div>',
    '</div></div>')

  # ── Seção 4: Adubação NPK ──────────────────────────────────────────────────
  linhas_npk <- ""
  if (!is.null(rec)) {
    pares <- list(
      list("N — Plantio",      rec$N_plantio,   "kg/ha"),
      list("N — Cobertura",    rec$N_cobertura, "kg/ha"),
      list("P₂O₅",             rec$P2O5,        "kg/ha"),
      list("K₂O",              rec$K2O,         "kg/ha")
    )
    for (p in pares) {
      v <- p[[2]]
      if (!is.null(v) && !is.na(v)) {
        linhas_npk <- paste0(linhas_npk,
          '<tr><td style="padding:8px 12px;font-weight:500;color:#333;">', p[[1]], '</td>',
          '<td style="padding:8px 12px;text-align:center;font-weight:700;font-size:15px;color:#1B3A2D;">',
          v, '</td>',
          '<td style="padding:8px 12px;font-size:11px;color:#666;">', p[[3]], '</td></tr>\n')
      }
    }
  }

  sec_npk <- paste0('
  <div class="secao">
    <div class="secao-titulo"><span class="num">4</span> Adubação NPK Recomendada</div>
    <table class="tabela-solo">
      <thead><tr><th>Nutriente</th><th>Dose</th><th>Unidade</th></tr></thead>
      <tbody>', linhas_npk, '</tbody>
    </table>
  </div>')


  # ── Seção 5: Nível Tecnológico ────────────────────────────────────────────
  sec_nivel <- ""
  if (!is.null(nivel_tec)) {
    f <- FATORES_NIVEL
    itens_ok_labels <- sapply(
      Filter(function(x) x$id %in% nivel_tec$itens_ok, CHECKLIST_TRATOS),
      function(x) x$label
    )
    itens_nao_labels <- sapply(
      Filter(function(x) !x$id %in% nivel_tec$itens_ok, CHECKLIST_TRATOS),
      function(x) x$label
    )
    cor_hex <- nivel_tec$cor
    linhas_ok  <- if (length(itens_ok_labels) > 0)
      paste0(sapply(itens_ok_labels, function(l)
        paste0('<li style="color:#2E7D32;">✅ ', l, '</li>')), collapse="\n")
      else ""
    linhas_nao <- if (length(itens_nao_labels) > 0)
      paste0(sapply(itens_nao_labels, function(l)
        paste0('<li style="color:#888;">❌ ', l, '</li>')), collapse="\n")
      else ""
    just <- justificativa_nivel(nivel_tec, cultura)
    sec_nivel <- paste0('
  <div class="secao">
    <div class="secao-titulo"><span class="num">5</span> N\u00edvel Tecnol\u00f3gico do Sistema</div>
    <div style="padding:12px 16px;">
      <div style="display:flex;align-items:center;gap:12px;margin-bottom:12px;">
        <div style="background:', cor_hex, ';color:white;font-weight:700;font-size:14px;
             padding:6px 18px;border-radius:20px;">', nivel_tec$nivel_label, '</div>
        <div style="flex:1;background:#E0E0E0;border-radius:4px;height:8px;">
          <div style="background:', cor_hex, ';width:', nivel_tec$pct, '%;height:8px;border-radius:4px;"></div>
        </div>
        <span style="font-weight:700;color:', cor_hex, ';font-size:14px;">', nivel_tec$pct, '%</span>
      </div>
      <p style="font-size:12px;color:#555;margin-bottom:10px;">', just, '</p>
      <div style="display:grid;grid-template-columns:1fr 1fr;gap:12px;">
        <div>
          <div style="font-size:11px;font-weight:700;color:#2E7D32;margin-bottom:4px;">Pr\u00e1ticas adotadas</div>
          <ul style="font-size:11px;list-style:none;padding:0;margin:0;">', linhas_ok, '</ul>
        </div>
        <div>
          <div style="font-size:11px;font-weight:700;color:#888;margin-bottom:4px;">N\u00e3o adotadas</div>
          <ul style="font-size:11px;list-style:none;padding:0;margin:0;">', linhas_nao, '</ul>
        </div>
      </div>
    </div>
  </div>')
  }

  # ── Seção 6: Estádio Fenológico (milho) ────────────────────────────────────
  sec_fenol <- ""
  if (!is.null(fenol) && cultura == "milho") {
    sec_fenol <- paste0('
  <div class="secao page-break">
    <div class="secao-titulo"><span class="num">5</span> Estádio Fenológico — Momento de Aplicação</div>
    <p style="font-size:12px;color:#555;margin-bottom:12px;">', fenol$descricao, '</p>',
    fenol$svg,
    '<p style="font-size:10px;color:#888;margin-top:8px;">
      Referência: Ritchie, S.W. et al. (1993). How a corn plant develops. Iowa State Univ. Extension, Ames.
    </p>
  </div>')
  }

  # ── Seção 6: Observações / Rodapé ─────────────────────────────────────────
  sec_rodape <- paste0('
  <div class="rodape">
    <p>Relatório gerado pelo <b>GEMS_Fertilizer</b> — Sistema de Recomendação de Adubação e Calagem.</p>
    <p>Baseado no Manual de Adubação e Calagem para Minas Gerais (5ª Aproximação, 2023) e Manual de Recomendações do Estado de Sergipe (EMBRAPA/UFS).</p>
    <p style="color:#e74c3c;margin-top:6px;">
      ⚠ Este relatório é um instrumento de apoio técnico. As recomendações devem ser validadas por um Engenheiro Agrônomo habilitado, considerando condições locais específicas não contempladas pelo sistema.
    </p>
  </div>')

  # ── HTML completo ──────────────────────────────────────────────────────────
  paste0('<!DOCTYPE html>
<html lang="pt-BR">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Relatório de Fertilidade — GEMS_Fertilizer</title>
<style>
  @import url("https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600&display=swap");
  * { box-sizing: border-box; margin: 0; padding: 0; }
  body { font-family: "DM Sans", sans-serif; background: #F8F9FA; color: #333; font-size: 13px; line-height: 1.5; }
  .container { max-width: 800px; margin: 0 auto; background: white; padding: 32px 40px; }
  .cabecalho { border-bottom: 3px solid #1B3A2D; padding-bottom: 16px; margin-bottom: 24px; }
  .logo-titulo { display: flex; align-items: center; gap: 12px; margin-bottom: 4px; }
  .logo-box { background: #1B3A2D; color: white; font-weight: 700; font-size: 13px; padding: 6px 10px; border-radius: 6px; letter-spacing: 0.5px; }
  h1 { font-size: 18px; font-weight: 600; color: #1B3A2D; }
  .subtitulo-cab { font-size: 11px; color: #888; margin-top: 2px; }
  .secao { margin-bottom: 24px; border: 1px solid #E8E8E8; border-radius: 10px; overflow: hidden; }
  .secao-titulo { background: #1B3A2D; color: white; padding: 10px 16px; font-weight: 600; font-size: 13px; display: flex; align-items: center; gap: 8px; }
  .num { background: rgba(255,255,255,0.25); border-radius: 50%; width: 22px; height: 22px; display: inline-flex; align-items: center; justify-content: center; font-size: 11px; font-weight: 700; }
  .subtitulo { font-weight: 600; font-size: 12px; color: #1B3A2D; padding: 10px 12px 4px; }
  .grid-2 { display: grid; grid-template-columns: 1fr 1fr; gap: 0; }
  .campo { padding: 8px 16px; border-bottom: 1px solid #F0F0F0; }
  .campo-label { font-size: 10px; color: #888; display: block; text-transform: uppercase; letter-spacing: 0.4px; }
  .campo-valor { font-weight: 500; color: #222; font-size: 12px; }
  .tabela-solo { width: 100%; border-collapse: collapse; }
  .tabela-solo thead tr { background: #F5F5F5; }
  .tabela-solo th { padding: 8px 12px; text-align: left; font-size: 11px; font-weight: 600; color: #555; text-transform: uppercase; letter-spacing: 0.3px; border-bottom: 2px solid #E0E0E0; }
  .tabela-solo tbody tr:nth-child(even) { background: #FAFAFA; }
  .rodape { margin-top: 32px; padding-top: 16px; border-top: 1px solid #E0E0E0; font-size: 10px; color: #888; line-height: 1.6; }
  .page-break { page-break-before: always; }
  @media print {
    body { background: white; }
    .container { padding: 16px; max-width: 100%; }
    .secao { break-inside: avoid; }
    .page-break { page-break-before: always; }
    button, .no-print { display: none !important; }
  }
</style>
</head>
<body>
<div class="container">
  <div class="cabecalho">
    <div class="logo-titulo">
      <div class="logo-box">GEMS</div>
      <div>
        <h1>Relatório de Fertilidade e Recomendação de Adubação</h1>
        <div class="subtitulo-cab">GEMS_Fertilizer v0.4 — Sistema de Recomendação de Adubação e Calagem</div>
      </div>
    </div>
  </div>
  <div style="text-align:right;margin-bottom:16px;" class="no-print">
    <button onclick="window.print()" style="background:#1B3A2D;color:white;border:none;padding:8px 18px;border-radius:6px;cursor:pointer;font-family:inherit;font-size:12px;">
      🖨️ Imprimir / Salvar como PDF
    </button>
  </div>',
  sec_identidade,
  sec_solo,
  sec_corretivos,
  sec_npk,
  sec_nivel,
  sec_fenol,
  sec_rodape,
'</div></body></html>')
}
