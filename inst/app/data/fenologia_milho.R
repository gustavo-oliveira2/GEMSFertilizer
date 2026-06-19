# ==============================================================================
# MÓDULO FENOLÓGICO — MILHO (Zea mays)
# SVG realista: caule com gradiente cilíndrico, nós, folhas arqueadas Bézier,
# nervura central, raízes fasciculadas + escora, espiga em V8, destaque por cor
# Baseado na escala VE-V2-V4-V6-V8-VT-R1 (Ritchie et al., 1993)
# ==============================================================================

fenologia_milho <- function(n_cobertura, n_plantio = 0) {
  nc <- n_cobertura %||% 0

  if (nc < 60) {
    estadio  <- "V4"
    parcelas <- list(list(v="V4", dose=nc))
    titulo   <- paste0("Aduba\u00e7\u00e3o de cobertura \u2014 dose \u00fanica em V4")
  } else if (nc < 120) {
    d1 <- round(nc * 2/3); d2 <- nc - d1
    estadio  <- "V4+V6"
    parcelas <- list(list(v="V4", dose=d1), list(v="V6", dose=d2))
    titulo   <- paste0("Aduba\u00e7\u00e3o de cobertura \u2014 parcelas em V4 e V6")
  } else {
    d1 <- round(nc / 2); d2 <- nc - d1
    estadio  <- "V4+V8"
    parcelas <- list(list(v="V4", dose=d1), list(v="V8", dose=d2))
    titulo   <- paste0("Aduba\u00e7\u00e3o de cobertura \u2014 parcelas em V4 e V8")
  }

  list(
    estadio     = estadio,
    parcelas    = parcelas,
    titulo      = titulo,
    n_cobertura = nc,
    svg         = gerar_svg_milho(parcelas, nc, titulo),
    descricao   = gerar_descricao_milho(parcelas, nc)
  )
}

gerar_descricao_milho <- function(parcelas, nc) {
  if (length(parcelas) == 1) {
    paste0("Com ", nc, " kg/ha de N em cobertura, aplicar dose \u00fanica em <b>V4</b> ",
           "(4\u00aa folha completamente expandida com colar vis\u00edvel, \u2248 25 dias ap\u00f3s emerg\u00eancia). ",
           "Aplicar a lan\u00e7o ou em linha lateral a \u223c10 cm da planta.")
  } else {
    p1 <- parcelas[[1]]; p2 <- parcelas[[2]]
    ord <- list(V4="4\u00aa", V6="6\u00aa", V8="8\u00aa")
    paste0("Com ", nc, " kg/ha de N em cobertura, parcelar em duas aplica\u00e7\u00f5es: ",
           "<b>", p1$dose, " kg/ha em ", p1$v, "</b> (",
           ord[[p1$v]], " folha com colar vis\u00edvel) e ",
           "<b>", p2$dose, " kg/ha em ", p2$v, "</b> (",
           ord[[p2$v]], " folha com colar vis\u00edvel). ",
           "O parcelamento reduz perdas por volatiliza\u00e7\u00e3o e lixivia\u00e7\u00e3o.")
  }
}

# ==============================================================================
# GERADOR DO SVG REALISTA
# ==============================================================================
gerar_svg_milho <- function(parcelas, nc_total, titulo) {
  aplicar_em <- sapply(parcelas, function(p) as.integer(gsub("V","",p$v)))
  doses_map  <- setNames(sapply(parcelas, function(p) p$dose),
                          sapply(parcelas, function(p) p$v))

  # Estádios a mostrar
  estadios <- if (length(parcelas)==1) c(2,4)
              else if (parcelas[[2]]$v=="V6") c(2,4,6)
              else c(2,4,6,8)

  n_pl   <- length(estadios)
  # Posições X de cada planta
  xs     <- switch(n_pl,
    `2` = c(180, 430),
    `3` = c(120, 300, 500),
    `4` = c(80,  230, 395, 565)
  )

  viewW  <- 660
  viewH  <- 540
  y_solo <- 390

  # Defs (gradientes)
  defs <- '
<defs>
  <linearGradient id="gc" x1="0" y1="0" x2="1" y2="0">
    <stop offset="0%"   stop-color="#2E7D32"/>
    <stop offset="25%"  stop-color="#558B2F"/>
    <stop offset="50%"  stop-color="#8BC34A"/>
    <stop offset="75%"  stop-color="#558B2F"/>
    <stop offset="100%" stop-color="#2E7D32"/>
  </linearGradient>
  <linearGradient id="gfd" x1="0" y1="0" x2="0" y2="1">
    <stop offset="0%"   stop-color="#43A047"/>
    <stop offset="45%"  stop-color="#66BB6A"/>
    <stop offset="100%" stop-color="#2E7D32"/>
  </linearGradient>
  <linearGradient id="gfe" x1="0" y1="0" x2="0" y2="1">
    <stop offset="0%"   stop-color="#388E3C"/>
    <stop offset="45%"  stop-color="#4CAF50"/>
    <stop offset="100%" stop-color="#1B5E20"/>
  </linearGradient>
  <linearGradient id="glrj" x1="0" y1="0" x2="0" y2="1">
    <stop offset="0%"   stop-color="#FFB300"/>
    <stop offset="100%" stop-color="#E65100"/>
  </linearGradient>
  <linearGradient id="gteal" x1="0" y1="0" x2="0" y2="1">
    <stop offset="0%"   stop-color="#00ACC1"/>
    <stop offset="100%" stop-color="#006064"/>
  </linearGradient>
  <linearGradient id="ge" x1="0" y1="0" x2="1" y2="0">
    <stop offset="0%"   stop-color="#F9A825"/>
    <stop offset="45%"  stop-color="#FFD54F"/>
    <stop offset="100%" stop-color="#F57F17"/>
  </linearGradient>
  <linearGradient id="gp" x1="0" y1="0" x2="1" y2="0">
    <stop offset="0%"   stop-color="#2E7D32"/>
    <stop offset="55%"  stop-color="#66BB6A"/>
    <stop offset="100%" stop-color="#1B5E20"/>
  </linearGradient>
  <linearGradient id="gs" x1="0" y1="0" x2="0" y2="1">
    <stop offset="0%"   stop-color="#8D6E63"/>
    <stop offset="40%"  stop-color="#6D4C41"/>
    <stop offset="100%" stop-color="#3E2723"/>
  </linearGradient>
  <marker id="arr" viewBox="0 0 10 10" refX="8" refY="5"
    markerWidth="6" markerHeight="6" orient="auto-start-reverse">
    <path d="M2 1L8 5L2 9" fill="none" stroke="#E65100"
      stroke-width="1.5" stroke-linecap="round"/>
  </marker>
  <marker id="arr2" viewBox="0 0 10 10" refX="8" refY="5"
    markerWidth="6" markerHeight="6" orient="auto-start-reverse">
    <path d="M2 1L8 5L2 9" fill="none" stroke="#006064"
      stroke-width="1.5" stroke-linecap="round"/>
  </marker>
  <marker id="arrt" viewBox="0 0 10 10" refX="8" refY="5"
    markerWidth="6" markerHeight="6" orient="auto-start-reverse">
    <path d="M2 1L8 5L2 9" fill="none" stroke="#795548"
      stroke-width="1.5" stroke-linecap="round"/>
  </marker>
</defs>'

  # Solo
  solo <- paste0(
    '<rect x="0" y="', y_solo, '" width="', viewW, '" height="90" fill="url(#gs)"/>\n',
    '<rect x="0" y="', y_solo, '" width="', viewW, '" height="14" fill="#A1887F" opacity="0.8"/>\n',
    '<ellipse cx="50"  cy="', y_solo+6, '" rx="14" ry="5" fill="#8D6E63" opacity="0.45"/>\n',
    '<ellipse cx="200" cy="', y_solo+5, '" rx="10" ry="4" fill="#795548" opacity="0.4"/>\n',
    '<ellipse cx="380" cy="', y_solo+6, '" rx="16" ry="5" fill="#8D6E63" opacity="0.45"/>\n',
    '<ellipse cx="560" cy="', y_solo+5, '" rx="12" ry="4" fill="#795548" opacity="0.4"/>\n'
  )

  # Cabeçalho
  header <- paste0(
    '<text x="', viewW/2, '" y="22" text-anchor="middle" ',
    'font-family="sans-serif" font-size="13" font-weight="700" fill="#1B5E20">',
    titulo, '</text>\n',
    '<text x="', viewW/2, '" y="38" text-anchor="middle" ',
    'font-family="sans-serif" font-size="10" fill="#555">',
    'N cobertura: ', nc_total, ' kg/ha \u2014 Zea mays (milho) \u2014 ',
    'cont. folhas pelo colar vis\u00edvel (Ritchie et al., 1993)</text>\n'
  )

  # Função: desenha uma planta completa
  desenhar_planta <- function(x, vnum) {
    dest    <- vnum %in% aplicar_em
    cor_arr <- if (vnum == aplicar_em[1]) "#E65100" else "#006064"
    id_grad <- if (vnum == aplicar_em[1]) "url(#glrj)" else "url(#gteal)"
    id_mark <- if (vnum == aplicar_em[1]) "url(#arr)" else "url(#arr2)"
    dose_txt <- if (!is.null(doses_map[paste0("V",vnum)][[1]]) &&
                    paste0("V",vnum) %in% names(doses_map))
                  paste0(doses_map[[paste0("V",vnum)]], " kg N/ha") else ""

    # Escala: porte cresce com vnum
    escala   <- 0.5 + vnum * 0.14
    h_caule  <- round(20 + vnum * 32 * escala)
    y_topo   <- y_solo - h_caule
    esp_no   <- round(h_caule / vnum)   # espaço entre nós
    cw       <- round(5 + vnum * 0.4)   # largura do caule

    out <- ""

    # ── Raízes ──────────────────────────────────────────────────────────────
    # Raízes fasciculadas primárias
    n_raiz <- 4 + vnum
    for (ri in seq_len(n_raiz)) {
      ang   <- -140 + ri * (280 / n_raiz)
      rad   <- ang * pi / 180
      leng  <- 18 + ri %% 3 * 8
      ex    <- round(x + sin(rad) * leng)
      ey    <- round(y_solo + abs(cos(rad)) * leng * 0.7)
      sw    <- if (abs(ang) < 80) 2.5 else 1.8
      out   <- paste0(out,
        '<path d="M', x, ',', y_solo, ' Q',
        round((x+ex)/2 + sin(rad)*4), ',', round((y_solo+ey)/2 + 6), ' ',
        ex, ',', ey, '" fill="none" stroke="#6D4C41" stroke-width="',
        sw, '" stroke-linecap="round"/>\n')
      # Raízes secundárias finas
      if (vnum >= 4) {
        ex2 <- round(ex + sin(rad+0.4)*10)
        ey2 <- round(ey + 10)
        out <- paste0(out,
          '<path d="M', ex, ',', ey, ' Q',
          round((ex+ex2)/2), ',', round((ey+ey2)/2), ' ',
          ex2, ',', ey2, '" fill="none" stroke="#8D6E63" stroke-width="0.9"',
          ' stroke-linecap="round"/>\n')
      }
    }
    # Raízes escora (adventícias aéreas) — apenas V6 e V8
    if (vnum >= 6) {
      y_esc <- y_solo - round(h_caule * 0.12)
      out <- paste0(out,
        '<path d="M', x-cw, ',', y_esc, ' Q', x-22, ',', y_solo-6, ' ',
        x-16, ',', y_solo, '" fill="none" stroke="#6D4C41" stroke-width="2.0"',
        ' stroke-linecap="round"/>\n',
        '<path d="M', x+cw, ',', y_esc, ' Q', x+22, ',', y_solo-6, ' ',
        x+16, ',', y_solo, '" fill="none" stroke="#6D4C41" stroke-width="2.0"',
        ' stroke-linecap="round"/>\n')
    }
    if (vnum == 8) {
      y_esc2 <- y_solo - round(h_caule * 0.20)
      out <- paste0(out,
        '<path d="M', x-cw, ',', y_esc2, ' Q', x-30, ',', y_solo-10, ' ',
        x-22, ',', y_solo, '" fill="none" stroke="#795548" stroke-width="1.4"',
        ' stroke-linecap="round"/>\n',
        '<path d="M', x+cw, ',', y_esc2, ' Q', x+30, ',', y_solo-10, ' ',
        x+22, ',', y_solo, '" fill="none" stroke="#795548" stroke-width="1.4"',
        ' stroke-linecap="round"/>\n')
    }

    # ── Caule ────────────────────────────────────────────────────────────────
    out <- paste0(out,
      '<rect x="', x-cw, '" y="', y_topo, '" width="', 2*cw, '" height="', h_caule,
      '" rx="', cw, '" fill="url(#gc)" stroke="#2E7D32" stroke-width="0.5"/>\n')
    # Nós
    for (ni in seq_len(vnum)) {
      yn <- y_solo - ni * esp_no
      out <- paste0(out,
        '<ellipse cx="', x, '" cy="', yn, '" rx="', cw+3, '" ry="',
        round(cw*0.6), '" fill="#33691E" stroke="#1B5E20" stroke-width="0.4"/>\n')
    }

    # ── Folhas ───────────────────────────────────────────────────────────────
    for (fi in seq_len(vnum)) {
      lado   <- if (fi %% 2 == 1) 1 else -1     # ímpar=dir, par=esq
      y_ins  <- y_solo - fi * esp_no
      comp   <- round(28 + fi * 7 * escala)     # folhas maiores nas superiores
      curva  <- round(comp * 0.55)

      # Bézier: base no caule, arco lateral, ponta descendo
      bx1 <- x + lado * round(comp * 0.3)
      by1 <- y_ins - round(comp * 0.25)
      bx2 <- x + lado * comp
      by2 <- y_ins - round(comp * 0.08)

      # Folha destaque ou normal
      e_dest  <- (fi == vnum && dest)
      grad_id <- if (e_dest) id_grad
                 else if (lado == 1) "url(#gfd)" else "url(#gfe)"
      sw_f    <- if (e_dest) "0.9" else "0.5"
      sk_f    <- if (e_dest) "#BF360C" else "#2E7D32"
      if (e_dest && vnum %in% c(6,8)) { sk_f <- "#004D40" }

      # Corpo da folha (shape em bigode fechado)
      cx_mid <- x + lado * round(comp * 0.6)
      cy_mid <- y_ins - round(comp * 0.18)

      out <- paste0(out,
        '<path d="M', x, ',', y_ins,
        ' C', bx1, ',', by1, ' ', bx2, ',', by2-4, ' ', bx2, ',', by2,
        ' C', cx_mid, ',', y_ins+4, ' ', x+lado*4, ',', y_ins+2, ' ', x, ',', y_ins, 'Z"',
        ' fill="', grad_id, '" stroke="', sk_f, '" stroke-width="', sw_f, '"/>\n',
        # Nervura central
        '<line x1="', x, '" y1="', y_ins, '" x2="', bx2, '" y2="', by2,
        '" stroke="#1B5E20" stroke-width="0.7" opacity="0.5"/>\n'
      )

      # Círculo de destaque na folha de aplicação
      if (e_dest) {
        cx_d <- round(x + lado * comp * 0.45)
        cy_d <- round(y_ins - comp * 0.20)
        out  <- paste0(out,
          '<circle cx="', cx_d, '" cy="', cy_d, '" r="22"',
          ' fill="rgba(255,152,0,0.10)" stroke="', cor_arr,
          '" stroke-width="2" stroke-dasharray="5 3"/>\n')
        # Seta + label
        seta_y0 <- cy_d - 36
        out <- paste0(out,
          '<path d="M', cx_d, ',', seta_y0+2, ' L', cx_d, ',', cy_d-24,
          '" fill="none" stroke="', cor_arr, '" stroke-width="1.8"',
          ' marker-end="', id_mark, '"/>\n',
          '<rect x="', cx_d-34, '" y="', seta_y0-20,
          '" width="68" height="20" rx="5"',
          ' fill="', if(vnum==aplicar_em[1]) "#FFF3E0" else "#E0F7FA",
          '" stroke="', cor_arr, '" stroke-width="0.9"/>\n',
          '<text x="', cx_d, '" y="', seta_y0-6,
          '" text-anchor="middle" font-family="sans-serif" font-size="9"',
          ' font-weight="700" fill="', cor_arr, '">',
          vnum, '\u00aa folha \u2190 V', vnum, '</text>\n'
        )
      }
    }

    # ── Espiga (apenas V8) ──────────────────────────────────────────────────
    if (vnum == 8) {
      ye   <- y_solo - round(h_caule * 0.38)
      xe   <- x + 14
      out  <- paste0(out,
        '<path d="M', x+cw, ',', ye,
        ' C', xe+20, ',', ye-12, ' ', xe+36, ',', ye-8, ' ', xe+38, ',', ye+10,
        ' C', xe+36, ',', ye+28, ' ', xe+16, ',', ye+32, ' ', x+cw, ',', ye+20, 'Z"',
        ' fill="url(#gp)" stroke="#2E7D32" stroke-width="0.7"/>\n',
        '<path d="M', x+cw+2, ',', ye+2,
        ' C', xe+18, ',', ye-8, ' ', xe+30, ',', ye-4, ' ', xe+32, ',', ye+10,
        ' C', xe+30, ',', ye+24, ' ', xe+14, ',', ye+27, ' ', x+cw+2, ',', ye+18, 'Z"',
        ' fill="url(#ge)" stroke="#E65100" stroke-width="0.5"/>\n',
        # Fileiras de grãos
        paste(sapply(c(4,9,14), function(dx) paste0(
          '<line x1="', xe+dx, '" y1="', ye-2, '" x2="', xe+dx, '" y2="', ye+24,
          '" stroke="#F57F17" stroke-width="0.5" opacity="0.6"/>\n'
        )), collapse=""),
        # Estilete
        '<path d="M', xe+30, ',', ye-4, ' C', xe+40, ',', ye-16, ' ',
        xe+46, ',', ye-24, '" fill="none" stroke="#C8A84B"',
        ' stroke-width="0.9" stroke-linecap="round"/>\n',
        '<path d="M', xe+28, ',', ye-5, ' C', xe+38, ',', ye-18, ' ',
        xe+44, ',', ye-27, '" fill="none" stroke="#A0856E"',
        ' stroke-width="0.7" stroke-linecap="round"/>\n'
      )
    }

    # ── Rótulo inferior ──────────────────────────────────────────────────────
    cor_lbl <- if (dest) cor_arr else "#4E342E"
    fw_lbl  <- if (dest) "700" else "500"
    out <- paste0(out,
      '<text x="', x, '" y="', y_solo+18,
      '" text-anchor="middle" font-family="sans-serif" font-size="11"',
      ' font-weight="', fw_lbl, '" fill="', cor_lbl, '">V', vnum, '</text>\n',
      if (nchar(dose_txt) > 0)
        paste0('<text x="', x, '" y="', y_solo+30,
               '" text-anchor="middle" font-family="sans-serif" font-size="10"',
               ' font-weight="600" fill="', cor_arr, '">', dose_txt, '</text>\n')
      else "",
      '<text x="', x, '" y="', y_solo + if(nchar(dose_txt)>0) 41 else 30,
      '" text-anchor="middle" font-family="sans-serif" font-size="9" fill="#9E9E9E">',
      c(V2="\u224810d", V4="\u224825d", V6="\u224835d", V8="\u224845d")[[paste0("V",vnum)]],
      '</text>\n'
    )
    out
  }

  # Gera todas as plantas
  plantas <- paste(sapply(seq_along(estadios), function(i)
    desenhar_planta(xs[i], estadios[i])), collapse="")

  # Seta temporal
  seta <- paste0(
    '<line x1="30" y1="', viewH-56, '" x2="', viewW-20, '" y2="', viewH-56,
    '" stroke="#795548" stroke-width="1.5" marker-end="url(#arrt)"/>\n',
    paste(sapply(seq_along(estadios), function(i) paste0(
      '<line x1="', xs[i], '" y1="', viewH-60, '" x2="', xs[i], '" y2="', viewH-52,
      '" stroke="#795548" stroke-width="1"/>\n'
    )), collapse=""),
    '<text x="', viewW/2, '" y="', viewH-42,
    '" text-anchor="middle" font-family="sans-serif" font-size="10" fill="#795548">',
    'Desenvolvimento vegetativo (dias ap\u00f3s emerg\u00eancia)</text>\n'
  )

  # Legenda
  leg_y <- viewH - 28
  p1v   <- parcelas[[1]]$v
  leg   <- paste0(
    '<rect x="', viewW/2 - 220, '" y="', leg_y-10, '" width="170" height="20"',
    ' rx="6" fill="#FFF3E0" stroke="#FF8F00" stroke-width="1"/>\n',
    '<circle cx="', viewW/2-210, '" cy="', leg_y, '" r="5" fill="none"',
    ' stroke="#FF8F00" stroke-width="2" stroke-dasharray="3 2"/>\n',
    '<text x="', viewW/2-196, '" y="', leg_y+4,
    '" font-family="sans-serif" font-size="10" font-weight="700" fill="#E65100">',
    '1\u00aa parcela \u2014 ', p1v, ' (laranja)</text>\n',
    if (length(parcelas) > 1) paste0(
      '<rect x="', viewW/2+50, '" y="', leg_y-10, '" width="170" height="20"',
      ' rx="6" fill="#E0F7FA" stroke="#00ACC1" stroke-width="1"/>\n',
      '<circle cx="', viewW/2+60, '" cy="', leg_y, '" r="5" fill="none"',
      ' stroke="#00ACC1" stroke-width="2" stroke-dasharray="3 2"/>\n',
      '<text x="', viewW/2+74, '" y="', leg_y+4,
      '" font-family="sans-serif" font-size="10" font-weight="700" fill="#006064">',
      '2\u00aa parcela \u2014 ', parcelas[[2]]$v, ' (azul-teal)</text>\n'
    ) else ""
  )

  paste0(
    '<svg width="100%" viewBox="0 0 ', viewW, ' ', viewH,
    '" role="img" xmlns="http://www.w3.org/2000/svg">\n',
    '<title>Est\u00e1dio fenol\u00f3gico do milho \u2014 aduba\u00e7\u00e3o de N em cobertura</title>\n',
    '<desc>Ilustra\u00e7\u00e3o bot\u00e2nica dos est\u00e1dios vegetativos do milho com',
    ' indica\u00e7\u00e3o do momento de aplica\u00e7\u00e3o de N em cobertura.</desc>\n',
    defs, '\n',
    header,
    solo,
    plantas,
    seta,
    leg,
    '</svg>\n'
  )
}
