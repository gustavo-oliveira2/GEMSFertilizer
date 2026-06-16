# ==============================================================================
# MÓDULO DE ANÁLISES ESTATÍSTICAS — aba Pesquisa
# Correlação, Regressão, ANOVA, PCA, Cluster e Limiares Críticos
# (Cate-Nelson e Linear-Platô)
# ==============================================================================

# ------------------------------------------------------------------------------
# UTILITÁRIOS
# ------------------------------------------------------------------------------

# Atributos numéricos disponíveis no df, com nomes amigáveis
atributos_numericos_disponiveis <- function(df) {
  candidatos <- intersect(names(nomes_atributos_regional), names(df))
  candidatos <- candidatos[sapply(candidatos, function(c) {
    sum(!is.na(df[[c]])) >= 3
  })]
  setNames(candidatos, nomes_atributos_regional[candidatos])
}

# Variáveis categóricas para agrupamento (ANOVA, filtros)
grupos_categoricos_disponiveis <- function(df) {
  cands <- c(municipio = "municipio", ano = "ano",
             cultura = "cultura", tipo_parcela = "tipo_parcela")
  cands <- cands[cands %in% names(df)]
  cands <- cands[sapply(cands, function(c) length(unique(stats::na.omit(df[[c]]))) >= 2)]
  nomes <- c(municipio = "Munic\u00edpio", ano = "Ano",
             cultura = "Cultura", tipo_parcela = "Tipo de Parcela")
  setNames(cands, nomes[cands])
}

# ------------------------------------------------------------------------------
# 1. MATRIZ DE CORRELAÇÃO
# ------------------------------------------------------------------------------
calc_correlacao <- function(df, vars, metodo = "pearson") {
  d <- df[, vars, drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  n <- nrow(d)

  if (n < 4 || length(vars) < 2) {
    return(list(erro = "Dados insuficientes (m\u00ednimo 4 amostras completas e 2 atributos)."))
  }

  cor_mat <- stats::cor(d, method = metodo)
  p_mat <- matrix(NA_real_, length(vars), length(vars),
                   dimnames = list(vars, vars))
  for (i in seq_along(vars)) {
    for (j in seq_along(vars)) {
      if (i == j) { p_mat[i, j] <- 0; next }
      teste <- tryCatch(stats::cor.test(d[[i]], d[[j]], method = metodo),
                         error = function(e) NULL)
      p_mat[i, j] <- if (!is.null(teste)) teste$p.value else NA
    }
  }

  # Pares mais relevantes (|r| > 0.4, exclui diagonal, sem duplicar)
  pares <- list()
  for (i in seq_along(vars)) {
    for (j in seq_along(vars)) {
      if (j <= i) next
      r <- cor_mat[i, j]; p <- p_mat[i, j]
      if (!is.na(r) && abs(r) >= 0.4) {
        pares[[length(pares) + 1]] <- data.frame(
          var1 = vars[i], var2 = vars[j], r = round(r, 3), p = round(p, 4)
        )
      }
    }
  }
  pares_df <- if (length(pares) > 0) do.call(rbind, pares) else
    data.frame(var1=character(0), var2=character(0), r=numeric(0), p=numeric(0))
  pares_df <- pares_df[order(-abs(pares_df$r)), , drop = FALSE]

  list(erro = NULL, cor = cor_mat, p = p_mat, n = n, vars = vars, pares = pares_df)
}

# ------------------------------------------------------------------------------
# 2. REGRESSÃO (simples e polinomial)
# ------------------------------------------------------------------------------
calc_regressao <- function(df, x, y, grau = 1) {
  d <- df[, c(x, y)]
  names(d) <- c("x", "y")
  d <- d[stats::complete.cases(d), ]

  if (nrow(d) < 4) {
    return(list(erro = "Dados insuficientes (m\u00ednimo 4 amostras completas)."))
  }

  grau <- min(grau, 3, nrow(d) - 2)
  grau <- max(grau, 1)

  formula_str <- if (grau == 1) "y ~ x" else paste0("y ~ poly(x, ", grau, ", raw = TRUE)")
  mod <- stats::lm(stats::as.formula(formula_str), data = d)

  r2 <- summary(mod)$r.squared
  p_modelo <- tryCatch({
    fstat <- summary(mod)$fstatistic
    stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
  }, error = function(e) NA)

  # Curva ajustada para plotagem
  x_seq <- seq(min(d$x), max(d$x), length.out = 100)
  pred  <- stats::predict(mod, newdata = data.frame(x = x_seq))

  list(erro = NULL, modelo = mod, r2 = r2, p = p_modelo, n = nrow(d),
       dados = d, x_seq = x_seq, pred = pred, grau = grau,
       coef = stats::coef(mod))
}

# ------------------------------------------------------------------------------
# 2b. REGRESSÃO MÚLTIPLA
# ------------------------------------------------------------------------------
calc_regressao_multipla <- function(df, x_vars, y_var) {
  if (length(x_vars) < 2) {
    return(list(erro = "Selecione ao menos 2 vari\u00e1veis explicativas (X)."))
  }
  if (y_var %in% x_vars) {
    return(list(erro = "A vari\u00e1vel dependente (Y) n\u00e3o pode estar entre as explicativas (X)."))
  }

  vars <- c(x_vars, y_var)
  d <- df[, vars, drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  n <- nrow(d)

  if (n < length(x_vars) + 3) {
    return(list(erro = paste0(
      "Dados insuficientes: s\u00e3o necess\u00e1rias pelo menos ", length(x_vars) + 3,
      " amostras completas para ", length(x_vars), " vari\u00e1veis explicativas (h\u00e1 ", n, ")."
    )))
  }

  names(d) <- c(paste0("x", seq_along(x_vars)), "y")
  formula_str <- paste("y ~", paste(names(d)[seq_along(x_vars)], collapse = " + "))
  mod <- stats::lm(stats::as.formula(formula_str), data = d)
  s <- summary(mod)

  # Coeficientes não-padronizados (unidades originais)
  coefs <- as.data.frame(s$coefficients)
  coefs$variavel <- c("(Intercepto)", x_vars)
  rownames(coefs) <- NULL
  names(coefs) <- c("estimativa", "erro_padrao", "t", "p_valor", "variavel")
  coefs <- coefs[, c("variavel", "estimativa", "erro_padrao", "t", "p_valor")]
  coefs[, 2:5] <- round(coefs[, 2:5], 4)

  # Coeficientes padronizados (betas) — magnitude relativa de cada preditor
  d_std <- as.data.frame(scale(d))
  mod_std <- stats::lm(stats::as.formula(formula_str), data = d_std)
  betas <- stats::coef(mod_std)[-1]
  names(betas) <- x_vars

  # VIF (Fator de Inflação da Variância), calculado sem pacotes extras:
  # VIF_i = 1 / (1 - R²_i), onde R²_i vem da regressão de Xi sobre os demais X's
  vif <- sapply(seq_along(x_vars), function(i) {
    outros <- setdiff(names(d)[seq_along(x_vars)], names(d)[i])
    if (length(outros) == 0) return(1)
    f <- stats::as.formula(paste(names(d)[i], "~", paste(outros, collapse = " + ")))
    r2i <- summary(stats::lm(f, data = d))$r.squared
    if (r2i >= 0.999) Inf else round(1 / (1 - r2i), 2)
  })
  names(vif) <- x_vars

  f_stat <- s$fstatistic
  f_p <- stats::pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

  list(erro = NULL, modelo = mod, coefs = coefs, betas = round(betas, 3), vif = vif,
       r2 = round(s$r.squared, 3), r2_adj = round(s$adj.r.squared, 3),
       f_stat = round(unname(f_stat[1]), 2), f_p = round(unname(f_p), 5),
       n = n, x_vars = x_vars, y_var = y_var,
       observado = d$y, predito = unname(stats::predict(mod)))
}

# ------------------------------------------------------------------------------
# 2c. ANÁLISE DE TRILHA (Path Analysis) — efeitos diretos e indiretos
# ------------------------------------------------------------------------------
calc_analise_trilha <- function(df, x_vars, y_var) {
  if (length(x_vars) < 2) {
    return(list(erro = "Selecione ao menos 2 vari\u00e1veis explicativas (X)."))
  }
  if (y_var %in% x_vars) {
    return(list(erro = "A vari\u00e1vel principal (Y) n\u00e3o pode estar entre as explicativas (X)."))
  }

  vars <- c(x_vars, y_var)
  d <- df[, vars, drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  n <- nrow(d)

  if (n < length(x_vars) + 3) {
    return(list(erro = paste0(
      "Dados insuficientes: s\u00e3o necess\u00e1rias pelo menos ", length(x_vars) + 3,
      " amostras completas para ", length(x_vars), " vari\u00e1veis explicativas (h\u00e1 ", n, ")."
    )))
  }

  # Padroniza tudo (m\u00e9dia 0, vari\u00e2ncia 1): os coeficientes da regress\u00e3o
  # padronizada de Y sobre os X's SÃO os efeitos diretos (coeficientes de trilha)
  d_std <- as.data.frame(scale(d))
  names(d_std) <- c(paste0("x", seq_along(x_vars)), "y")

  formula_str <- paste("y ~", paste(names(d_std)[seq_along(x_vars)], collapse = " + "))
  mod_std <- stats::lm(stats::as.formula(formula_str), data = d_std)
  diretos <- stats::coef(mod_std)[-1]
  names(diretos) <- x_vars

  # Matriz de correlações simples entre as variáveis explicativas
  cor_x <- stats::cor(d[, x_vars, drop = FALSE])

  # Correlação simples de cada X com Y
  cor_y <- sapply(x_vars, function(v) stats::cor(d[[v]], d[[y_var]]))
  names(cor_y) <- x_vars

  # Efeito indireto de Xi via Xj = r(Xi,Xj) * efeito_direto(Xj)
  indiretos_detalhe <- matrix(0, nrow = length(x_vars), ncol = length(x_vars),
                               dimnames = list(x_vars, x_vars))
  for (i in x_vars) {
    for (j in x_vars) {
      if (i == j) next
      indiretos_detalhe[i, j] <- cor_x[i, j] * diretos[j]
    }
  }
  indireto_total <- rowSums(indiretos_detalhe)

  resumo <- data.frame(
    variavel          = x_vars,
    correlacao_total  = round(unname(cor_y[x_vars]), 3),
    efeito_direto     = round(unname(diretos[x_vars]), 3),
    efeito_indireto   = round(unname(indireto_total[x_vars]), 3),
    stringsAsFactors  = FALSE
  )
  resumo$residuo <- round(resumo$correlacao_total - (resumo$efeito_direto + resumo$efeito_indireto), 3)

  # Diagnóstico de multicolinearidade: determinante da matriz de correlação
  # entre os X's próximo de 0 indica redundância (Cruz & Carneiro, 2003)
  det_cor <- det(cor_x)
  multicolinear <- det_cor < 0.05

  r2 <- summary(mod_std)$r.squared

  list(erro = NULL, resumo = resumo, indiretos_detalhe = round(indiretos_detalhe, 3),
       cor_x = round(cor_x, 3), det_cor = round(det_cor, 5), multicolinear = multicolinear,
       r2 = round(r2, 3), n = n, x_vars = x_vars, y_var = y_var)
}

# ------------------------------------------------------------------------------
# 3. ANOVA + TUKEY
# ------------------------------------------------------------------------------
calc_anova <- function(df, atributo, grupo) {
  d <- df[, c(atributo, grupo)]
  names(d) <- c("y", "g")
  d <- d[stats::complete.cases(d), ]
  d$g <- as.factor(d$g)

  if (nrow(d) < 4 || length(levels(d$g)) < 2) {
    return(list(erro = "Dados insuficientes ou apenas um grupo dispon\u00edvel."))
  }
  if (any(table(d$g) < 2)) {
    return(list(erro = "H\u00e1 grupo(s) com menos de 2 observa\u00e7\u00f5es \u2014 remova-os ou agregue."))
  }

  mod   <- stats::aov(y ~ g, data = d)
  resumo <- summary(mod)
  p_val  <- resumo[[1]][["Pr(>F)"]][1]
  tukey  <- stats::TukeyHSD(mod)

  # Médias por grupo
  medias <- do.call(rbind, lapply(split(d$y, d$g), function(x) {
    data.frame(n = length(x), media = round(mean(x), 2), dp = round(stats::sd(x), 2))
  }))
  medias$grupo <- rownames(medias)
  rownames(medias) <- NULL
  medias <- medias[order(-medias$media), c("grupo","n","media","dp")]

  # Pares significativos do Tukey
  tk_df <- as.data.frame(tukey$g)
  tk_df$par <- rownames(tk_df)
  names(tk_df) <- c("diferenca", "lwr", "upr", "p_adj", "par")
  tk_sig <- tk_df[tk_df$p_adj < 0.05, c("par", "diferenca", "p_adj")]
  tk_sig$diferenca <- round(tk_sig$diferenca, 2)
  tk_sig$p_adj <- round(tk_sig$p_adj, 4)
  names(tk_sig) <- c("Compara\u00e7\u00e3o", "Diferen\u00e7a", "p-ajustado")

  list(erro = NULL, p_global = p_val, medias = medias, tukey_sig = tk_sig,
       n = nrow(d), dados = d, k = length(levels(d$g)))
}

# ------------------------------------------------------------------------------
# 4. PCA (componentes principais)
# ------------------------------------------------------------------------------
calc_pca <- function(df, vars) {
  d <- df[, vars, drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]

  if (nrow(d) < 5 || length(vars) < 3) {
    return(list(erro = "Dados insuficientes (m\u00ednimo 5 amostras e 3 atributos)."))
  }

  # Remove colunas com variância zero
  var_ok <- sapply(d, function(x) stats::sd(x) > 0)
  if (sum(var_ok) < 3) {
    return(list(erro = "Variabilidade insuficiente em pelo menos 3 atributos."))
  }
  d <- d[, var_ok, drop = FALSE]
  vars <- names(d)

  pca <- stats::prcomp(d, scale. = TRUE, center = TRUE)
  var_exp <- (pca$sdev^2) / sum(pca$sdev^2) * 100

  scores   <- as.data.frame(pca$x[, 1:2, drop = FALSE])
  loadings <- as.data.frame(pca$rotation[, 1:2, drop = FALSE])
  loadings$var <- rownames(loadings)

  list(erro = NULL, pca = pca, scores = scores, loadings = loadings,
       var_exp = round(var_exp[1:2], 1), vars = vars, n = nrow(d))
}

# ------------------------------------------------------------------------------
# 5. CLUSTER (k-means)
# ------------------------------------------------------------------------------
calc_cluster <- function(df, vars, k = 3) {
  d <- df[, vars, drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]

  if (nrow(d) < k * 2 || length(vars) < 2) {
    return(list(erro = paste0("Dados insuficientes para ", k, " grupos (m\u00ednimo ", k*2, " amostras).")))
  }

  var_ok <- sapply(d, function(x) stats::sd(x) > 0)
  if (sum(var_ok) < 2) return(list(erro = "Variabilidade insuficiente nos atributos selecionados."))
  d <- d[, var_ok, drop = FALSE]
  vars <- names(d)

  ds <- scale(d)
  set.seed(42)
  km <- stats::kmeans(ds, centers = k, nstart = 25)

  # PCA para visualização 2D
  pca <- stats::prcomp(ds, scale. = FALSE)
  scores <- as.data.frame(pca$x[, 1:2, drop = FALSE])
  scores$cluster <- factor(km$cluster)
  var_exp <- round((pca$sdev^2 / sum(pca$sdev^2) * 100)[1:2], 1)

  # Perfil médio por cluster (escala original)
  d$cluster <- factor(km$cluster)
  perfil <- do.call(rbind, lapply(split(d[, vars, drop=FALSE], d$cluster), function(sub) {
    sapply(sub, mean)
  }))
  perfil <- as.data.frame(round(perfil, 2))
  perfil$cluster <- rownames(perfil)
  perfil$n <- as.integer(table(d$cluster))

  list(erro = NULL, kmeans = km, scores = scores, var_exp = var_exp,
       perfil = perfil, vars = vars, n = nrow(d), k = k)
}

# ------------------------------------------------------------------------------
# 6. CATE-NELSON (limiar crítico — classificação por quadrantes)
# ------------------------------------------------------------------------------
calc_cate_nelson <- function(df, x_var, y_var = "produtividade",
                              cultura = NULL, y_critico_pct = 90,
                              apenas_testemunha = FALSE) {

  d <- df
  if (!is.null(cultura) && cultura != "Todas" && "cultura" %in% names(d)) {
    d <- d[d$cultura == cultura, ]
  }
  if (apenas_testemunha && "tipo_parcela" %in% names(d)) {
    ok <- grepl("Testemunha", d$tipo_parcela, ignore.case = TRUE)
    ok[is.na(ok)] <- FALSE
    d <- d[ok, ]
  }
  d <- d[!is.na(d[[x_var]]) & !is.na(d[[y_var]]) & d[[y_var]] > 0, ]

  if (nrow(d) < 8) {
    return(list(erro = "Dados insuficientes (m\u00ednimo 8 pontos com produtividade > 0)."))
  }

  ymax <- max(d[[y_var]])
  d$y_rel <- d[[y_var]] / ymax * 100
  yc <- y_critico_pct

  candidatos <- sort(unique(d[[x_var]]))
  if (length(candidatos) > 2) candidatos <- candidatos[-c(1, length(candidatos))]

  resultados <- lapply(candidatos, function(xc) {
    baixo <- d[[x_var]] < xc
    acerto_q1 <- sum(baixo  & d$y_rel <  yc)   # X baixo, Y baixo (resposta esperada)
    acerto_q2 <- sum(!baixo & d$y_rel >= yc)   # X alto, Y alto (sem resposta)
    list(xc = xc, acertos = acerto_q1 + acerto_q2)
  })

  acertos_vec <- sapply(resultados, function(r) r$acertos)
  melhor_idx  <- which.max(acertos_vec)
  melhor      <- resultados[[melhor_idx]]
  pct_acerto  <- round(melhor$acertos / nrow(d) * 100, 1)

  # Classificação de cada ponto (quadrante)
  d$quadrante <- ifelse(d[[x_var]] < melhor$xc & d$y_rel < yc, "Resposta esperada (X baixo, Y baixo)",
                  ifelse(d[[x_var]] >= melhor$xc & d$y_rel >= yc, "Sem resposta (X alto, Y alto)",
                  "Discordante"))

  list(erro = NULL, xc = melhor$xc, y_critico = yc, ymax = ymax,
       pct_acerto = pct_acerto, n = nrow(d), dados = d,
       x_var = x_var, y_var = y_var)
}

# ------------------------------------------------------------------------------
# 7. LINEAR-PLATÔ (regressão segmentada por busca em grade)
# ------------------------------------------------------------------------------
calc_linear_plato <- function(df, x_var, y_var = "produtividade",
                              cultura = NULL, apenas_testemunha = FALSE) {

  d <- df
  if (!is.null(cultura) && cultura != "Todas" && "cultura" %in% names(d)) {
    d <- d[d$cultura == cultura, ]
  }
  if (apenas_testemunha && "tipo_parcela" %in% names(d)) {
    ok <- grepl("Testemunha", d$tipo_parcela, ignore.case = TRUE)
    ok[is.na(ok)] <- FALSE
    d <- d[ok, ]
  }
  d <- d[!is.na(d[[x_var]]) & !is.na(d[[y_var]]) & d[[y_var]] > 0, ]

  if (nrow(d) < 8) {
    return(list(erro = "Dados insuficientes (m\u00ednimo 8 pontos com produtividade > 0)."))
  }

  x <- d[[x_var]]; y <- d[[y_var]]

  candidatos <- seq(min(x), max(x), length.out = 60)
  candidatos <- candidatos[candidatos > min(x) & candidatos < max(x)]
  if (length(candidatos) == 0) {
    return(list(erro = "N\u00e3o foi poss\u00edvel determinar candidatos a ponto de quebra (valores de X muito pr\u00f3ximos)."))
  }

  melhor <- list(bp = NA, rss = Inf)
  for (bp in candidatos) {
    xseg <- pmin(x, bp)
    mod  <- stats::lm(y ~ xseg)
    rss  <- sum(stats::residuals(mod)^2)
    if (rss < melhor$rss) {
      melhor <- list(bp = bp, rss = rss, modelo = mod,
                      intercepto = unname(stats::coef(mod)[1]),
                      inclinacao = unname(stats::coef(mod)[2]))
    }
  }

  sst <- sum((y - mean(y))^2)
  r2  <- round(1 - melhor$rss / sst, 3)
  plato_val <- round(melhor$intercepto + melhor$inclinacao * melhor$bp, 1)

  # Curva ajustada
  x_seq <- seq(min(x), max(x), length.out = 100)
  pred  <- ifelse(x_seq < melhor$bp,
                   melhor$intercepto + melhor$inclinacao * x_seq,
                   plato_val)

  list(erro = NULL, breakpoint = round(melhor$bp, 2),
       intercepto = round(melhor$intercepto, 2),
       inclinacao = round(melhor$inclinacao, 3),
       plato = plato_val, r2 = r2, n = nrow(d), dados = d,
       x_seq = x_seq, pred = pred, x_var = x_var, y_var = y_var)
}

# ------------------------------------------------------------------------------
# TEXTOS DE INTERPRETAÇÃO AUTOMÁTICA
# ------------------------------------------------------------------------------
interpretar_correlacao_par <- function(r, p) {
  forca <- if (abs(r) >= 0.7) "forte"
           else if (abs(r) >= 0.4) "moderada"
           else "fraca"
  direcao <- if (r > 0) "positiva" else "negativa"
  sig <- if (!is.na(p) && p < 0.05) "estatisticamente significativa (p &lt; 0.05)"
         else "n\u00e3o significativa (p \u2265 0.05)"
  paste0("Correla\u00e7\u00e3o ", forca, " ", direcao, " (r = ", round(r,3), "), ", sig, ".")
}

interpretar_r2 <- function(r2) {
  if (r2 >= 0.7) "O modelo explica uma propor\u00e7\u00e3o alta da variabilidade."
  else if (r2 >= 0.4) "O modelo explica uma propor\u00e7\u00e3o moderada da variabilidade."
  else "O modelo explica uma propor\u00e7\u00e3o baixa da variabilidade \u2014 outros fatores provavelmente dominam."
}

interpretar_cate_nelson <- function(cn, nome_x) {
  paste0(
    "Limiar cr\u00edtico estimado para ", nome_x, ": <b>", round(cn$xc, 2), "</b>. ",
    "Classifica\u00e7\u00e3o correta de ", cn$pct_acerto, "% dos pontos (n = ", cn$n, "), ",
    "considerando produtividade relativa cr\u00edtica de ", cn$y_critico, "% do m\u00e1ximo observado ",
    "(", round(cn$ymax, 0), " kg/ha). ",
    if (cn$pct_acerto < 70)
      "Classifica\u00e7\u00e3o abaixo de 70% sugere alta dispers\u00e3o \u2014 considere filtrar por cultura ou usar apenas parcelas testemunha."
    else
      "Classifica\u00e7\u00e3o acima de 70% indica boa separa\u00e7\u00e3o entre as classes de resposta."
  )
}

interpretar_linear_plato <- function(lp, nome_x) {
  paste0(
    "Limiar cr\u00edtico (ponto de quebra) estimado para ", nome_x, ": <b>", lp$breakpoint, "</b>. ",
    "Abaixo desse valor, cada unidade adicional de ", nome_x,
    " associa-se a uma varia\u00e7\u00e3o de ", lp$inclinacao, " kg/ha na produtividade. ",
    "Acima do limiar, a produtividade tende a um plat\u00f4 de aproximadamente ", lp$plato, " kg/ha. ",
    "R\u00b2 = ", lp$r2, " (n = ", lp$n, "). ", interpretar_r2(lp$r2)
  )
}

interpretar_regressao_multipla <- function(res, nomes_x, nome_y) {
  idx_max <- which.max(abs(res$betas))
  var_max <- names(res$betas)[idx_max]
  nome_var_max <- nomes_x[[var_max]] %||% var_max

  vif_alto <- names(res$vif)[res$vif > 10 & is.finite(res$vif)]
  vif_inf  <- names(res$vif)[!is.finite(res$vif)]
  txt_vif <- ""
  if (length(vif_inf) > 0) {
    nomes_inf <- sapply(vif_inf, function(v) nomes_x[[v]] %||% v)
    txt_vif <- paste0(
      "<br><br>\u26d4 <b>Multicolinearidade severa</b>: ", paste(nomes_inf, collapse = ", "),
      " \u00e9 (quase) combina\u00e7\u00e3o linear das demais vari\u00e1veis (VIF \u2192 \u221e). ",
      "Remova vari\u00e1veis redundantes antes de interpretar os coeficientes."
    )
  } else if (length(vif_alto) > 0) {
    nomes_alto <- sapply(vif_alto, function(v) nomes_x[[v]] %||% v)
    txt_vif <- paste0(
      "<br><br>\u26a0\ufe0f <b>Multicolinearidade alta</b> (VIF &gt; 10) em: ",
      paste(nomes_alto, collapse = ", "),
      " \u2014 os coeficientes dessas vari\u00e1veis podem ser inst\u00e1veis; considere remover redundantes."
    )
  }

  paste0(
    "Modelo: ", nome_y, " ~ ", paste(sapply(res$x_vars, function(v) nomes_x[[v]] %||% v), collapse = " + "), ". ",
    "R\u00b2 = ", res$r2, " (R\u00b2 ajustado = ", res$r2_adj, "), F = ", res$f_stat,
    ", p ", if (res$f_p < 0.001) "&lt; 0.001" else paste0("= ", res$f_p), ". ",
    interpretar_r2(res$r2), " ",
    "A vari\u00e1vel com maior efeito padronizado (|\u03b2|) \u00e9 <b>", nome_var_max,
    "</b> (\u03b2 = ", res$betas[[idx_max]], ") \u2014 isto \u00e9, mantendo as demais vari\u00e1veis ",
    "constantes, \u00e9 a que mais influencia ", nome_y, " em termos relativos.",
    txt_vif
  )
}

interpretar_trilha <- function(res, nomes_x, nome_y) {
  idx_max <- which.max(abs(res$resumo$efeito_direto))
  var_max <- res$resumo$variavel[idx_max]
  nome_var_max <- nomes_x[[var_max]] %||% var_max

  idx_indireto <- which(abs(res$resumo$efeito_indireto) > abs(res$resumo$efeito_direto) &
                         abs(res$resumo$correlacao_total) > 0.1)
  txt_indireto <- ""
  if (length(idx_indireto) > 0) {
    nomes_ind <- sapply(res$resumo$variavel[idx_indireto], function(v) nomes_x[[v]] %||% v)
    txt_indireto <- paste0(
      "<br><br>Para <b>", paste(nomes_ind, collapse = ", "), "</b>, o efeito INDIRETO ",
      "(via correla\u00e7\u00e3o com as demais vari\u00e1veis) supera o efeito DIRETO em magnitude ",
      "\u2014 a correla\u00e7\u00e3o simples dessas vari\u00e1veis com ", nome_y,
      " reflete majoritariamente outras vari\u00e1veis do modelo, n\u00e3o um efeito pr\u00f3prio."
    )
  }

  txt_multicol <- ""
  if (res$multicolinear) {
    txt_multicol <- paste0(
      "<br><br>\u26a0\ufe0f <b>Alerta de multicolinearidade</b>: o determinante da matriz de ",
      "correla\u00e7\u00f5es entre as vari\u00e1veis explicativas \u00e9 ", res$det_cor,
      " (pr\u00f3ximo de 0) \u2014 segundo Cruz &amp; Carneiro (2003), valores baixos indicam ",
      "redund\u00e2ncia entre os X's, o que pode inflar/desestabilizar os efeitos diretos ",
      "estimados. Considere remover vari\u00e1veis altamente correlacionadas entre si."
    )
  }

  paste0(
    "Decomposi\u00e7\u00e3o da correla\u00e7\u00e3o de cada vari\u00e1vel com ", nome_y,
    " em efeito DIRETO (coeficiente de trilha, controlando as demais vari\u00e1veis) e ",
    "INDIRETO (mediado pelas demais vari\u00e1veis). R\u00b2 do modelo padronizado = ",
    res$r2, " (n = ", res$n, "). ",
    "Maior efeito direto: <b>", nome_var_max, "</b> (", res$resumo$efeito_direto[idx_max],
    "), correla\u00e7\u00e3o total com ", nome_y, " = ", res$resumo$correlacao_total[idx_max], ".",
    txt_indireto, txt_multicol
  )
}
