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
# 3. ANOVA + TUKEY (≤10 grupos) ou SCOTT-KNOTT (>10 grupos) com letras
# ------------------------------------------------------------------------------
calc_anova <- function(df, atributo, grupo) {
  d <- df[, c(atributo, grupo)]
  names(d) <- c("y", "g")
  d <- d[stats::complete.cases(d), ]
  d$g <- as.factor(d$g)
  k <- length(levels(d$g))

  if (nrow(d) < 4 || k < 2)
    return(list(erro = "Dados insuficientes ou apenas um grupo dispon\u00edvel."))
  if (any(table(d$g) < 2))
    return(list(erro = "H\u00e1 grupo(s) com menos de 2 observa\u00e7\u00f5es \u2014 remova-os ou agregue."))

  mod    <- stats::aov(y ~ g, data = d)
  resumo <- summary(mod)
  p_val  <- resumo[[1]][["Pr(>F)"]][1]
  f_val  <- resumo[[1]][["F value"]][1]
  gl_res <- resumo[[1]][["Df"]][2]
  qmr    <- resumo[[1]][["Mean Sq"]][2]  # Quadrado Médio do Resíduo

  # Médias por grupo
  medias_lst <- lapply(split(d$y, d$g), function(x)
    data.frame(n = length(x), media = round(mean(x), 4), dp = round(stats::sd(x), 4))
  )
  medias <- do.call(rbind, medias_lst)
  medias$grupo <- rownames(medias)
  rownames(medias) <- NULL
  medias <- medias[order(-medias$media), c("grupo","n","media","dp")]

  metodo_posthoc <- if (k <= 10) "Tukey" else "Scott-Knott"

  # ── PÓS-HOC ────────────────────────────────────────────────────────────────
  if (metodo_posthoc == "Tukey") {
    letras <- tukey_letras(d, qmr, gl_res, k)
  } else {
    letras <- scott_knott_letras(d, qmr, gl_res)
  }

  # Junta letras nas médias
  medias$letra <- letras[medias$grupo]
  medias$media_letra <- paste0(medias$media, " ", medias$letra)

  list(
    erro = NULL, p_global = round(p_val, 5), f_val = round(f_val, 3),
    gl_trat = k - 1, gl_res = gl_res,
    medias = medias, letras = letras,
    n = nrow(d), dados = d, k = k,
    metodo_posthoc = metodo_posthoc
  )
}

# ── TUKEY — letras de agrupamento (sem pacotes externos) ─────────────────────
tukey_letras <- function(d, qmr, gl_res, k) {
  lvls  <- levels(d$g)
  medias_v <- tapply(d$y, d$g, mean)
  ns_v     <- tapply(d$y, d$g, length)
  alpha <- 0.05

  # Matriz de diferenças significativas entre pares
  sig_mat <- matrix(FALSE, k, k, dimnames = list(lvls, lvls))
  for (i in seq_len(k - 1)) {
    for (j in (i + 1):k) {
      gi <- lvls[i]; gj <- lvls[j]
      ni <- ns_v[gi]; nj <- ns_v[gj]
      se <- sqrt(qmr * (1/ni + 1/nj) / 2)
      q  <- abs(medias_v[gi] - medias_v[gj]) / se
      # p-valor via distribuição Tukey (Studentized Range)
      p  <- 1 - stats::ptukey(q, k, gl_res)
      sig_mat[gi, gj] <- p < alpha
      sig_mat[gj, gi] <- p < alpha
    }
  }
  atribuir_letras(lvls, medias_v, sig_mat)
}

# ── SCOTT-KNOTT — algoritmo de partição recursiva ────────────────────────────
scott_knott_letras <- function(d, qmr, gl_res) {
  lvls     <- levels(d$g)
  medias_v <- sort(tapply(d$y, d$g, mean))
  ns_v     <- tapply(d$y, d$g, length)
  k        <- length(lvls)

  # Particionamento recursivo de Scott-Knott
  grupos <- sk_recursivo(names(medias_v), medias_v, ns_v, qmr, gl_res)

  # Atribui letras a partir dos grupos (a = maior média)
  letras <- character(k)
  names(letras) <- names(medias_v)
  letra_seq <- letters
  for (idx in seq_along(grupos)) {
    for (g in grupos[[idx]]) letras[g] <- letra_seq[idx]
  }
  letras
}

sk_recursivo <- function(nomes, medias, ns, qmr, gl_res, grupos = list()) {
  m <- length(nomes)
  if (m <= 1) return(c(grupos, list(nomes)))

  n_tot  <- sum(ns[nomes])
  media_g <- sum(medias[nomes] * ns[nomes]) / n_tot

  # Estatística lambda de Scott-Knott — maior partição que maximiza SQentre
  melhor_lambda <- -Inf
  melhor_corte  <- 1

  for (corte in seq_len(m - 1)) {
    g1 <- nomes[seq_len(corte)]
    g2 <- nomes[(corte + 1):m]
    n1 <- sum(ns[g1]); n2 <- sum(ns[g2])
    m1 <- sum(medias[g1] * ns[g1]) / n1
    m2 <- sum(medias[g2] * ns[g2]) / n2
    sq <- n1 * n2 / (n1 + n2) * (m1 - m2)^2
    if (sq > melhor_lambda) {
      melhor_lambda <- sq
      melhor_corte  <- corte
    }
  }

  # Teste qui-quadrado de Scott-Knott
  # B = lambda / (sigma² total estimado)
  sigma2 <- qmr
  B <- melhor_lambda / sigma2
  # Graus de liberdade = k partições - 1 (aprox.)
  p_sk <- 1 - stats::pchisq(B, df = 1)

  g1 <- nomes[seq_len(melhor_corte)]
  g2 <- nomes[(melhor_corte + 1):m]

  if (p_sk < 0.05) {
    # Divisão significativa — recursão em cada subgrupo
    r1 <- sk_recursivo(g1, medias, ns, qmr, gl_res, list())
    r2 <- sk_recursivo(g2, medias, ns, qmr, gl_res, list())
    c(grupos, r1, r2)
  } else {
    # Sem divisão — todos no mesmo grupo
    c(grupos, list(nomes))
  }
}

# ── ATRIBUI LETRAS a partir de matriz de significância (Tukey) ───────────────
atribuir_letras <- function(lvls, medias_v, sig_mat) {
  # Ordena por média decrescente
  ord   <- names(sort(medias_v, decreasing = TRUE))
  k     <- length(ord)
  letras <- character(k)
  names(letras) <- ord

  letra_atual <- 1
  grupos_ativos <- list()  # lista de vetores de grupos em andamento

  for (i in seq_len(k)) {
    trt <- ord[i]
    # Verifica se este tratamento pode ser adicionado a algum grupo ativo
    adicionado <- FALSE
    for (gi in seq_along(grupos_ativos)) {
      grp <- grupos_ativos[[gi]]
      # Pode entrar no grupo se não difere significativamente de nenhum membro
      pode <- !any(sig_mat[trt, grp])
      if (pode) {
        grupos_ativos[[gi]] <- c(grp, trt)
        letras[trt] <- paste0(letras[trt], letters[gi])
        adicionado <- TRUE
      }
    }
    if (!adicionado) {
      grupos_ativos <- c(grupos_ativos, list(trt))
      letras[trt] <- paste0(letras[trt], letters[length(grupos_ativos)])
    }
  }
  letras
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

# ==============================================================================
# ESTATÍSTICA DESCRITIVA COMPLETA + NORMALIDADE
# ==============================================================================

calc_descritiva <- function(df, variaveis, grupo_var = NULL) {
  if (length(variaveis) == 0)
    return(list(erro = "Selecione ao menos uma variável."))

  resultados <- list()

  for (var in variaveis) {
    if (!var %in% names(df)) next
    x_all <- df[[var]]
    if (!is.null(grupo_var) && grupo_var != "Nenhum" && grupo_var %in% names(df)) {
      grupos <- split(x_all, df[[grupo_var]])
    } else {
      grupos <- list("Todos" = x_all)
    }

    for (grp_nome in names(grupos)) {
      x <- as.numeric(na.omit(grupos[[grp_nome]]))
      n <- length(x)
      if (n < 3) next

      # Medidas de posição
      media   <- round(mean(x), 4)
      mediana <- round(median(x), 4)
      # Moda (valor mais frequente; NA se todos únicos)
      tab_x <- table(round(x, 3))
      moda  <- if (max(tab_x) > 1) as.numeric(names(which.max(tab_x))) else NA

      # Medidas de dispersão
      dp      <- round(sd(x), 4)
      variancia <- round(var(x), 4)
      cv_pct  <- round(dp / abs(media) * 100, 2)
      minimo  <- round(min(x), 4)
      maximo  <- round(max(x), 4)
      amplitude <- round(maximo - minimo, 4)
      q1      <- round(quantile(x, 0.25), 4)
      q3      <- round(quantile(x, 0.75), 4)
      iqr     <- round(q3 - q1, 4)
      p10     <- round(quantile(x, 0.10), 4)
      p90     <- round(quantile(x, 0.90), 4)

      # Assimetria (Pearson / momento) e Curtose (excesso)
      n_d <- n
      assimetria <- round(
        (n_d / ((n_d - 1) * (n_d - 2))) *
        sum(((x - media) / dp)^3), 4
      )
      curtose <- round(
        ((n_d * (n_d + 1)) / ((n_d - 1) * (n_d - 2) * (n_d - 3))) *
        sum(((x - media) / dp)^4) -
        (3 * (n_d - 1)^2) / ((n_d - 2) * (n_d - 3)), 4
      )

      # Outliers (método Tukey / IQR)
      lim_inf <- q1 - 1.5 * iqr
      lim_sup <- q3 + 1.5 * iqr
      outliers <- x[x < lim_inf | x > lim_sup]
      n_outliers <- length(outliers)

      # Normalidade
      norm <- testar_normalidade(x)

      # Classificação do CV%
      cv_class <- if (cv_pct <= 10) "Baixo (dados homogêneos)"
                  else if (cv_pct <= 20) "Médio"
                  else if (cv_pct <= 30) "Alto"
                  else "Muito alto (dados heterogêneos)"

      resultados[[paste0(var, "___", grp_nome)]] <- list(
        variavel = var, grupo = grp_nome, n = n,
        media = media, mediana = mediana, moda = moda,
        dp = dp, variancia = variancia, cv_pct = cv_pct, cv_class = cv_class,
        minimo = minimo, maximo = maximo, amplitude = amplitude,
        q1 = q1, q3 = q3, iqr = iqr, p10 = p10, p90 = p90,
        assimetria = assimetria, curtose = curtose,
        n_outliers = n_outliers, outliers = outliers,
        lim_inf_outlier = round(lim_inf, 4),
        lim_sup_outlier = round(lim_sup, 4),
        normalidade = norm,
        x_vals = x
      )
    }
  }

  if (length(resultados) == 0)
    return(list(erro = "Nenhum dado válido encontrado para as variáveis selecionadas."))

  list(erro = NULL, resultados = resultados,
       variaveis = variaveis, grupo_var = grupo_var)
}

# Testa normalidade: Shapiro-Wilk (n≤50) ou Lilliefors via KS (n>50)
testar_normalidade <- function(x) {
  n <- length(x)
  if (n < 3) return(list(teste = "ND", estatistica = NA, p = NA,
                          normal = NA, interpretacao = "n < 3"))

  if (n <= 50) {
    sw <- tryCatch(stats::shapiro.test(x), error = function(e) NULL)
    if (is.null(sw)) return(list(teste = "SW", estatistica = NA, p = NA,
                                  normal = NA, interpretacao = "Erro no teste"))
    p   <- sw$p.value
    est <- round(sw$statistic, 4)
    nm  <- "Shapiro-Wilk"
  } else {
    # Lilliefors (correção do KS para média e DP estimados da amostra)
    x_std <- (x - mean(x)) / sd(x)
    ks <- tryCatch(stats::ks.test(x_std, "pnorm"), error = function(e) NULL)
    if (is.null(ks)) return(list(teste = "KS", estatistica = NA, p = NA,
                                  normal = NA, interpretacao = "Erro no teste"))
    p   <- ks$p.value
    est <- round(ks$statistic, 4)
    nm  <- "Kolmogorov-Smirnov"
  }

  normal <- p >= 0.05

  recomendacao <- if (normal) {
    paste0("Distribuição normal (p = ", round(p, 4), "). ",
           "ANOVA paramétrica é adequada.")
  } else if (n <= 10) {
    paste0("Distribuição não normal (p = ", round(p, 4), "). ",
           "n pequeno — use Kruskal-Wallis ou transforme os dados.")
  } else {
    cv_x <- sd(x) / abs(mean(x)) * 100
    transf <- if (all(x > 0)) {
      if (cv_x > 30) "log(x) ou √x (dados com alta variabilidade)"
      else "√x (assimetria moderada)"
    } else "Sem transformação simples (há zeros/negativos) — use Kruskal-Wallis"
    paste0("Distribuição não normal (p = ", round(p, 4), "). ",
           "Sugestão de transformação: ", transf,
           ". Alternativa: Kruskal-Wallis.")
  }

  list(teste = nm, estatistica = est, p = round(p, 4),
       normal = normal, interpretacao = recomendacao)
}

# ==============================================================================
# KRUSKAL-WALLIS + PÓS-HOC DUNN
# ==============================================================================

calc_kruskal <- function(df, atributo, grupo) {
  if (!atributo %in% names(df))
    return(list(erro = paste0("Variável '", atributo, "' não encontrada.")))
  if (!grupo %in% names(df))
    return(list(erro = paste0("Variável de grupo '", grupo, "' não encontrada.")))

  d <- df[!is.na(df[[atributo]]) & !is.na(df[[grupo]]), ]
  d[[grupo]] <- as.factor(d[[grupo]])
  n_grupos <- length(levels(d[[grupo]]))

  if (n_grupos < 2)
    return(list(erro = "Selecione um grupo com ao menos 2 níveis."))
  if (nrow(d) < 6)
    return(list(erro = "Dados insuficientes (mínimo 6 observações)."))

  # Kruskal-Wallis
  kw <- tryCatch(
    stats::kruskal.test(d[[atributo]] ~ d[[grupo]]),
    error = function(e) list(erro = conditionMessage(e))
  )
  if (!is.null(kw$erro)) return(list(erro = kw$erro))

  # Pós-hoc Dunn com correção de Bonferroni (implementado sem pacotes extras)
  dunn <- dunn_test_manual(d[[atributo]], d[[grupo]])

  # Medianas por grupo
  medianas <- tapply(d[[atributo]], d[[grupo]], median, na.rm = TRUE)
  ns       <- tapply(d[[atributo]], d[[grupo]], function(x) sum(!is.na(x)))

  list(
    erro       = NULL,
    n          = nrow(d),
    n_grupos   = n_grupos,
    H          = round(kw$statistic, 4),
    df         = kw$parameter,
    p          = round(kw$p.value, 5),
    significativo = kw$p.value < 0.05,
    medianas   = round(medianas, 3),
    ns         = ns,
    dunn       = dunn,
    atributo   = atributo,
    grupo      = grupo
  )
}

# Teste de Dunn manual (sem dependência de pacote)
dunn_test_manual <- function(y, g) {
  g   <- as.factor(g)
  lvl <- levels(g)
  k   <- length(lvl)
  n   <- length(y)
  rks <- rank(y)   # ranks globais (empates pela média)

  pares <- combn(lvl, 2, simplify = FALSE)
  resultados <- lapply(pares, function(par) {
    i1 <- g == par[1]; i2 <- g == par[2]
    n1 <- sum(i1);     n2 <- sum(i2)
    R1 <- mean(rks[i1]); R2 <- mean(rks[i2])
    # Estatística z de Dunn
    # Fator de correção para empates
    tab <- table(rks)
    corr <- sum(tab^3 - tab) / (12 * (n - 1))
    se_dunn <- sqrt((n * (n + 1) / 12 - corr) * (1/n1 + 1/n2))
    z  <- (R1 - R2) / se_dunn
    p_raw <- 2 * stats::pnorm(-abs(z))
    data.frame(
      Grupo_1 = par[1], Grupo_2 = par[2],
      Z = round(z, 3), p_bruto = round(p_raw, 5),
      stringsAsFactors = FALSE
    )
  })

  df_dunn <- do.call(rbind, resultados)
  # Correção de Bonferroni
  m <- nrow(df_dunn)
  df_dunn$p_bonferroni <- round(pmin(df_dunn$p_bruto * m, 1), 5)
  df_dunn$significativo <- df_dunn$p_bonferroni < 0.05
  df_dunn
}

# ==============================================================================
# MANN-WHITNEY (2 grupos independentes)
# ==============================================================================

calc_mann_whitney <- function(df, atributo, grupo) {
  if (!atributo %in% names(df))
    return(list(erro = paste0("Variável '", atributo, "' não encontrada.")))
  if (!grupo %in% names(df))
    return(list(erro = paste0("Grupo '", grupo, "' não encontrado.")))

  d    <- df[!is.na(df[[atributo]]) & !is.na(df[[grupo]]), ]
  lvls <- unique(as.character(d[[grupo]]))

  if (length(lvls) != 2)
    return(list(erro = paste0(
      "Mann-Whitney exige exatamente 2 grupos. Encontrados: ", length(lvls),
      " (", paste(lvls, collapse=", "), "). Use Kruskal-Wallis para 3+ grupos."
    )))

  x1 <- d[[atributo]][d[[grupo]] == lvls[1]]
  x2 <- d[[atributo]][d[[grupo]] == lvls[2]]

  wt <- tryCatch(
    stats::wilcox.test(x1, x2, exact = FALSE, conf.int = TRUE),
    error = function(e) NULL
  )
  if (is.null(wt)) return(list(erro = "Erro ao executar Mann-Whitney."))

  list(
    erro = NULL,
    n1 = length(x1), n2 = length(x2),
    grupo1 = lvls[1], grupo2 = lvls[2],
    mediana1 = round(median(x1), 3), mediana2 = round(median(x2), 3),
    W = round(wt$statistic, 2),
    p = round(wt$p.value, 5),
    significativo = wt$p.value < 0.05,
    estimativa = round(wt$estimate, 4),  # diferença de localização (Hodges-Lehmann)
    atributo = atributo, grupo = grupo
  )
}

# ==============================================================================
# INTERPRETAÇÕES DAS ANÁLISES NÃO-PARAMÉTRICAS
# ==============================================================================

interpretar_kruskal <- function(res, nome_atrib, nome_grupo) {
  sig <- if (res$significativo) {
    paste0(
      "Diferença <b>estatisticamente significativa</b> entre os grupos de <b>",
      nome_grupo, "</b> para <b>", nome_atrib, "</b> ",
      "(H = ", res$H, ", gl = ", res$df, ", p = ", res$p, "). ",
      "O teste pós-hoc de Dunn (Bonferroni) indica quais pares diferem."
    )
  } else {
    paste0(
      "<b>Sem diferença significativa</b> entre os grupos de <b>", nome_grupo,
      "</b> para <b>", nome_atrib, "</b> ",
      "(H = ", res$H, ", gl = ", res$df, ", p = ", res$p, ")."
    )
  }

  n_sig <- if (!is.null(res$dunn))
    sum(res$dunn$significativo, na.rm = TRUE) else 0

  dunn_txt <- if (res$significativo && n_sig > 0) {
    paste0(" ", n_sig, " par(es) com diferença significativa após correção de Bonferroni.")
  } else if (res$significativo) {
    " Nenhum par isolado significativo após correção de Bonferroni."
  } else ""

  paste0(sig, dunn_txt)
}

interpretar_mann_whitney <- function(res, nome_atrib) {
  if (res$significativo) {
    paste0(
      "Diferença <b>significativa</b> entre <b>", res$grupo1, "</b> (mediana = ",
      res$mediana1, ") e <b>", res$grupo2, "</b> (mediana = ", res$mediana2,
      ") para <b>", nome_atrib, "</b> (W = ", res$W, ", p = ", res$p, "). ",
      "Estimativa de Hodges-Lehmann para a diferença de localização: ",
      res$estimativa, "."
    )
  } else {
    paste0(
      "<b>Sem diferença significativa</b> entre <b>", res$grupo1,
      "</b> (mediana = ", res$mediana1, ") e <b>", res$grupo2,
      "</b> (mediana = ", res$mediana2, ") para <b>", nome_atrib,
      "</b> (W = ", res$W, ", p = ", res$p, ")."
    )
  }
}
