# ==============================================================================
# GEMS_Fertilizer — inst/app/app.R
# Ponto de entrada quando chamado via GEMSFertilizer::run_app()
# ==============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(httr)
library(jsonlite)
library(stringr)
library(shinycssloaders)
library(rvest)

# ------------------------------------------------------------------------------
# CAMINHO DO CSV DE PREÇOS
# Quando rodando como pacote usa diretório gravável do usuário (GEMS_PRECOS_DIR).
# Em desenvolvimento local (runApp direto), usa data/ relativo.
# ------------------------------------------------------------------------------
gems_precos_dir <- Sys.getenv("GEMS_PRECOS_DIR", unset = "")
if (nchar(gems_precos_dir) > 0 && dir.exists(gems_precos_dir)) {
  PRECOS_CSV <<- file.path(gems_precos_dir, "precos_referencia.csv")
} else {
  PRECOS_CSV <<- "data/precos_referencia.csv"
}

# ------------------------------------------------------------------------------
# CARREGAR MÓDULOS
# ------------------------------------------------------------------------------
source("data/tabelas_solo.R")
source("data/recomendacoes.R")
source("data/calculos.R")
source("data/precos.R")

# Funções auxiliares de UI devem ser carregadas ANTES do objeto ui ser criado
source("ui/ui_helpers.R")
source("ui/ui_main.R")
source("server/server_main.R")

shinyApp(ui = ui, server = server)
