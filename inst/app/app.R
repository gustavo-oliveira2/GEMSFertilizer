# ==============================================================================
# ADUBO CERTO - Sistema de Recomendação de Adubação e Calagem
# Baseado no Manual de Minas Gerais (5ª Aproximação) e Manual de Sergipe
# ==============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
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
library(leaflet)
library(readxl)        # scraping CEPEA/AgroLink

# ==============================================================================
# DADOS E TABELAS TÉCNICAS
# ==============================================================================
source("data/tabelas_solo.R")
source("data/recomendacoes.R")
source("data/calculos.R")
source("data/precos.R")
source("data/banco_regional.R")
source("data/analises_estatisticas.R")
source("data/parser_laudos.R")
source("data/leitor_lote.R")
source("data/dados_ambientais.R")
source("data/fenologia_milho.R")
source("data/relatorio_html.R")
source("data/nivel_tecnologico.R")

# Funções auxiliares de UI devem ser carregadas ANTES do objeto ui ser criado
source("ui/ui_helpers.R")
source("ui/ui_main.R")
source("server/server_main.R")

shinyApp(ui = ui, server = server)
