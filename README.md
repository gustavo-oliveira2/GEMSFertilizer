# GEMSFertilizer <img src="man/figures/logo.png" align="right" height="120" alt="" />

<!-- badges -->
[![R-CMD-check](https://github.com/gustavo-oliveira2/GEMSFertilizer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gustavo-oliveira2/GEMSFertilizer/actions/workflows/R-CMD-check.yaml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R](https://img.shields.io/badge/R-%3E%3D4.1-276DC3?logo=r)](https://www.r-project.org/)
[![Versão](https://img.shields.io/badge/vers%C3%A3o-0.5.0-2d6a4f)](https://github.com/gustavo-oliveira2/GEMSFertilizer)

> **Sistema interativo de recomendação de adubação mineral, calagem e gessagem para culturas agrícolas — com classificação de nível tecnológico, análises estatísticas, banco regional de fertilidade e relatório técnico completo.**

Baseado no **Manual de Adubação e Calagem para Minas Gerais — 5ª Aproximação (2023)** e no **Manual de Recomendações de Adubação e Calagem do Estado de Sergipe (EMBRAPA)**.

---

## Instalação

```r
# Instalar o pacote remotes se necessário
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")

# Instalar o GEMSFertilizer direto do GitHub
remotes::install_github("gustavo-oliveira2/GEMSFertilizer")
```

## Uso

```r
# Iniciar o aplicativo
GEMSFertilizer::run_app()
```

---

## Funcionalidades

### 🌱 Interpretação de Solo
- pH, Matéria Orgânica (opcional), P-Mehlich, K, Ca, Mg, Al, H+Al, CTC, V%, m%
- Micronutrientes: S, B, Cu, Fe, Mn, Zn
- **P-rem** como alternativa à textura para interpretação do fósforo
- Argila e M.O. opcionais — a análise não é bloqueada na ausência desses dados
- Cards coloridos por classe agronômica (Muito Baixo → Muito Alto)

### 🧪 Recomendações para 9 Culturas
**Milho · Feijão · Cana-de-açúcar · Arroz · Mandioca · Amendoim · Sorgo · Pastagem · Abacaxi**

| Componente | Métodos disponíveis |
|---|---|
| **Calagem** | V% (5ª Aprox. MG) · Neutralização Al³⁺ + Ca+Mg · Tampão SMP |
| **Gessagem** | Textura (Sousa & Lobato 2004) · V% subsolo · Ca²⁺/CTCef (Caires & Guimarães 2018) |
| **Adubação NPK** | N plantio + N cobertura · P₂O₅ · K₂O · conforme produtividade esperada |

### 🏭 Nível Tecnológico do Sistema de Produção
- Checklist interativo de **21 práticas** em 5 categorias: semente, plantio, fertilidade, manejo fitossanitário e gestão
- Classificação automática em **Baixa / Média / Alta Tecnologia** por pontuação ponderada
- Ajuste automático das doses de NPK conforme o nível:

| Nutriente | Baixa | Média | Alta |
|---|---|---|---|
| N plantio | ×0,70 | ×1,00 | ×1,20 |
| N cobertura | ×0,65 | ×1,00 | ×1,30 |
| P₂O₅ | ×0,75 | ×1,00 | ×1,15 |
| K₂O | ×0,75 | ×1,00 | ×1,10 |

### 📊 Análises Estatísticas (11 análises)

| Categoria | Análises |
|---|---|
| **Descritiva** | Média, mediana, DP, CV%, assimetria, curtose, outliers, normalidade (Shapiro-Wilk / KS) |
| **Exploratória** | Correlação (Pearson/Spearman), regressão simples e múltipla com VIF, análise de trilha |
| **Comparação** | ANOVA com Tukey (≤10 grupos) ou Scott-Knott (>10 grupos), Kruskal-Wallis + Dunn, Mann-Whitney + Hodges-Lehmann |
| **Multivariada** | PCA, Cluster k-means |
| **Limiares** | Cate-Nelson, Linear-Platô |

Todas as análises dispõem de filtro por cultura e gráficos Plotly interativos.

### 📋 Análise em Lote
- Download de **template Excel** padronizado (19 colunas, exemplos, aba Guia)
- Upload de **qualquer planilha** (XLSX, XLS, CSV) — mapeamento automático de 150+ sinônimos de nomes de colunas em português e inglês
- Tabela de revisão editável antes do cálculo
- Exportação das recomendações em Excel e CSV

### 🌍 Banco Regional de Fertilidade
- Upload de histórico de análises por município (até 200 amostras)
- **Mapa coroplético** por município via `geobr` + `leaflet`
- **Semáforo regional**: heatmap município × parâmetro em verde/amarelo/vermelho com metas personalizáveis
- **Cruzamento Clima × Fertilidade**: correlação de Pearson entre chuva/temperatura e atributos do solo por município
- Benchmarking: compara análise atual com distribuição histórica regional (P10 / Mediana / P90)

### 🌦️ Dados Ambientais
- API **Open-Meteo** (gratuita, sem chave): temperatura, precipitação, ET₀ (FAO-56) e balanço hídrico
- Resumo mensal e classificação agronômica do balanço hídrico
- Integração com o banco regional por coordenadas geográficas

### 🌽 Módulo Fenológico do Milho
- Ilustração SVG botânica gerada dinamicamente dos estádios V2, V4, V6 e V8
- Lógica de parcelamento de N: dose única (V4), V4+V6 ou V4+V8 conforme a dose total
- Referência: Ritchie et al. (1993) — *How a corn plant develops*

### 📄 Relatório Técnico HTML/PDF
- Exportação de relatório completo com um clique
- Conteúdo: identificação do produtor, fertilidade com badges coloridos, calagem e gessagem, NPK com justificativa de nível tecnológico, ilustração fenológica
- Impressão via `window.print()` — sem dependências extras

### 💰 Análise Financeira
- 16 fontes comerciais de N, P₂O₅ e K₂O com preços editáveis
- Links para CEPEA/ESALQ, AgroLink e Menor Preço Brasil
- Custo total por hectare e por kg de nutriente

### 🔬 Modo Pesquisa
- Conversão de doses ha⁻¹ para unidades experimentais: metro linear, cova, vaso, parcela
- Gerador de gradiente de tratamentos

---

## Estrutura do Projeto

```
GEMSFertilizer/
├── app.R                        # Ponto de entrada
├── data/
│   ├── tabelas_solo.R           # Interpretação dos parâmetros do solo
│   ├── calculos.R               # CTC, V%, m%, analisar_solo()
│   ├── recomendacoes.R          # Calagem, gessagem, NPK por cultura
│   ├── precos.R                 # Módulo financeiro
│   ├── banco_regional.R         # Banco de fertilidade regional
│   ├── analises_estatisticas.R  # 11 análises estatísticas
│   ├── parser_laudos.R          # Leitura de laudos ITPS e Labominas
│   ├── leitor_lote.R            # Leitura flexível de planilhas em lote
│   ├── dados_ambientais.R       # API Open-Meteo
│   ├── fenologia_milho.R        # SVG fenológico do milho
│   ├── nivel_tecnologico.R      # Classificação do nível tecnológico
│   └── relatorio_html.R         # Gerador de relatório HTML/PDF
├── ui/
│   ├── ui_helpers.R             # CSS, funções auxiliares de UI
│   └── ui_main.R                # Estrutura de abas e painéis
└── server/
    └── server_main.R            # Lógica reativa completa (~4.200 linhas)
```

---

## Dependências Principais

```r
# Interface
shiny, shinyjs, shinyWidgets, shinycssloaders, shinydashboard

# Visualização
plotly, DT, leaflet, ggplot2

# Dados e análise
dplyr, tidyr, readxl, jsonlite, httr, stringr

# Mapas e geoespacial
geobr, sf

# PDF (laudos)
pdftools
```

---

## Referências

- Ribeiro, A.C.; Guimarães, P.T.G.; Alvarez V., V.H. — *Recomendações para uso de corretivos e fertilizantes em Minas Gerais: 5ª Aproximação.* CFSEMG, 1999/2023.
- EMBRAPA / UFS — *Manual de Recomendações de Adubação e Calagem para o Estado de Sergipe.*
- Caires, E.F.; Guimarães, M.F. (2018) — Gessagem por saturação de Ca²⁺ na CTCef.
- Sousa, D.M.G.; Lobato, E. (2004) — *Cerrado: Correção do Solo e Adubação.* EMBRAPA.
- Ritchie, S.W. et al. (1993) — *How a corn plant develops.* Iowa State University.

---

## Licença

GPL (≥ 3) — veja o arquivo [LICENSE](LICENSE).

---

## Contato

**GEMS Research Group — UFS Campus do Sertão**  
Gustavo Hugo Ferreira de Oliveira  
Nossa Senhora da Glória, Sergipe, Brasil  
GitHub: [gustavo-oliveira2](https://github.com/gustavo-oliveira2)
