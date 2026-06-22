# GEMSFertilizer 0.5.0

## Nível Tecnológico do Sistema de Produção (novo)
* Checklist interativo de 21 práticas agronômicas agrupadas em 5 categorias
  (semente e material genético, plantio, fertilidade e nutrição, manejo
  fitossanitário, gestão e assistência técnica)
* Classificação automática em Baixa / Média / Alta Tecnologia com pontuação
  ponderada por peso de cada prática
* Fatores de ajuste das doses de NPK aplicados conforme o nível classificado:
  N plantio ×0,70–1,20 | N cobertura ×0,65–1,30 | P₂O₅ ×0,75–1,15 | K₂O ×0,75–1,10
* Interface compacta: resumo fixo (selo circular + barra de progresso) e
  checklist retrátil em acordeão — sem alongar o painel lateral

## Módulo Fenológico do Milho (novo)
* Ilustração SVG botânica gerada dinamicamente com caule gradiente cilíndrico,
  nós, folhas arqueadas com nervura central, raízes fasciculadas e escora, e
  espiga emergindo em V8
* Lógica de parcelamento de N baseada na dose total: dose única em V4 (< 60 kg/ha),
  parcelas V4+V6 (60–119 kg/ha) ou V4+V8 (≥ 120 kg/ha)
* Figura disponível exclusivamente no relatório exportado (não exibida na tela)

## Relatório Completo HTML/PDF (novo)
* Download de relatório técnico contendo: identificação do produtor e propriedade,
  tabela de fertilidade com badges coloridos por classe agronômica, calagem e
  gessagem por método, adubação NPK ajustada pelo nível tecnológico, seção de
  nível tecnológico com barra de progresso e justificativa referenciada nos
  manuais, e ilustração fenológica do milho
* Impressão via `window.print()` com CSS `@media print` — sem dependências extras

## Campos do produtor
* Novos campos no painel esquerdo: nome do produtor, propriedade, município/UF
  e identificação da amostra — usados no cabeçalho do relatório

# GEMSFertilizer 0.4.0

## Módulo de Análise em Lote (novo)
* Template Excel padronizado para download com 19 colunas, exemplos e aba "Guia"
* Leitura flexível de qualquer planilha (XLSX, XLS, CSV) via mapeamento automático
  de 150+ sinônimos de nomes de colunas em português e inglês
* Detecção automática da linha de cabeçalho real (ignora títulos decorativos)
* Tabela de revisão editável antes do cálculo
* Recomendações individuais por amostra com exportação em Excel e CSV

## Parser de laudos PDF (atualizado)
* Normalização ASCII robusta via `norm_ascii()` — resolve problemas de encoding
  UTF-8 em sistemas Linux com locale C/POSIX
* ITPS: detecção correta do K em mg/dm³ (maior das duas ocorrências); MO
  convertida de g/dm³ para dag/kg (÷10)
* Labominas: exclusão do número do relatório (ex: 5732/2025) da lista de IDs
  de amostras; leitura correta das 3 amostras em colunas
* Todos os padrões de busca movidos para dicionário de sinônimos com
  `grepl(fixed = TRUE)` — sem regex com parênteses literais

## Dados Ambientais e Banco Regional
* Cruzamento Clima × Fertilidade: correlação de Pearson entre variável climática
  anual (chuva, temperatura, ET₀ ou balanço hídrico) e atributo de solo por
  município via API Open-Meteo
* Semáforo regional: heatmap município × parâmetro em verde/amarelo/vermelho
  com metas personalizáveis pelo usuário
* Controle de acesso por cadeado 🔒: abas Lote, Pesquisa, Banco Regional e
  Clima ocultas por padrão via `shinyjs::hidden()`

## Correções
* `mk_metric()` e `cls_metric()` movidas para escopo global — resolvido erro
  "função não encontrada" na aba Clima
* `renderPlotly()` separado de `plotlyOutput()` na aba Banco Regional —
  resolvido erro "not a valid CSS unit"
* `colnames` do `datatable()` substituído por `setNames()` na tabela de
  cruzamento clima — resolvido erro "column names not found"

# GEMSFertilizer 0.3.0

## Análises Estatísticas (expandido)
* Estatística Descritiva completa: n, média, mediana, moda, DP, CV%, assimetria,
  curtose, IQR, P10/P90, outliers (Tukey/IQR), normalidade (Shapiro-Wilk ou KS)
* Kruskal-Wallis com pós-hoc Dunn (correção de Bonferroni, implementado sem
  pacotes externos)
* Mann-Whitney com estimativa de Hodges-Lehmann
* ANOVA com seleção automática Tukey (≤10 grupos) ou Scott-Knott (>10 grupos),
  letras de agrupamento e boxplots coloridos por grupo de significância
* Filtro de cultura nas análises estatísticas

## Banco Regional de Fertilidade
* Upload de planilha com até 200 análises históricas (33 colunas)
* Mapa coroplético por município via `geobr` + `leaflet`
* Benchmarking: compara análise atual com distribuição histórica regional
* Série temporal por município e estatísticas regionais (P10/P90)

## Dados Ambientais
* API Open-Meteo integrada: temperatura, precipitação, ET₀ (FAO-56) e balanço
  hídrico para qualquer município do Brasil, sem chave de acesso
* Resumo mensal com classificação agronômica do balanço hídrico

## P-rem e campos opcionais
* Fósforo remanescente (P-rem) como alternativa à argila na interpretação do P
* Matéria orgânica e Argila tornadas opcionais sem bloqueio da análise
* Lógica de prioridade: P-rem > Argila > fallback 30% com aviso

# GEMSFertilizer 0.2.0

## Banco Regional (primeira versão)
* Template de digitalização com 33 colunas
* Leitura por posição de coluna (robusto a re-formatações Excel)
* Mapa coroplético com `geobr` e `leaflet`

## Análises Estatísticas (primeira versão)
* Correlação, regressão simples e múltipla com VIF, análise de trilha,
  PCA, cluster k-means, Cate-Nelson e linear-platô

## Modo Pesquisa expandido
* Gerador de gradiente de tratamentos com doses por ha, metro linear e cova

## Análise em Lote (primeira versão)
* Upload de múltiplos arquivos com revisão e exportação

# GEMSFertilizer 0.1.0

## Primeira versão pública

### Culturas suportadas
* Milho, Feijão, Cana-de-açúcar, Arroz, Mandioca, Amendoim, Sorgo,
  Pastagem e Abacaxi

### Calagem
* Método Saturação por Bases V% (5ª Aproximação MG)
* Método Neutralização Al³⁺ + Ca+Mg (padrão Nordeste/Sergipe)
* Método Tampão SMP
* Comparação simultânea dos três métodos

### Gessagem
* Método Textura — Sousa & Lobato (2004)
* Método V% subsolo — Demattê/Vitti (2008)
* Método Saturação por Ca²⁺ na CTCef — Caires & Guimarães (2018)

### Módulo financeiro
* Tabela editável de 16 fontes comerciais de N, P₂O₅ e K₂O
* Links para CEPEA/ESALQ, AgroLink e Menor Preço Brasil
* Persistência local de preços via CSV

### Modo Pesquisa
* Conversão de doses ha⁻¹ para metro linear, cova, vaso e parcela
* Cálculo para NPK formulado
* Tabela de gradiente de tratamentos

### Gráficos
* Radar de fertilidade (7 parâmetros)
* Macronutrientes com nível ótimo de referência
* Gauges de V% e m% com alvo por cultura
* Micronutrientes em % do nível ótimo
