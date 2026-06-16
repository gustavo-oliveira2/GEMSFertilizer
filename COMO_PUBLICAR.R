# Como publicar o GEMSFertilizer no GitHub
# Execute este script no RStudio após abrir GEMSFertilizer.Rproj

# ==============================================================================
# PASSO 1 — Instalar ferramentas necessárias (só precisa fazer uma vez)
# ==============================================================================
install.packages(c("devtools", "usethis", "roxygen2", "pkgdown"))

# ==============================================================================
# PASSO 2 — Verificar o pacote localmente
# ==============================================================================
# Abre o projeto GEMSFertilizer.Rproj no RStudio, depois:

devtools::document()      # Gera/atualiza NAMESPACE e man/*.Rd
devtools::check()         # R CMD CHECK — corrija quaisquer erros/warnings
devtools::install()       # Instala localmente para testar

# Teste rápido:
GEMSFertilizer::run_app()

# ==============================================================================
# PASSO 3 — Criar repositório no GitHub
# ==============================================================================
# Opção A — Via RStudio + usethis (recomendado):

usethis::use_git()                              # Inicia git local
usethis::create_github_token()                  # Abre navegador para criar token
gitcreds::gitcreds_set()                        # Cola o token quando solicitado
usethis::use_github(private = FALSE)            # Cria o repo e faz o push

# Opção B — Via linha de comando (se preferir):
# 1. Crie o repositório em https://github.com/new (nome: GEMSFertilizer)
# 2. No terminal dentro da pasta do pacote:
#    git init
#    git add .
#    git commit -m "feat: versão inicial do pacote GEMSFertilizer v0.1.0"
#    git branch -M main
#    git remote add origin https://github.com/SEU_USUARIO/GEMSFertilizer.git
#    git push -u origin main

# ==============================================================================
# PASSO 4 — Atualizar URLs no DESCRIPTION e README
# ==============================================================================
# Substitua "seu_usuario" pelo seu usuário do GitHub em:
#   - DESCRIPTION  → URL: e BugReports:
#   - README.md    → badge e links de instalação

# ==============================================================================
# PASSO 5 — Criar um Release no GitHub (recomendado)
# ==============================================================================
# No GitHub:
#   Releases → Create a new release
#   Tag: v0.1.0
#   Title: GEMSFertilizer v0.1.0 — Versão inicial
#   Description: cole o conteúdo do NEWS.md

# ==============================================================================
# PASSO 6 — Qualquer usuário R instala com:
# ==============================================================================
# install.packages("remotes")
# remotes::install_github("SEU_USUARIO/GEMSFertilizer")
# GEMSFertilizer::run_app()

# ==============================================================================
# PASSO OPCIONAL — Site de documentação com pkgdown
# ==============================================================================
usethis::use_pkgdown_github_pages()  # Configura GitHub Pages automaticamente
pkgdown::build_site()                # Gera site local para prévia
# Após o push, o site fica em: https://SEU_USUARIO.github.io/GEMSFertilizer/

# ==============================================================================
# ATUALIZAÇÕES FUTURAS
# ==============================================================================
# 1. Edite os arquivos em inst/app/ normalmente
# 2. Atualize a versão em DESCRIPTION (ex: 0.1.0 → 0.2.0)
# 3. Documente as mudanças em NEWS.md
# 4. devtools::check()  →  devtools::install()  →  teste
# 5. git add . && git commit -m "..." && git push
# 6. Crie novo Release no GitHub
