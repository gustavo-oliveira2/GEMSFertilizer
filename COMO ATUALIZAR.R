#Passo 1 — Atualizar a versão no DESCRIPTION
#Abra o arquivo DESCRIPTION e mude a linha Version::
#  Version: 0.2.0
#Também atualize o NEWS.md com o que mudou nessa versão (opcional, mas bom para histórico).

#Passo 2 — Commit e Push pelo Terminal do RStudio
#Na aba Terminal (não o Console R):
#  bash# Entra na pasta do pacote (ajuste o caminho se necessário)
#cd C:/Users/User/Documents/GEMSFertilizer/GEMSFertilizer

# Adiciona todos os arquivos modificados
#git add .

# Cria o commit com mensagem descritiva
#git commit -m "feat: análises estatísticas, banco regional, modo pesquisa, botões de preço v0.2.0"

# Envia para o GitHub
#git push

#Passo 3 — Criar um Release no GitHub (recomendado)
#No navegador, acesse seu repositório no GitHub:

#  Clique em Releases → Create a new release
#Em Tag version: v0.2.0
#Em Release title: GEMSFertilizer v0.2.0
#Cole no corpo as principais novidades (pode copiar do NEWS.md)
#Clique em Publish release


#Passo 4 — Qualquer usuário atualiza com uma linha
rremotes::install_github("SEU_USUARIO/GEMSFertilizer")
GEMSFertilizer::run_app()

#Se der erro de autenticação no git push
#O token pode ter expirado. Renove:
  r# No Console R
usethis::create_github_token()   # abre navegador, gere novo token
gitcreds::gitcreds_set()         # cole o novo token quando pedir
#Depois tente o git push novamente no Terminal.

