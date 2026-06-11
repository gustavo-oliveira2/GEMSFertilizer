#' @keywords internal
"_PACKAGE"

# Suprime notas do R CMD CHECK para variáveis globais do Shiny
utils::globalVariables(c(
  "input", "output", "session",
  "todas_fontes", "culturas_lista", "v_alvo_cultura",
  "PRECOS_CSV"
))

# ==============================================================================
#' Inicia o GEMS_Fertilizer
#'
#' Abre o aplicativo Shiny de recomendacao de adubacao e calagem no navegador
#' padrao do sistema.
#'
#' @param host Endereco de host. Padrao: \code{"127.0.0.1"} (local).
#'   Use \code{"0.0.0.0"} para expor na rede local.
#' @param port Porta TCP. Padrao: porta aleatoria disponivel.
#' @param launch.browser Se \code{TRUE} (padrao), abre o navegador
#'   automaticamente.
#' @param display.mode Modo de exibicao do Shiny. Padrao:
#'   \code{"normal"}.
#' @param ... Argumentos adicionais passados para \code{\link[shiny]{runApp}}.
#'
#' @return Nao retorna valor (execucao interativa).
#'
#' @examples
#' \dontrun{
#'   # Iniciar o app normalmente
#'   GEMSFertilizer::run_app()
#'
#'   # Expor na rede local (ex: uso em laboratorio)
#'   GEMSFertilizer::run_app(host = "0.0.0.0", port = 3838)
#' }
#'
#' @export
run_app <- function(host          = getOption("shiny.host", "127.0.0.1"),
                    port          = getOption("shiny.port"),
                    launch.browser = getOption("shiny.launch.browser",
                                               interactive()),
                    display.mode  = "normal",
                    ...) {

  app_dir <- system.file("app", package = "GEMSFertilizer")

  if (app_dir == "") {
    stop(
      "Nao foi possivel encontrar o diretorio do app. ",
      "Tente reinstalar o pacote com:\n",
      "  remotes::install_github(\"seu_usuario/GEMSFertilizer\")",
      call. = FALSE
    )
  }

  # O CSV de precos fica em inst/app/data/ — precisa ser gravavel.
  # Na primeira execucao, copia para um diretorio gravavel do usuario.
  precos_orig <- file.path(app_dir, "data", "precos_referencia.csv")
  precos_user <- file.path(rappdirs_user_data(), "precos_referencia.csv")

  if (!file.exists(precos_user) && file.exists(precos_orig)) {
    dir.create(rappdirs_user_data(), recursive = TRUE, showWarnings = FALSE)
    file.copy(precos_orig, precos_user)
    message("GEMSFertilizer: arquivo de precos copiado para:\n  ", precos_user)
  }

  # Injeta caminho gravavel como variavel de ambiente para o app
  old_env <- Sys.getenv("GEMS_PRECOS_DIR", unset = NA)
  Sys.setenv(GEMS_PRECOS_DIR = rappdirs_user_data())
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("GEMS_PRECOS_DIR")
    else Sys.setenv(GEMS_PRECOS_DIR = old_env)
  }, add = TRUE)

  shiny::runApp(
    appDir        = app_dir,
    host          = host,
    port          = port,
    launch.browser = launch.browser,
    display.mode  = display.mode,
    ...
  )
}

# ==============================================================================
#' Versao do pacote GEMSFertilizer
#'
#' @return String com a versao atual do pacote.
#' @examples
#' GEMSFertilizer::gems_version()
#' @export
gems_version <- function() {
  as.character(utils::packageVersion("GEMSFertilizer"))
}

# ==============================================================================
# Diretorio de dados do usuario (gravavel, persiste entre sessoes)
rappdirs_user_data <- function() {
  # Usa rappdirs se disponivel, senao cai para tools::R_user_dir
  if (requireNamespace("rappdirs", quietly = TRUE)) {
    rappdirs::user_data_dir("GEMSFertilizer")
  } else if (exists("R_user_dir", where = "package:tools")) {
    tools::R_user_dir("GEMSFertilizer", which = "data")
  } else {
    file.path(Sys.getenv("HOME"), ".GEMSFertilizer")
  }
}
