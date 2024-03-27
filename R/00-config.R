config <- function() {
  config_path <- system.file("config.yaml", package = "DataTools")
  yaml.load_file(config_path)
}

#' globals
#' @description
#' Contains the package's global variables
#' @field api_key The OpenAI API key
#' @field url.completions The URL for the completions endpoint
pkg.env <- new.env()
pkg.env$api_key <- NULL
pkg.env$url.completions = "https://api.openai.com/v1/completions"
