config <- function() {
  config_path <- system.file("config.yaml", package = "DataTools")
  yaml.load_file(config_path)
}

#' globals
#' @description
#' Contains the package's global variables
pkg.env = new.env()
pkg.env$api_key = NULL

#' Contains the package's base URLs
#'
#' @description
#' These are the base URLs for the `rgpt3` package. Do not change these!
url.completions = "https://api.openai.com/v1/completions"
