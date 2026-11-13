config <- function() {
  config_path <- system.file("config.yaml", package = "DataTools")
  yaml.load_file(config_path)
}
