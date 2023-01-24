#' Start Application
#'
#' @param port port
#' @param host host
#' @param launch.browser If true, the system's default web browser will be launched
#'
#' @export
startApplication <- function(port = getOption("shiny.port"),
                             host = getOption("shiny.host", "127.0.0.1"),
                             launch.browser = getOption("shiny.launch.browser", interactive())) {
  shiny::runApp(
    system.file("app", package = "IsoAppTools"),
    port = port,
    host = host,
    launch.browser = launch.browser
  )
}
