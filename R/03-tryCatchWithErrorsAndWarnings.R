#' Try Catch with Warnings and Errors
#'
#' Catches errors and warnings and forwards them as an alert. If an error occurs, NULL is returned.
#' If a warning occurs, the result is returned.
#'
#' @param silent (logical) if TRUE prevents an alert, instead a warning will be displayed. Use this
#'  when not applied within a Shiny session.
#' @inheritParams shinyTools::shinyTryCatch
#'
#' @export
tryCatchWithWarningsAndErrors <- function(expr,
                                          errorTitle = "Modeling failed",
                                          warningTitle = "",
                                          alertStyle = "shinyjs",
                                          silent = FALSE) {
  deprecate_warn("24.05.1",
                 "DataTools::tryCatchWithWarningsAndErrors()",
                 "shinyTools::shinyTryCatch()")

  shinyTryCatch(expr = expr,
                errorTitle = errorTitle,
                warningTitle = warningTitle,
                alertStyle = alertStyle,
                inShiny = !silent)
}

# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# uiTestTryCatch <- fluidPage(
#   shinyjs::useShinyjs(),
#   actionButton("buttonWarn", "Test shinyjs warning"),
#   actionButton("buttonErr", "Test shinyjs error"),
#   actionButton("buttonShinyalertWarn", "Test shinyalert warning"),
#   actionButton("buttonShinyalertErr", "Test shinyalert error"),
#   actionButton("buttonShinyalertErrinsideRender", "Test shinyalert error inside render"),
#   tags$hr(),
#   textOutput("testRes"),
#   plotOutput("testResInRender")
# )
#
# serverTestTryCatch <- function(input, output, session) {
#   testRes <- reactiveVal()
#
#   observe({
#     tmpRes <- {
#       warning("test warning")
#       5 + 4
#     } %>%
#       tryCatchWithWarningsAndErrors(errorTitle = "Modeling failed")
#     testRes(tmpRes)
#   }) %>%
#     bindEvent(input$buttonWarn)
#
#   observe({
#     tmpRes <- {
#       stop("test error")
#       5 + 4
#     } %>%
#       tryCatchWithWarningsAndErrors(errorTitle = "Modeling failed")
#     testRes(tmpRes)
#   }) %>%
#     bindEvent(input$buttonErr)
#
#   observe({
#     tmpRes <- {
#       warning("test warning")
#       5 + 4
#     } %>%
#       tryCatchWithWarningsAndErrors(warningTitle = "Warning", alertStyle = "shinyalert")
#     testRes(tmpRes)
#   }) %>%
#     bindEvent(input$buttonShinyalertWarn)
#
#   observe({
#     tmpRes <- {
#       warning("test warning")
#       stop("test error")
#       5 + 4
#     } %>%
#       tryCatchWithWarningsAndErrors(errorTitle = "Modeling failed", alertStyle = "shinyalert", silent = TRUE)
#     testRes(tmpRes)
#   }) %>%
#     bindEvent(input$buttonShinyalertErr)
#
#   output$testRes <- renderText({
#     as.character(testRes())
#   })
#
#   output$testResInRender <- renderPlot({
#     req(input$buttonShinyalertErrinsideRender > 0)
#     p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) +
#       ggplot2::geom_boxplot() +
#       ggplot2::labs(title = "Boxplot of MPG by Cylinder",
#                     x = "Number of Cylinders",
#                     y = "Miles per Gallon")
#
#     p <- p + ggplot2::xlim(c(3, 8))
#
#     {
#       warning("Test warning")
#       print(p)
#       } %>%
#       tryCatchWithWarningsAndErrors(errorTitle = "Plotting failed: ",
#                                     warningTitle = "Warning in plotting: ",
#                                     alertStyle = "shinyalert")
#   })
# }
#
# shinyApp(uiTestTryCatch, serverTestTryCatch)
