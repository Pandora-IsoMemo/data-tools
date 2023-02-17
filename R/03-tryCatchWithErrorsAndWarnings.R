#' Try Catch with Warnings and Errors
#'
#' Catches errors and warnings and forwards them as an alert. If an error occurs, NULL is returned.
#' If a warning occurs, the result is returned.
#'
#' @param expr expression to be evaluated.
#' @param messagePreError (character) error message prefix.
#'
#' @export
tryCatchWithWarningsAndErrors <- function(expr, messagePreError = "Modeling failed:") {
  exprWarnings <- NULL
  w.handler <- function(w){ # warning handler
    exprWarnings <<- w
    invokeRestart("muffleWarning")
  }

  res <- withCallingHandlers(tryCatch({
    expr
  },
  error = function(cond) {
    shinyjs::alert(paste(messagePreError, cond$message))
    return(NULL)
  }),
  warning = w.handler)

  if (!is.null(exprWarnings)) {
    shinyjs::alert(paste0(exprWarnings, collapse = "\n"))
  }

  res
}

# TEST MODULE -------------------------------------------------------------

uiTestTryCatch <- fluidPage(
  shinyjs::useShinyjs(),
  actionButton("buttonWarn", "Test warning"),
  actionButton("buttonErr", "Test error"),
  tags$hr(),
  textOutput("resWarn"),
  textOutput("resErr")
)

serverTestTryCatch <- function(input, output, session) {
  resWarn <- reactiveVal()
  resErr <- reactiveVal()

  observe({
    tmpRes <- tryCatchWithWarningsAndErrors({
      warning("test warning")
      5+4
      }, messagePreError = "Modeling failed:")
    resWarn(tmpRes)
  }) %>%
    bindEvent(input$buttonWarn)

  observe({
    tmpRes <- tryCatchWithWarningsAndErrors({
      stop("test error")
      5+4
    }, messagePreError = "Modeling failed:")
    resErr(tmpRes)
  }) %>%
    bindEvent(input$buttonErr)

  output$resWarn <- renderText({
    as.character(resWarn())
  })

  output$resErr <- renderText({
    as.character(resErr())
  })
}

shinyApp(uiTestTryCatch, serverTestTryCatch)