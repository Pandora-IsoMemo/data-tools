#' Try Catch with Warnings and Errors
#'
#' Catches errors and warnings and forwards them as an alert. If an error occurs, NULL is returned.
#' If a warning occurs, the result is returned.
#'
#' @param expr expression to be evaluated.
#' @param errorTitle (character) error message title.
#' @param warningTitle (character) warning message title.
#' @param alertStyle (character) Either "shinyjs", or "shinyalert". Specifies how an error or a
#'  warning is given out. If "shinyjs" than shinyjs::alert is used; if "shinyalert" than
#'  shinyalert::shinyalert is used.
#'
#' @export
tryCatchWithWarningsAndErrors <- function(expr,
                                          errorTitle = "Modeling failed",
                                          warningTitle = "",
                                          alertStyle = "shinyjs") {
  tryCatchMessage <- NULL

  w.handler <- function(w) {
    # warning handler
    tryCatchMessage[["text"]] <<- w
    tryCatchMessage[["text"]] <<-
      paste0(tryCatchMessage[["text"]], collapse = "\n")
    tryCatchMessage[["title"]] <<- warningTitle
    tryCatchMessage[["type"]] <<- "warning"
    invokeRestart("muffleWarning")
  }

  e.handler <- function(e) {
    # error handler
    tryCatchMessage[["text"]] <<- e$message
    tryCatchMessage[["title"]] <<- errorTitle
    tryCatchMessage[["type"]] <<- "error"
    return(NULL)
  }

  res <- withCallingHandlers(tryCatch({
    expr
  },
  error = e.handler),
  warning = w.handler)

  # give out error or warning
  switch (
    alertStyle,
    "shinyjs" = shinyjs::alert(paste(
      tryCatchMessage[["title"]],
      tryCatchMessage[["text"]],
      sep = "\n "
    )),
    "shinyalert" = shinyalert::shinyalert(
      title = tryCatchMessage[["title"]],
      text = tryCatchMessage[["text"]],
      type = tryCatchMessage[["type"]]
    )
  )

  # output result of expr
  res
}

# TEST MODULE -------------------------------------------------------------

uiTestTryCatch <- fluidPage(
  shinyjs::useShinyjs(),
  actionButton("buttonWarn", "Test shinyjs warning"),
  actionButton("buttonErr", "Test shinyjs error"),
  actionButton("buttonShinyalertWarn", "Test shinyalert warning"),
  actionButton("buttonShinyalertErr", "Test shinyalert error"),
  tags$hr(),
  textOutput("testRes")
)

serverTestTryCatch <- function(input, output, session) {
  testRes <- reactiveVal()

  observe({
    tmpRes <- tryCatchWithWarningsAndErrors({
      warning("test warning")
      5 + 4
    }, errorTitle = "Modeling failed")
    testRes(tmpRes)
  }) %>%
    bindEvent(input$buttonWarn)

  observe({
    tmpRes <- tryCatchWithWarningsAndErrors({
      stop("test error")
      5 + 4
    }, errorTitle = "Modeling failed")
    testRes(tmpRes)
  }) %>%
    bindEvent(input$buttonErr)

  observe({
    tmpRes <- tryCatchWithWarningsAndErrors({
      warning("test warning")
      5 + 4
    }, warningTitle = "Warning", alertStyle = "shinyalert")
    testRes(tmpRes)
  }) %>%
    bindEvent(input$buttonShinyalertWarn)

  observe({
    tmpRes <- tryCatchWithWarningsAndErrors({
      stop("test error")
      5 + 4
    }, errorTitle = "Modeling failed", alertStyle = "shinyalert")
    testRes(tmpRes)
  }) %>%
    bindEvent(input$buttonShinyalertErr)

  output$testRes <- renderText({
    as.character(testRes())
  })
}

shinyApp(uiTestTryCatch, serverTestTryCatch)
