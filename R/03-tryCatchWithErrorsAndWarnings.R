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
#' @param silent (logical) if TRUE prevents an alert, instead a warning will be displayed. Use this
#'  when not applied within a Shiny session.
#'
#' @export
tryCatchWithWarningsAndErrors <- function(expr,
                                          errorTitle = "Modeling failed",
                                          warningTitle = "",
                                          alertStyle = "shinyjs",
                                          silent = FALSE) {
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

  if (!is.null(tryCatchMessage) && !silent) {
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
  }

  if (!is.null(tryCatchMessage) && silent) {
    # give out error or warning
    warning(paste0(tryCatchMessage[["title"]], tryCatchMessage[["text"]]), call. = FALSE)
  }

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
    tmpRes <- {
      warning("test warning")
      5 + 4
    } %>%
      tryCatchWithWarningsAndErrors(errorTitle = "Modeling failed")
    testRes(tmpRes)
  }) %>%
    bindEvent(input$buttonWarn)

  observe({
    tmpRes <- {
      stop("test error")
      5 + 4
    } %>%
      tryCatchWithWarningsAndErrors(errorTitle = "Modeling failed")
    testRes(tmpRes)
  }) %>%
    bindEvent(input$buttonErr)

  observe({
    tmpRes <- {
      warning("test warning")
      5 + 4
    } %>%
      tryCatchWithWarningsAndErrors(warningTitle = "Warning", alertStyle = "shinyalert")
    testRes(tmpRes)
  }) %>%
    bindEvent(input$buttonShinyalertWarn)

  observe({
    tmpRes <- {
      stop("test error")
      5 + 4
    } %>%
      tryCatchWithWarningsAndErrors(errorTitle = "Modeling failed", alertStyle = "shinyalert")
    testRes(tmpRes)
  }) %>%
    bindEvent(input$buttonShinyalertErr)

  output$testRes <- renderText({
    as.character(testRes())
  })
}

shinyApp(uiTestTryCatch, serverTestTryCatch)
