# Try Catch with Warnings and Errors

Catches errors and warnings and forwards them as an alert. If an error
occurs, NULL is returned. If a warning occurs, the result is returned.

## Usage

``` r
tryCatchWithWarningsAndErrors(
  expr,
  errorTitle = "Modeling failed",
  warningTitle = "",
  alertStyle = "shinyjs",
  silent = FALSE
)
```

## Arguments

- expr:

  expression to be evaluated.

- errorTitle:

  (character) error message title.

- warningTitle:

  (character) warning message title.

- alertStyle:

  (character) Either "shinyjs", or "shinyalert". Specifies how an error
  or a warning is given out. If "shinyjs" than shinyjs::alert is used;
  if "shinyalert" than shinyalert::shinyalert is used.

- silent:

  (logical) if TRUE prevents an alert, instead a warning will be
  displayed. Use this when not applied within a Shiny session.
