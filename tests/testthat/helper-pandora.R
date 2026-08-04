pandora_api_available <- local({
  checked <- FALSE
  available <- FALSE

  function(timeout = 10) {
    if (checked) return(available)

    old_timeout <- getOption("timeout")
    on.exit(options(timeout = old_timeout), add = TRUE)
    options(timeout = timeout)

    available <<- tryCatch({
      res <- Pandora::callAPI(action = "group_list", all_fields = "true")
      !is.null(res) && length(res) > 0
    }, error = function(e) {
      FALSE
    }, warning = function(w) {
      FALSE
    })

    checked <<- TRUE
    available
  }
})

skip_if_no_pandora_api <- function() {
  testthat::skip_if_not(
    pandora_api_available(),
    "Pandora API is not accessible; skipping Pandora-dependent tests."
  )
}
