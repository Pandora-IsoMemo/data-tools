#' Download a link to import data
#'
#' @param id module id
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
observeDownloadDataLink <- function(id, input, output, session) {
  exportLink <- reactive({
      allInputs <- reactiveValuesToList(input)

      sourceInputs <- allInputs[names(allInputs)[
        grepl("dataSelector", names(allInputs)) &
          !grepl("previewDat", names(allInputs)) &
          !grepl("repoInfoTable", names(allInputs))
      ]]

      dataLink <- list(
        dataSource = sourceInputs,
        dataPreparation = NULL,
        dataMerging = NULL,
        dataQuery = NULL
      )

      jsonlite::toJSON(dataLink)
  })

  output$downloadDataLink <- downloadHandler(
    filename = function() {
      paste(round(Sys.time()) %>%
              gsub(pattern = ":", replacement = "-") %>%
              gsub(pattern = "\ ", replacement = "_"),
            sprintf("%s.%s",
                    "linkToData",
                    "json"),
            sep = "_")
    },
    content = function(file) {
      jsonlite::write_json(exportLink(), file)
    }
  )
}
