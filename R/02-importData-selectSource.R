#' Select Source UI
#'
#' UI of the module
#'
#' @param isInternet (logical) set TRUE, if there is an internet connection. This parameter is
#'  ignored if \code{type = "file"} or \code{type = "remoteModel"}
#' @param fileInputAccept (character) (optional) accept attribute for fileInput. E.g. ".zip"
#'  or ".csv"
#' @rdname selectSourceServer
selectSourceUI <- function(id,
                           defaultSource,
                           ckanFileTypes,
                           importType,
                           isInternet,
                           fileInputAccept = "zip") {
  ns <- NS(id)

  sourceChoices <- switch(importType,
                          data = c("Pandora Platform" = "ckan",
                                   "File" = "file",
                                   "URL" = "url",
                                   "Github Repository" = "remoteModel"),
                          model = c("Pandora Platform" = "ckan",
                                    "File" = "file",
                                    "URL" = "url",
                                    "Github Repository" = "remoteModel"),
                          zip = c("Pandora Platform" = "ckan",
                                  "File" = "file",
                                  "URL" = "url",
                                  "Github Repository" = "remoteModel"),
                          list = c("Pandora Platform" = "ckan",
                                   "File" = "file",
                                   "URL" = "url")
  )

  tagList(
    # source selection ----
    fluidRow(
      column(6,
             tags$strong("1. Select source"),
             radioButtons(
               ns("source"),
               label = NULL,
               choices = sourceChoices,
               selected = defaultSource,
               inline = TRUE)),
      if (importType == "data") {
        column(6,
               align = "right",
               style = "margin-top: 1.7em;",
               conditionalPanel(
                 ns = ns,
                 condition = "input.source == 'file'",
                 radioButtons(ns("dataOrLink"),
                              label = NULL,
                              choices = c("Load Dataset" = "fullData", "Load Data Query" = "dataLink"),
                              selected = "fullData",
                              inline = TRUE),
                 if (!isInternet) helpText("No internet connection!") else NULL
               )
        )
      } else NULL
    ),
    tags$br(),
    # source == ckan/file/url/model ----
    ## source == ckan
    conditionalPanel(
      condition = "input.source == 'ckan'",
      ns = ns,
      fluidRow(column(width = 6,
                      filterCKANRepoUI(ns("repoFilter")),
                      filterCKANResourceUI(ns("resourceFilter"), ckanFileTypes = ckanFileTypes),
                      loadCKANResourceUI(ns("resourceLoad"))
      ),
      column(width = 6,
             tags$strong("Additional information about the repository from ",
                         tags$a("Pandora", href = "https://pandora.earth/", target = "_blank")),
             dataTableOutput(ns(
               "repoInfoTable"
             ))
      ))
    ),
    ## source == file
    conditionalPanel(
      condition = "input.source == 'file' && input.dataOrLink == 'fullData'",
      ns = ns,
      fileInput(ns("file"),
                label = NULL,
                accept = fileInputAccept,
                width = "100%")
    ),
    conditionalPanel(
      condition = "input.source == 'file' && input.dataOrLink == 'dataLink'",
      ns = ns,
      fileInput(ns("file"),
                label = NULL,
                accept = ".json",
                width = "100%")
    ),
    ## source == url
    conditionalPanel(
      condition = "input.source == 'url'",
      ns = ns,
      fluidRow(column(width = 10,
                      textInput(ns("url"), label = NULL, placeholder = "URL", width = "100%")),
               column(width = 2,
                      actionButton(ns("loadUrl"), "Load")))
    ),
    ## source == model
    conditionalPanel(
      condition = "input.source == 'remoteModel'",
      ns = ns,
      remoteModelsUI(ns("remoteModels"), selectLabel = if (importType == "data") "Load data query" else NULL)
    ),
    tags$hr()
  )
}


#' Select Source Server
#'
#' Server function of the module
#' @param id id of module
#' @param openPopupReset (reactive) TRUE if popup is (re-)opened
#' @param internetCon (reactive) TRUE if there is an internet connection
#' @inheritParams configureFileServer
#' @inheritParams remoteModelsServer
#' @inheritParams importDataServer
selectSourceServer <- function(id,
                               importType = "data",
                               openPopupReset,
                               internetCon,
                               githubRepo,
                               folderOnGithub,
                               pathToLocal,
                               ckanFileTypes = c("xls", "xlsx", "csv", "odt", "txt")) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 logDebug(initServerLogTxt(ns("")))

                 ckanNetworks <- reactive({
                   req(isTRUE(openPopupReset())) # trigger update always when the popup opens

                   if (isTRUE(internetCon())) {
                   logDebug("%s: Calling ckan API with 'group_list' and updating 'ckanNetworks()'...", id)
                   res <- local({
                     old_timeout <- getOption("timeout")  # Save the current timeout
                     options(timeout = 10)  # Set timeout to 10 seconds

                     on.exit(options(timeout = old_timeout))  # Restore old timeout when function ends
                     callAPI(action = "group_list", all_fields = "true") %>%
                       withProgress(message = "Connecting to Pandora...") %>%
                       shinyTryCatch(errorTitle = "Pandora connection error", alertStyle = "shinyalert")
                   })
                   } else {
                     res <- NULL
                   }

                   if (length(res) > 0) {
                     return(res)
                   } else {
                     return(NULL)
                   }
                 })

                 ckanPackages <- reactive({
                   req(isTRUE(openPopupReset())) # trigger update always when the popup opens

                   if (length(ckanNetworks()) == 0) {
                     logDebug("%s: Skipping calling ckan API and updating 'ckanPackages()' because of connection issues...", id)
                     # assume no connection
                     return(NULL)
                   }

                   logDebug("%s: Calling ckan API with 'current_package_list_with_resources' and updating 'ckanPackages()'...", id)
                   res <- local({
                     old_timeout <- getOption("timeout")  # Save the current timeout
                     options(timeout = 10)  # Set timeout to 10 seconds

                     on.exit(options(timeout = old_timeout))  # Restore old timeout when function ends
                     callAPI(action = "current_package_list_with_resources",
                             limit = 1000) %>%
                       withProgress(message = "Loading Pandora packages...") %>%
                       shinyTryCatch(errorTitle = "Pandora connection error", alertStyle = "shinyalert")
                   })

                   if (length(res) > 0) {
                     return(res)
                   } else {
                     return(NULL)
                   }
                 })


                 dataSource <- reactiveValues(file = NULL,
                                              filename = NULL,
                                              type = NULL,
                                              input = NULL)

                 observe({
                   req(isTRUE(openPopupReset()))
                   logDebug("Update after openPopupReset()")
                   # reset
                   reset("file")
                   updateTextInput(session, "repoFilter-ckanMeta", value = "")

                   if (isTRUE(internetCon())) {
                     if (importType == "data") shinyjs::enable(ns("dataOrLink"), asis = TRUE)
                     # update/reset ckanGroup, ckanRecord, ckanResourceTypes
                     updatePickerInput(session,
                                       "repoFilter-ckanGroup",
                                       choices = getCKANGroupChoices(groupList = ckanNetworks()),
                                       selected = character(0))
                     choicesRepo <- getCKANRecordChoices(packageList = ckanPackages())
                     updateSelectizeInput(session,
                                          "resourceFilter-ckanRecord",
                                          choices = choicesRepo,
                                          selected = choicesRepo[1])
                     choicesTypes <- getCKANTypesChoices(packageList = ckanPackages(),
                                                         ckanFileTypes = ckanFileTypes)
                     updatePickerInput(session,
                                       "resourceFilter-ckanResourceTypes",
                                       choices = choicesTypes,
                                       selected = choicesTypes)
                   } else {
                     warning("selectSourceServer: No internet connection!")
                     #shinyjs::disable(ns("dataOrLink"), asis = TRUE)
                     updateRadioButtons(session, "source", selected = "file")
                     updateTextInput(session, "url", placeholder = "No internet connection ...")
                     shinyjs::disable(ns("loadUrl"), asis = TRUE)
                   }
                 }) %>%
                   bindEvent(openPopupReset())

                 output$repoInfoTable <- renderDataTable({
                   validate(need(ckanPackages(), "No connection!"))
                   validate(need(input[["resourceFilter-ckanRecord"]],
                                 "Please select a repository!"))

                   aboutTable <- ckanPackages()[ckanPackages()[["name"]] == input[["resourceFilter-ckanRecord"]], , drop = FALSE] %>%
                     formatRepositoryList(renameColumns = TRUE)
                   DT::datatable(
                     data.frame(
                       Field = colnames(aboutTable),
                       Value = as.character(aboutTable[1, ])
                     ),
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     options = list(
                       dom = "t",
                       searching = FALSE,
                       scrollX = TRUE,
                       scrollY = "24rem"
                     )
                   )
                 })

                 observe({
                   req(isTRUE(internetCon()), nrow(ckanPackages()) == 0)
                   logDebug("Updating Pandora package list")
                   updatePickerInput(session,
                                     "repoFilter-ckanGroup",
                                     choices = getCKANGroupChoices(groupList = ckanNetworks()))
                   updateSelectizeInput(session,
                                        "resourceFilter-ckanRecord",
                                        choices = getCKANRecordChoices(packageList = ckanPackages()))
                   fileTypes <- getCKANTypesChoices(packageList = ckanPackages(),
                                                    ckanFileTypes = ckanFileTypes)
                   updatePickerInput(session,
                                     "resourceFilter-ckanResourceTypes",
                                     choices = fileTypes,
                                     selected = fileTypes)
                 }) %>%
                   bindEvent(internetCon())

                 observe({
                   logDebug("Updating input$source and reset")

                   # disable load data link
                   if (input$source == "file") {
                     shinyjs::show(ns("dataOrLink"), asis = TRUE)
                   } else {
                     updateRadioButtons(session, "dataOrLink", selected = "fullData")
                     #shinyjs::hide(ns("dataOrLink"), asis = TRUE)
                   }

                   dataSource$file <- NULL
                   dataSource$filename <- NULL
                   dataSource$type <- NULL
                   dataSource$input <- NULL

                   # Do not reset inputs when switching the input$source!!
                   # This would also reset input values after importing a data link.
                 }) %>%
                   bindEvent(input$source)

                 observe({
                   logDebug("reset CKAN fields")
                   # reset ckanGroup, ckanRecord, ckanResourceTypes
                   # reset meta string
                   updateTextInput(session, "repoFilter-ckanMeta", value = "")
                   # reset network filter
                   updatePickerInput(session,
                                     "repoFilter-ckanGroup",
                                     choices = getCKANGroupChoices(groupList = ckanNetworks()),
                                     selected = character(0))
                   # reset file type filter
                   fileTypes <- getCKANTypesChoices(packageList = ckanPackages(),
                                                    ckanFileTypes = ckanFileTypes)
                   updatePickerInput(session,
                                     "resourceFilter-ckanResourceTypes",
                                     choices = fileTypes,
                                     selected = fileTypes)

                   # reset repository
                   choicesRepo <- getCKANRecordChoices(packageList = ckanPackages())
                   updateSelectizeInput(session,
                                        "resourceFilter-ckanRecord",
                                        choices = choicesRepo,
                                        selected = choicesRepo[1])
                   # reset resource
                   choicesResource <- getCKANResourcesChoices(packageList = ckanPackages())
                   updateSelectizeInput(session,
                                        "resourceLoad-ckanResource",
                                        choices = choicesResource,
                                        selected = choicesResource[1])
                 }) %>%
                   bindEvent(input[["resourceLoad-resetCKAN"]])

                 observe({
                   req(isTRUE(internetCon()))
                   logDebug("Apply Meta filter")
                   updateSelectizeInput(session,
                                        "resourceFilter-ckanRecord",
                                        choices = getCKANRecordChoices(network = input[["repoFilter-ckanGroup"]],
                                                                       pattern = input[["repoFilter-ckanMeta"]],
                                                                       packageList = ckanPackages()))
                   fileTypes <- getCKANTypesChoices(network = input[["repoFilter-ckanGroup"]],
                                                    pattern = input[["repoFilter-ckanMeta"]],
                                                    packageList = ckanPackages(),
                                                    ckanFileTypes = ckanFileTypes)
                   updatePickerInput(session,
                                     "resourceFilter-ckanResourceTypes",
                                     choices = fileTypes,
                                     selected = fileTypes)
                 }) %>%
                   bindEvent(input[["repoFilter-applyMeta"]])

                 observe({
                   req(isTRUE(internetCon()))
                   logDebug("Apply Network filter")
                   updateSelectizeInput(session,
                                        "resourceFilter-ckanRecord",
                                        choices = getCKANRecordChoices(network = input[["repoFilter-ckanGroup"]],
                                                                       pattern = input[["repoFilter-ckanMeta"]],
                                                                       packageList = ckanPackages()))
                   fileTypes <- getCKANTypesChoices(network = input[["repoFilter-ckanGroup"]],
                                                    pattern = input[["repoFilter-ckanMeta"]],
                                                    packageList = ckanPackages(),
                                                    ckanFileTypes = ckanFileTypes)
                   updatePickerInput(session,
                                     "resourceFilter-ckanResourceTypes",
                                     choices = fileTypes,
                                     selected = fileTypes)
                 }) %>%
                   bindEvent(input[["repoFilter-ckanGroup"]])

                 observe({
                   logDebug("Apply file type filter")
                   # reset and update resource
                   choicesResource <- getCKANResourcesChoices(fileType = input[["resourceFilter-ckanResourceTypes"]],
                                                              repository = "",
                                                              network = input[["repoFilter-ckanGroup"]],
                                                              pattern = input[["repoFilter-ckanMeta"]],
                                                              packageList = ckanPackages())
                   updateSelectizeInput(session,
                                        "resourceLoad-ckanResource",
                                        choices = choicesResource,
                                        selected = choicesResource[1])
                 }) %>%
                   bindEvent(input[["resourceFilter-ckanResourceTypes"]])

                 filterCKANResourceServer("resourceFilter")
                 loadCKANResourceServer("resourceLoad")

                 observe({
                   req(isTRUE(internetCon()))
                   logDebug("Updating ckanResources()")

                   choicesResource <- getCKANResourcesChoices(fileType = input[["resourceFilter-ckanResourceTypes"]],
                                                              repository = input[["resourceFilter-ckanRecord"]],
                                                              network = input[["repoFilter-ckanGroup"]],
                                                              pattern = input[["repoFilter-ckanMeta"]],
                                                              packageList = ckanPackages())

                   updateSelectizeInput(session,
                                        "resourceLoad-ckanResource",
                                        choices = choicesResource,
                                        selected = choicesResource[1])
                 }) %>%
                   bindEvent(list(input[["resourceFilter-ckanRecord"]], input[["resourceFilter-ckanResourceTypes"]],
                                  input[["repoFilter-ckanGroup"]], input[["repoFilter-applyMeta"]]))

                 # UPDATE dataSource ----
                 ## logic for ckan, file, url, model ----
                 observe({
                   req(input[["resourceLoad-ckanResource"]])
                   logDebug("load CKAN file")
                   # dataSource of "CKAN" ----
                   dataSource <- dataSource %>%
                     getDataSource(input = input,
                                   type = "ckan",
                                   isInternet = internetCon()) %>%
                     addSourceType(importType = importType,
                                   source = "ckan",
                                   inputDataOrLink = input[["dataOrLink"]])
                 }) %>%
                   bindEvent(input[["resourceLoad-loadCKAN"]])

                 observe({
                   logDebug("Updating input$file")
                   # dataSource of "file" ----
                   dataSource <- dataSource %>%
                     getDataSource(input = input,
                                   type = "file",
                                   isInternet = internetCon()) %>%
                     addSourceType(importType = importType,
                                   source = "file",
                                   inputDataOrLink = input[["dataOrLink"]])
                 }) %>%
                   bindEvent(input$file)

                 observe({
                   logDebug("Updating input$url")
                   # dataSource of "url" ----
                   req(input$source == "url", input$url)
                   req(trimws(input$url) != "")

                   dataSource <- dataSource %>%
                     getDataSource(input = input,
                                   type = "url",
                                   isInternet = internetCon()) %>%
                     addSourceType(importType = importType,
                                   source = "url",
                                   inputDataOrLink = input[["dataOrLink"]])
                 }) %>%
                   bindEvent(input$loadUrl)

                 pathToRemote <- remoteModelsServer(
                   "remoteModels",
                   githubRepo = githubRepo,
                   folderOnGithub = folderOnGithub,
                   pathToLocal = pathToLocal,
                   reloadChoices = openPopupReset,
                   resetSelected = reactive(list(input$source, openPopupReset())),
                   isInternet = internetCon
                 )

                 observe({
                   req(!is.null(input$source))
                   # dataSource of "remoteModel" ----
                   req(input$source == "remoteModel")
                   logDebug("Updating input$remoteModels")

                   dataSource <- dataSource %>%
                     getDataSource(input = input,
                                   pathToFile = pathToRemote(),
                                   type = "remoteModel",
                                   isInternet = internetCon()) %>%
                     addSourceType(importType = importType,
                                   source = "remoteModel",
                                   inputDataOrLink = input[["dataOrLink"]])

                   req(pathToRemote())
                   updateSelectInput(session, "fileType-type", selected = "zip")
                 }) %>%
                   bindEvent(pathToRemote(), ignoreNULL = FALSE)
                 # End UPDATE dataSource ----

                 dataSource
               })
}

filterCKANRepoUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$label(class = "control-label", HTML(
      paste(
        "Filter Pandora repositories &nbsp",
        # cannot use function 'showInfoUI' -> error when load_all; problem in conditional panel?
        tags$i(
          class = "glyphicon glyphicon-info-sign",
          style = sprintf("color:%s;", "#0072B2"),
          title =
            paste(
              "- Filter 'some', or 'key', or 'words':",
              "   'some|key|words'",
              "- Filter 'some', and 'key', and 'words':",
              "   '(some+)(.*)(key+)(.*)(words+)'",
              sep = " \n"
            )
        )
      )
    )),
    fluidRow(
      column(
        5,
        style = "margin-top: 0.5em;",
        textInput(
          ns("ckanMeta"),
          label = NULL,
          value = "",
          placeholder = "Meta data"
        )
      ),
      column(1,
             style = "margin-top: 0.5em; margin-left: -2em",
             actionButton(
               ns("applyMeta"),
               label = NULL,
               icon = icon("play")
             )),
      column(
        6,
        style = "margin-top: 0.5em; margin-left: 2em",
        pickerInput(
          ns("ckanGroup"),
          label = NULL,
          choices = c("Check connection ..." = ""),
          selected = "",
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `none-selected-text` = "No network filter",
            `selected-text-format` = "count > 8",
            style = "backgound:'gray'"
          )
        )
      )
    )
  )
}

#' Filter CKAN Resource UI
#'
#' UI of the module
#'
#' @inheritParams importDataServer
#' @rdname filterCKANResourceServer
filterCKANResourceUI <- function(id, ckanFileTypes) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        7,
        selectizeInput(
          ns("ckanRecord"),
          "Pandora repository",
          choices = c("Please check connection ..." = ""),
          width = "100%",
          options = list(
            onFocus = I(
              "function() {currentVal = this.getValue(); this.clear(true); }"
            ),
            onBlur = I(
              "function() {if(this.getValue() == '') {this.setValue(currentVal, true)}}"
            )
          )
        )
      ),
      column(
        5,
        pickerInput(
          ns("ckanResourceTypes"),
          label = "Filter file type",
          choices = ckanFileTypes,
          selected = ckanFileTypes,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `none-selected-text` = "No filter",
            `deselect-all-text` = "None",
            `select-all-text` = "All",
            `selected-text-format` = "count > 8",
            style = "backgound:'dark-gray'"
          )
        )
      )
    )
  )
}

#' Filter CKAN Resource Server
#'
#' Server function of the module
#' @param id id of module
filterCKANResourceServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 # SEARCH-OPTION: important for custom options of selectizeInput for ckanRecord, ckanResource:
                 # forces update after selection (even with 'Enter') and
                 # removes 'onFocus' as well as this.clear(true)
                 ###
                 observe({
                   logDebug("Updating ckanRecord after Enter (Pandora dataset)")
                   req(input[["ckanRecord"]])
                   # see SEARCH-OPTION
                   updateSelectizeInput(session, "ckanRecord", selected = input[["ckanRecord"]])
                 }) %>%
                   bindEvent(input[["ckanRecord"]])
               })
}

#' Load CKAN Resource UI
#'
#' UI of the module
#'
#' @rdname loadCKANResourceServer
loadCKANResourceUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        7,
        selectizeInput(
          ns("ckanResource"),
          label = "Pandora repository resource",
          choices = c("No resource available ..." = ""),
          width = "100%",
          options = list(
            onFocus = I(
              "function() {currentVal = this.getValue(); this.clear(true); }"
            ),
            onBlur = I(
              "function() {if(this.getValue() == '') {this.setValue(currentVal, true)}}"
            )
          )
        )
      ),
      column(
        2,
        style = "margin-top: 1em;",
        align = "right",
        actionButton(ns("loadCKAN"), "Load")
      ),
      column(
        3,
        style = "margin-top: 1em;",
        align = "right",
        actionButton(ns("resetCKAN"), "Reset")
      )
    )
  )
}

#' Load CKAN Resource Server
#'
#' Server function of the module
#' @param id id of module
loadCKANResourceServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 # SEARCH-OPTION: important for custom options of selectizeInput for ckanRecord, ckanResource:
                 # forces update after selection (even with 'Enter') and
                 # removes 'onFocus' as well as this.clear(true)
                 ###
                 observe({
                   logDebug("Updating ckanResource after Enter")
                   req(input[["ckanResource"]])
                   # see SEARCH-OPTION
                   updateSelectizeInput(session, "ckanResource", selected = input[["ckanResource"]])
                 }) %>%
                   bindEvent(input[["ckanResource"]])
               })
}
