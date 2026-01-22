# Configure Data UI

UI of the module

Server function of the module

## Usage

``` r
configureDataUI(
  id,
  batch,
  outputAsMatrix,
  isLink = FALSE,
  customHelpText = importOptions()[["customHelpText"]],
  defaultFileTypes = config()[["dataFileTypes"]],
  userFileTypes = c()
)

configureDataServer(
  id,
  dataProcessList,
  customNames,
  dataSource,
  dataSourceInputs,
  dataForPreview,
  defaultFileTypes = config()[["dataFileTypes"]],
  ignoreWarnings = FALSE
)
```

## Arguments

- id:

  id of module

- batch:

  (logical) use batch import.

- outputAsMatrix:

  (logical) TRUE if output must be a matrix, e.g. for batch = TRUE in
  Resources.

- isLink:

  (logical) if TRUE, the data source is a link

- customHelpText:

  (list) A help text element that can be added to a UI definition.
  Output of `shiny::helpText(...)`.

- defaultFileTypes:

  (character) default file types

- userFileTypes:

  (character) user file types specified in "Pandora Platform" settings

- dataProcessList:

  (reactiveVal) list of data imports submitted for data processing via
  buttons 'Create Query with data' or 'Prepare / Merge data'

- customNames:

  settings for custom column and row names

- dataSource:

  (reactiveValues) path, filename, type and input, output of
  [`selectSourceServer()`](https://pandora-isomemo.github.io/data-tools/reference/selectSourceServer.md)

- dataSourceInputs:

  (reactive) inputs related to the data source

- dataForPreview:

  (reactive) data to show in preview

- ignoreWarnings:

  (logical) TRUE to enable imports in case of warnings
