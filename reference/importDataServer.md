# Data import module

Displays a button which opens an import dialog when clicked. Wrapper
around
[`importUI`](https://pandora-isomemo.github.io/data-tools/reference/importServer.md)
with default label "Import Data".

Backend for data import module

## Usage

``` r
importDataUI(id, label = "Import Data")

importDataServer(
  id,
  title = "",
  defaultSource = c("ckan", "file", "url", "remoteModel"),
  ckanFileTypes = c("xls", "xlsx", "csv", "odt", "txt"),
  ignoreWarnings = FALSE,
  importType = c("data", "model", "zip", "list"),
  rowNames = reactiveVal(NULL),
  colNames = reactiveVal(NULL),
  customWarningChecks = list(),
  customErrorChecks = list(),
  batch = FALSE,
  outputAsMatrix = FALSE,
  fileExtension = NULL,
  mainFolder = NULL,
  subFolder = NULL,
  rPackageName = "",
  onlySettings = FALSE,
  expectedFileInZip = c(),
  options = importOptions()
)
```

## Arguments

- id:

  id of module

- label:

  label of button

- title:

  title of import module

- defaultSource:

  (character) default source for input "Source", e.g. "ckan", "file", or
  "url"

- ckanFileTypes:

  (character) file types allowed for import from Pandora ("ckan"). E.g.
  for \`importType = "data"\`: c("xls", "xlsx", "csv", "odt", "txt");
  for \`importType = "zip"\`: c("zip"); for \`importType = "list"\`:
  c("json")

- ignoreWarnings:

  (logical) TRUE to enable imports in case of warnings

- importType:

  (character) DEPRECATED. type of import, either "data", "model", "zip"
  or "list". ImportType == "model" expects a zip file containing a
  model. The file will be unzipped, the model object extracted, and
  checked if it is valid for the app. ImportType == "zip" enables the
  optional parameter 'expectedFileInZip'. The file is validated and the
  path to the zip file will be returned. ImportType == "list" expects a
  json file containing a list. The file will be read and checked.

- rowNames:

  (reactive) use this for rownames of imported data.

- colNames:

  (reactive) use this for colnames of imported data.

- customWarningChecks:

  list of reactive(!) functions which will be executed after importing
  of data. functions need to return TRUE if check is successful or a
  character with a warning otherwise.

- customErrorChecks:

  list of reactive(!) functions which will be executed after importing
  of data. functions need to return TRUE if check is successful or a
  character with a warning otherwise.

- batch:

  (logical) use batch import.

- outputAsMatrix:

  (logical) TRUE if output must be a matrix, e.g. for batch = TRUE in
  Resources.

- fileExtension:

  (character) DEPRECATED. Instead, please use ckanFileTypes.

- mainFolder:

  (character) DEPRECATED. folder containing all loadable .zip files.

- subFolder:

  (character) (optional) subfolder containing loadable .zip files. This
  parameter is ignored if importType == "data"

- rPackageName:

  (character) DEPRECATED. Instead, please use
  `options = importOptions(rPackageName = <your package>)`.

- onlySettings:

  (logical) if TRUE allow only upload of user inputs and user data. This
  parameter is ignored if importType == "data"

- expectedFileInZip:

  (character) (optional) This parameter is ignored if importType !=
  "zip". File names that must be contained in the zip upload.

- options:

  (list) Extra options for the import module. See
  [`importOptions`](https://pandora-isomemo.github.io/data-tools/reference/importOptions.md).
