# Configure File UI

UI of the module

Server function of the module

## Usage

``` r
configureFileUI(
  id,
  customHelpText = importOptions()[["customHelpText"]],
  defaultFileTypes = config()[["dataFileTypes"]],
  userFileTypes = c()
)

configureFileServer(
  id,
  importType = "model",
  dataSource,
  subFolder = NULL,
  rPackageName = importOptions()[["rPackageName"]],
  onlySettings = FALSE,
  fileExtension = "zip",
  expectedFileInZip = c()
)
```

## Arguments

- id:

  id of module

- customHelpText:

  (list) A help text element that can be added to a UI definition.
  Output of `shiny::helpText(...)`.

- defaultFileTypes:

  (character) default file types

- userFileTypes:

  (character) user file types specified in "Pandora Platform" settings

- importType:

  (character) type of import, either "model", "zip" or "list".
  ImportType == "model" expects a zip file containing a model. The file
  will be unzipped, the model object extracted, and checked if it is
  valid for the app. ImportType == "zip" enables the optional parameter
  'expectedFileInZip'. The file is validated and the path to the zip
  file will be returned. ImportType == "list" expects a json file
  containing a list. The file will be read and checked.

- dataSource:

  (reactiveValues) path, filename, type and input, output of
  [`selectSourceServer()`](https://pandora-isomemo.github.io/data-tools/reference/selectSourceServer.md)

- subFolder:

  (character) (optional) subfolder containing loadable .zip files.

- rPackageName:

  (character) name of the package (as in the Description file) in which
  this module is called. This value is needed to determine the Github
  repository of the package for loading example models or zip or data
  links.

- onlySettings:

  (logical) if TRUE allow only upload of user inputs and user data.

- fileExtension:

  (character) DEPRECATED. Instead, please use ckanFileTypes.

- expectedFileInZip:

  (character) (optional) This parameter is ignored if importType !=
  "zip". File names that must be contained in the zip upload.
