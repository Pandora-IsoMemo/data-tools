# Import module

Displays a button which opens a import dialog when clicked

Backend for import module

## Usage

``` r
importUI(id, label = "Import Model", title = NULL, titleTag = "h4")

importServer(
  id,
  title = "",
  defaultSource = c("ckan", "file", "url", "remoteModel"),
  ckanFileTypes = c("zip"),
  ignoreWarnings = FALSE,
  importType = c("model", "zip", "list"),
  fileExtension = NULL,
  subFolder = NULL,
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

  (character) module title

- titleTag:

  (character) HTML tag to put around the title, e.g. "h4" for `h4` from
  `htmltools`

- defaultSource:

  (character) default source for input "Source", e.g. "ckan", "file", or
  "url"

- ckanFileTypes:

  (character) file types allowed for import from Pandora ("ckan"). E.g.
  for \`importType = "model"\`: c("resources", "zip"); \`importType =
  "zip"\`: c("zip"); for \`importType = "list"\`: c("json")

- ignoreWarnings:

  (logical) TRUE to enable imports in case of warnings

- importType:

  (character) type of import, either "model", "zip" or "list".
  ImportType == "model" expects a zip file containing a model. The file
  will be unzipped, the model object extracted, and checked if it is
  valid for the app. ImportType == "zip" enables the optional parameter
  'expectedFileInZip'. The file is validated and the path to the zip
  file will be returned. ImportType == "list" expects a json file
  containing a list. The file will be read and checked.

- fileExtension:

  (character) DEPRECATED. Instead, please use ckanFileTypes.

- subFolder:

  (character) (optional) subfolder containing loadable .zip files.

- onlySettings:

  (logical) if TRUE allow only upload of user inputs and user data.

- expectedFileInZip:

  (character) (optional) This parameter is ignored if importType !=
  "zip". File names that must be contained in the zip upload.

- options:

  (list) Extra options for the import module. See
  [`importOptions`](https://pandora-isomemo.github.io/data-tools/reference/importOptions.md).
