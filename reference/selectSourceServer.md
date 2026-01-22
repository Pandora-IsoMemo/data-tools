# Select Source UI

UI of the module

Server function of the module

## Usage

``` r
selectSourceUI(
  id,
  defaultSource,
  ckanFileTypes,
  importType,
  isInternet,
  fileInputAccept = "zip"
)

selectSourceServer(
  id,
  importType = "data",
  openPopupReset,
  internetCon,
  githubRepo,
  folderOnGithub,
  pathToLocal,
  ckanFileTypes = c("xls", "xlsx", "csv", "odt", "txt")
)
```

## Arguments

- id:

  id of module

- defaultSource:

  (character) default source for input "Source", e.g. "ckan", "file", or
  "url"

- ckanFileTypes:

  (character) file types allowed for import from Pandora ("ckan"). E.g.
  for \`importType = "data"\`: c("xls", "xlsx", "csv", "odt", "txt");
  for \`importType = "zip"\`: c("zip"); for \`importType = "list"\`:
  c("json")

- importType:

  (character) type of import, either "model", "zip" or "list".
  ImportType == "model" expects a zip file containing a model. The file
  will be unzipped, the model object extracted, and checked if it is
  valid for the app. ImportType == "zip" enables the optional parameter
  'expectedFileInZip'. The file is validated and the path to the zip
  file will be returned. ImportType == "list" expects a json file
  containing a list. The file will be read and checked.

- isInternet:

  (logical) set TRUE, if there is an internet connection. This parameter
  is ignored if `type = "file"` or `type = "remoteModel"`

- fileInputAccept:

  (character) (optional) accept attribute for fileInput. E.g. ".zip" or
  ".csv"

- openPopupReset:

  (reactive) TRUE if popup is (re-)opened

- internetCon:

  (reactive) TRUE if there is an internet connection

- githubRepo:

  (character) name of used github repository, e.g. "bpred"

- folderOnGithub:

  (character) folder on github where remote files are stored. This
  should correspond to 'pathToLocal' since online and offline files
  should be the same and up-to-date

- pathToLocal:

  (character) relative path to the folder storing local files
