# downUploadButton module server

downUploadButton module server

## Usage

``` r
downUploadButtonServer(
  id,
  dat,
  inputs,
  model,
  rPackageName,
  githubRepo,
  mainFolder = "predefinedModels",
  subFolder = NULL,
  fileExtension = "zip",
  helpHTML = "",
  modelNotes = reactive(""),
  onlySettings = FALSE,
  compress = TRUE,
  compressionLevel = 9,
  reset = reactive(FALSE),
  title = "Download and Upload of Models",
  labelRemote = "Load online model",
  labelLocal = "Load local model"
)
```

## Arguments

- id:

  module id

- dat:

  (reactive) user data

- inputs:

  (reactiveValues) reactiveValues list of user inputs, in most cases
  just the "inputs" list

- model:

  (reactive) model output object

- rPackageName:

  (character) name of the package (as in the Description file) in which
  this module is called. This value is needed to determine the Github
  repository of the package for loading example models or zip or data
  links.

- githubRepo:

  (character) name of used github repository, e.g. "bpred"

- mainFolder:

  (character) folder containing all loadable .zip files. For most apps
  this is folder "predefinedModels". In most apps it can be found under
  "inst/app/".

- subFolder:

  (character) (optional) subfolder containing loadable .zip files

- fileExtension:

  (character) (optional) app specific file extension, e.g. "resources",
  "bpred", "bmsc"

- helpHTML:

  content of help function

- modelNotes:

  (reactive) notes regarding the object to be saved and displayed when
  uploaded

- onlySettings:

  (logical) if TRUE allow only download of user inputs and user data

- compress:

  a logical specifying whether saving to a named file is to use "gzip"
  compression, or one of "gzip", "bzip2" or "xz" to indicate the type of
  compression to be used. Ignored if file is a connection.

- compressionLevel:

  A number between 1 and 9. 9 compresses best, but it also takes the
  longest.

- reset:

  (reactive) resets the selection of the online files

- title:

  title used inside the modal

- labelRemote:

  label of the input for remote files

- labelLocal:

  label of the input for local files
