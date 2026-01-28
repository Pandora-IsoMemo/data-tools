# UI function of download model module

Download model module to download a zip file with notes and a (list of)
model(s)

## Usage

``` r
downloadModelUI(
  id,
  title = NULL,
  titleTag = "h4",
  label = "Download",
  width = NULL
)

downloadModelServer(
  id,
  dat,
  inputs,
  model,
  pathToOtherZip = reactive(NULL),
  rPackageName,
  subFolder = NULL,
  customFileName = reactive(""),
  defaultFileName = "",
  fileExtension = "zip",
  helpHTML = "",
  modelNotes = reactive(""),
  triggerUpdate = reactive(TRUE),
  onlySettings = FALSE,
  compress = TRUE,
  compressionLevel = 9
)
```

## Arguments

- id:

  namespace id

- title:

  (character) module title

- titleTag:

  (character) HTML tag to put around the title, e.g. "h4" for `h4` from
  `htmltools`

- label:

  button label

- width:

  width of inputs in percent

- dat:

  (reactive) user data

- inputs:

  (reactiveValues) reactiveValues list of user inputs, in most cases
  just the "inputs" list

- model:

  (reactive) model output object

- pathToOtherZip:

  (reactive) path to another zip file. The content will be added to the
  zip file that contains the content to download. Needed in MapR. If
  NULL, no files are added.

- rPackageName:

  (character) name of the package (as in the Description file) in which
  this module is called. This value is needed to determine the Github
  repository of the package for loading example models or zip or data
  links.

- subFolder:

  (character) (optional) subfolder containing loadable .zip files

- customFileName:

  (reactive) custom file name, if empty ("") the default file name is
  used. For example, this could be a reactive name that is updated after
  a model was uploaded into the app.

- defaultFileName:

  (character) default file name, if empty ("") a default file name is
  created containing the current time

- fileExtension:

  (character) (optional) app specific file extension, e.g. "resources",
  "bpred", "bmsc"

- helpHTML:

  content of help function

- modelNotes:

  (reactive) notes regarding the object to be saved and displayed when
  uploaded

- triggerUpdate:

  (reactive) trigger the update of the "Notes" text input. Useful, when
  applying this module inside a modal window.

- onlySettings:

  (logical) if TRUE allow only download of user inputs and user data

- compress:

  a logical specifying whether saving to a named file is to use "gzip"
  compression, or one of "gzip", "bzip2" or "xz" to indicate the type of
  compression to be used. Ignored if file is a connection.

- compressionLevel:

  A number between 1 and 9. 9 compresses best, but it also takes the
  longest.
