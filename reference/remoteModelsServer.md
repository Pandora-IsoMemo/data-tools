# Remote models module

Displays a select input to choose a remote model and a button to load it

Backend for the module

## Usage

``` r
remoteModelsUI(
  id,
  selectLabel = "Load online model",
  buttonLabel = "Load",
  width = "100%"
)

remoteModelsServer(
  id,
  githubRepo,
  pathToLocal = file.path(".", "predefinedModels"),
  folderOnGithub = "/predefinedModels",
  fileExtension = "",
  onlyLocalModels = reactive(FALSE),
  resetSelected = reactive(FALSE),
  reloadChoices = reactive(TRUE),
  rPackageName = NULL,
  rPackageVersion = NULL,
  isInternet = reactive(TRUE)
)
```

## Arguments

- id:

  id of module

- selectLabel:

  label of select input

- buttonLabel:

  button label

- width:

  width of inputs in percent

- githubRepo:

  (character) name of used github repository, e.g. "bpred"

- pathToLocal:

  (character) relative path to the folder storing local files

- folderOnGithub:

  (character) folder on github where remote files are stored. This
  should correspond to 'pathToLocal' since online and offline files
  should be the same and up-to-date

- fileExtension:

  (character) DEPRECATED, not in use anymore. (otional) app specific
  file extension, e.g. "resources", "bmsc", "bpred", or (app-unspecific)
  "zip". Only files with this extension are valid for import.

- onlyLocalModels:

  (reactive) if TRUE only local files are used instead of accessing
  github

- resetSelected:

  (reactive) if TRUE resets the selected remote file

- reloadChoices:

  (reactive) trigger access to github and reload choices of remote files

- rPackageName:

  (character) DEPRECATED (not in use and will be removed in future):
  name of the package (as in the description file) in which this module
  is applied, e.g. "Bpred"

- rPackageVersion:

  (character) DEPRECATED (not in use and will be removed in future):
  current version of the package where this module is applied, e.g.
  utils::packageVersion("Bpred")

- isInternet:

  (reactive) TRUE if there is an internet connection

## Value

(character) the path to the selected remote (github) or local file
