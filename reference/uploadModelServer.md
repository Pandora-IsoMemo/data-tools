# Server function upload model

Backend for upload model module

## Usage

``` r
uploadModelServer(
  id,
  githubRepo,
  mainFolder = "predefinedModels",
  subFolder = NULL,
  rPackageName = "",
  reloadChoices = reactive(TRUE),
  onlySettings = FALSE,
  fileExtension = "zip",
  reset = reactive(FALSE)
)
```

## Arguments

- id:

  namespace id

- githubRepo:

  (character) name of used github repository, e.g. "bpred"

- mainFolder:

  (character) folder containing all loadable .zip files. For most apps
  this is folder "predefinedModels". In most apps it can be found under
  "inst/app/".

- subFolder:

  (character) (optional) subfolder containing loadable .zip files

- rPackageName:

  (character) name of the package (as in the Description file) in which
  this module is called. This value is needed to determine the Github
  repository of the package for loading example models or zip or data
  links.

- reloadChoices:

  (reactive) trigger access to github and reload choices of remote files

- onlySettings:

  (logical) if TRUE allow only upload of user inputs and user data

- fileExtension:

  (character) (optional) app specific file extension, e.g. "resources",
  "bpred", "bmsc"

- reset:

  (reactive) resets the selection of the online files
