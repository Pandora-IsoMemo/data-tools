# Upload model module

UI function to upload a zip file with exportNotes and a list of models

## Usage

``` r
uploadModelUI(
  id,
  label,
  labelRemote = "Load online model",
  labelLocal = "Load local model",
  fileExtension = "zip",
  width = NULL
)
```

## Arguments

- id:

  id of module

- label:

  title of module

- labelRemote:

  label of the input for remote files

- labelLocal:

  label of the input for local files

- fileExtension:

  (character) (optional) app specific file extension, e.g. "resources",
  "bpred", "bmsc". Only files with this extension are selectable.

- width:

  width of inputs in percent
