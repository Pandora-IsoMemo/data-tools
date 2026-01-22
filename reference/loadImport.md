# Load Import

Wrapper to load "model", "zip" or "list" file.

## Usage

``` r
loadImport(importType, params)
```

## Arguments

- importType:

  (character) type of import, either "model", "zip" or "list".
  ImportType == "model" expects a zip file containing a model. The file
  will be unzipped, the model object extracted, and checked if it is
  valid for the app. ImportType == "zip" enables the optional parameter
  'expectedFileInZip'. The file is validated and the path to the zip
  file will be returned. ImportType == "list" expects a json file
  containing a list. The file will be read and checked.

- params:

  (list) named list of parameters required to import data or a model
