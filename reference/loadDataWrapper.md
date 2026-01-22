# Load Data Wrapper

Wrapper to load a data.frame from a \`"xlsx", "xls", "odt", "csv",
"txt"\` file.

## Usage

``` r
loadDataWrapper(
  values,
  filepath,
  filename,
  type,
  sep,
  dec,
  withRownames,
  withColnames,
  sheetId
)
```

## Arguments

- values:

  (list) list with import specifications

- filepath:

  (character) url or path

- filename:

  (character) file name

- type:

  (character) file type input

- sep:

  (character) column separator input

- dec:

  (character) decimal separator input

- withRownames:

  (logical) contains rownames input

- withColnames:

  (logical) contains colnames input

- sheetId:

  (numeric) sheet id
