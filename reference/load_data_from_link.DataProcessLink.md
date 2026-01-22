# S3 method: Load data from DataProcessLink Loads data from the source specified in the DataProcessLink object.

S3 method: Load data from DataProcessLink Loads data from the source
specified in the DataProcessLink object.

## Usage

``` r
# S3 method for class 'DataProcessLink'
load_data_from_link(object, ...)
```

## Arguments

- object:

  The DataProcessLink object to load data from.

- ...:

  Additional arguments, including: - values: A reactiveValues object to
  store loading results. - customNames: A list with custom naming
  options (withRownames, withColnames).

## Value

A reactiveValues object containing the loaded data and status.
