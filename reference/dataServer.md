# UI function of data module

Wrapper module for the
[`DataTools::importDataServer`](https://pandora-isomemo.github.io/data-tools/reference/importDataServer.md)
module that allows for loading example data

## Usage

``` r
dataUI(id, title = "Data", titleTag = "h4")

dataServer(id, path = NULL, transformations = list(), ...)
```

## Arguments

- id:

  module id

- title:

  (character) module title

- titleTag:

  (character) HTML tag to put around the title, e.g. "h4" for `h4` from
  `htmltools`

- path:

  path to the example data file, e.g. `file.path("data", "example.csv")`

- transformations:

  list of transformations to apply to the dataset

- ...:

  further arguments passed to
  [`DataTools::importDataServer`](https://pandora-isomemo.github.io/data-tools/reference/importDataServer.md)
