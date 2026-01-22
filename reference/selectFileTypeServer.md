# Select File Type UI

UI of the module

Server function of the module

## Usage

``` r
selectFileTypeUI(
  id,
  defaultFileTypes = config()[["dataFileTypes"]],
  userFileTypes = c()
)

selectFileTypeServer(id, dataSource, defaultFileTypes)
```

## Arguments

- id:

  id of module

- defaultFileTypes:

  default file types

- userFileTypes:

  (character) user file types specified in "Pandora Platform" settings

- dataSource:

  (reactiveValues) path, filename, type and input, output of
  [`selectSourceServer()`](https://pandora-isomemo.github.io/data-tools/reference/selectSourceServer.md)
