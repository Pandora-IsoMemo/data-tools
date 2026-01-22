# Get Data Source

Get Data Source

## Usage

``` r
getDataSource(
  dataSource = reactiveValues(file = NULL, filename = NULL, type = NULL, input = NULL),
  input,
  type = c("ckan", "file", "url", "remoteModel"),
  isInternet = TRUE,
  pathToFile = NULL
)
```

## Arguments

- dataSource:

  (reactiveValues) path, filename, type and input, output of
  [`selectSourceServer()`](https://pandora-isomemo.github.io/data-tools/reference/selectSourceServer.md)

- input:

  (reactiveValues)

- type:

  (character) source of import, one of "ckan", "file", "url",
  "remoteModel". Possible sources for data are: "ckan", "file", "url".
  Possible sources for models are: "ckan", "file", "url", "remoteModel".

- isInternet:

  (logical) set TRUE, if there is an internet connection. This parameter
  is ignored if `type = "file"` or `type = "remoteModel"`

- pathToFile:

  (character) file path, ignored if `type != "remoteModel"`
