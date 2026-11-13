# Get Data Source

Get Data Source

## Usage

``` r
getDataSource(
  dataSource = reactiveValues(file = NULL, filename = NULL, type = NULL, input = NULL),
  input,
  type = c("ckan", "radogost", "file", "url", "remoteModel"),
  isInternet = TRUE,
  pathToFile = NULL,
  radogostFiles = NULL,
  radogostDownload = download_radogost_file
)
```

## Arguments

- dataSource:

  (reactiveValues) path, filename, type and input, output of
  [`selectSourceServer()`](https://pandora-isomemo.github.io/data-tools/reference/selectSourceServer.md)

- input:

  (reactiveValues)

- type:

  (character) source of import, one of "ckan", "radogost", "file",
  "url", "remoteModel". Possible sources for data are: "ckan",
  "radogost", "file", "url". Possible sources for models are: "ckan",
  "file", "url", "remoteModel".

- isInternet:

  (logical) set TRUE, if there is an internet connection. This parameter
  is ignored if `type = "file"` or `type = "remoteModel"`

- pathToFile:

  (character) file path, ignored if `type != "remoteModel"`

- radogostFiles:

  (data.frame) RADOGOST search results when `type = "radogost"`. If
  NULL, reconstruct a saved source using `input[["radogost-file-url"]]`.

- radogostDownload:

  (function) RADOGOST file downloader
