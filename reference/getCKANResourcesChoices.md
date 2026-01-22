# Get CKAN Resource Choices

Get choices that will be available in the ckanResource input

## Usage

``` r
getCKANResourcesChoices(
  fileType = character(),
  repository = "",
  network = "",
  pattern = "",
  packageList = data.frame()
)
```

## Arguments

- fileType:

  (character) list of relevant file types, e.g. c("xls", "xlsx", "csv",
  "odt")

- repository:

  (character) name of a Pandora repository, e.g. an entry of the output
  from `getRepositories()$name`

- network:

  (character) name of a Pandora network, e.g. an entry of the output
  from `getNetworks()$name`

- pattern:

  (character) string for meta information search

- packageList:

  (data.frame) optional, output of callAPI() e.g. from a previous call
  to the Pandora API.
