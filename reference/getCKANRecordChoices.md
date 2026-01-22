# Get CKAN Record Choices

Get choices that will be available in the ckanRecord (repository) input

## Usage

``` r
getCKANRecordChoices(network = "", pattern = "", packageList = data.frame())
```

## Arguments

- network:

  (character) name of a Pandora network, e.g. an entry of the output
  from `getNetworks()$name`

- pattern:

  (character) string for meta information search

- packageList:

  (data.frame) optional, output of callAPI() e.g. from a previous call
  to the Pandora API.
