# Filter CKAN Resource UI

UI of the module

Server function of the module

## Usage

``` r
filterCKANResourceUI(id, ckanFileTypes)

filterCKANResourceServer(id)
```

## Arguments

- id:

  id of module

- ckanFileTypes:

  (character) file types allowed for import from Pandora ("ckan"). E.g.
  for \`importType = "data"\`: c("xls", "xlsx", "csv", "ods", "txt");
  for \`importType = "zip"\`: c("zip"); for \`importType = "list"\`:
  c("json")
