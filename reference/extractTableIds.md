# Extract Table IDs

Create IDs \`1:n\` to the names of loaded tables. Names are often url's
of the table file, something like
"https://pandoradata.earth/dataset/.../download/afriarch-isotopic-dataset.xlsx".

## Usage

``` r
extractTableIds(dataProcessList)
```

## Arguments

- dataProcessList:

  (list) list of data to be merged

## Value

(character) short internal table names
