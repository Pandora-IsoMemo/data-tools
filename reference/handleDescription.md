# Handle Description

Handle Description

## Usage

``` r
handleDescription(data, maxChar = 20)
```

## Arguments

- data:

  (data.frame) Output from IsoMemo::getData(). A data frame containing
  the requested databases, category domains, and variables of interest
  from the user

- maxChar:

  (numeric) Cut descriptions after maxChar characters for nicer display
  of the data frame in Shiny

## Value

A data frame containing a new column with full description kept while
entries of the column description are cutted.
