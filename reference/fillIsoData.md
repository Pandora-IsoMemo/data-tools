# Fill Iso Data

Fill Iso Data

## Usage

``` r
fillIsoData(data, mapping)
```

## Arguments

- data:

  (data.frame) Output from IsoMemo::getData(). A data frame containing
  the requested databases, category domains, and variables of interest
  from the user

- mapping:

  (data.frame) Output from IsoMemo::getFields() A data frame that
  describes data field name, data type, and domain category

## Value

A data frame also containing the columns defined in the mapping but
missing in the data. New columns have all values set to NA.
