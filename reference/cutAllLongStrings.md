# Cut All Strings

Cuts strings of character and factor columns if a string is longer than
cutAt parameter. Factors are converted to characters before cutting.

## Usage

``` r
cutAllLongStrings(df, cutAt = 50)
```

## Arguments

- df:

  (data.frame) data.frame with character and non-character columns

- cutAt:

  (numeric) number of characters after which to cut the entries of an
  character-column
