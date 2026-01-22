# Update Names Entries if Duplicate

Updates the entries called "name" of a list (newList), if the names
already exist in another list (oldList). This is useful if two lists
should be concatenated and duplicated names must be prevented.

## Usage

``` r
updateNameEntryIfDuplicate(newList, oldList, listType = "List", isShiny = TRUE)
```

## Arguments

- newList:

  (list) The names of this list should be updated

- oldList:

  (list) The names of this list should be kept

- listType:

  (character) type of list, will be used in the notification if names
  had been changed.

- isShiny:

  (logical) FALSE if not used in a shiny app, e.g. in tests.
