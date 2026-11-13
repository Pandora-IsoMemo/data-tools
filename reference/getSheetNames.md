# Get Workbook Sheet Names

XLS content is staged with an XLS extension when needed because the
sheet reader also checks the filename. Remote ODS files are staged
because their sheet reader requires a local archive.

## Usage

``` r
getSheetNames(filepath, workbookFormat = detectWorkbookFormat(filepath))
```

## Arguments

- filepath:

  (character) url or path

- workbookFormat:

  detected workbook format
