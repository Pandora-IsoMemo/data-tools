# Create a ZipImport object

Represents a zip extraction workspace and an index of its contents.

## Usage

``` r
new_ZipImport(zipfile, extract_dir = NULL, keep_dir = FALSE)
```

## Arguments

- zipfile:

  Path to a zip file.

- extract_dir:

  Optional directory to extract to. If NULL, a temp directory is used.

- keep_dir:

  Logical; if FALSE and extract_dir is NULL, extracted files are removed
  on cleanup.

## Value

A ZipImport object.
