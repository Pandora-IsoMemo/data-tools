# Create a new DownloadBundle object

This function creates a new DownloadBundle object, which is used to
prepare a bundle of files for download, including RDS files, notes, and
help files.

## Usage

``` r
new_DownloadBundle(
  package_name,
  sub_model = NULL,
  help_html = "",
  compression_level = 9
)
```

## Arguments

- package_name:

  The name of the R package for which the bundle is created.

- sub_model:

  Optional sub-model name within the R package.

- help_html:

  Optional HTML content for help documentation.

- compression_level:

  The level of compression to use when zipping the bundle (default is
  9).

## Value

A DownloadBundle object containing the specified parameters and a
temporary directory for file storage.
