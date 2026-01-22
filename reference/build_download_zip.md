# Build a download bundle zip file

This function creates a zip file containing the specified model data,
inputs, notes, and help documentation. It utilizes the DownloadBundle
class to manage the bundling process.

## Usage

``` r
build_download_zip(
  zipfile = tempfile(fileext = ".zip"),
  package_name = NULL,
  dat = NULL,
  inputs = NULL,
  model = NULL,
  sub_model = NULL,
  help_html = NULL,
  notes = NULL,
  exclude_model = FALSE,
  include_paths = NULL,
  include_root = NULL,
  compression_level = 9
)
```

## Arguments

- zipfile:

  The path to the output zip file.

- package_name:

  The name of the R package for which the bundle is created.

- dat:

  The model data to be included in the bundle.

- inputs:

  The model inputs to be included in the bundle.

- model:

  The model object to be included in the bundle.

- sub_model:

  Optional sub-model name within the R package.

- help_html:

  Optional HTML content for help documentation.

- notes:

  Optional notes to be included in the bundle.

- exclude_model:

  Logical indicating whether to include only settings (inputs and data)
  in the bundle, rather than the full model (default is FALSE).

- include_paths:

  Optional character vector of additional file/directory paths to
  include in the bundle.

- include_root:

  Optional root directory for the additional paths to preserve relative
  structure.

- compression_level:

  The level of compression to use when zipping the bundle (default is
  9).

## Value

The path to the created zip file (invisible).
