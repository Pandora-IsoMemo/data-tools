# Convenience: import zip, index it, optionally load known parts

Convenience: import zip, index it, optionally load known parts

## Usage

``` r
import_bundle_zip(
  zipfile,
  extract_dir = NULL,
  keep_dir = FALSE,
  include_hidden = FALSE,
  load_known = TRUE
)
```

## Arguments

- zipfile:

  Path to zip.

- extract_dir:

  Optional directory to extract to (NULL = temp).

- keep_dir:

  Keep extracted files if temp dir is used.

- include_hidden:

  Include dotfiles.

- load_known:

  Logical; if TRUE returns loaded known parts as well.

## Value

A list with \$zip_import (ZipImport), \$available (list), and optionally
\$loaded.
