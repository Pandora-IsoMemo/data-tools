# Load Zip Wrapper

Wrapper function to load a zip file. This function does not unzip the
file, but checks if the file is a valid zip file and returns the path to
the unzipped file.

## Usage

``` r
loadZipWrapper(
  values,
  filepath,
  filename,
  fileExtension = "zip",
  expectedFileInZip,
  ...
)
```

## Arguments

- values:

  (list) list with import specifications

- filepath:

  (character) url or path

- filename:

  (character) name of the model file

- fileExtension:

  (character) (optional) app specific file extension, e.g. "resources",
  "bpred", "bmsc"

- expectedFileInZip:

  (character) expected files in the zip file

- ...:

  parameters for other wrappers
