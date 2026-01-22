# Extract Object From File

Extract the inputs and data objects from from either a "inputs.rds" file
or from a "model.Rdata" file. Extracts the model object from either a
"model.rds" file or from a "model.Rdata" file. Recent model objects are
only stored as "model.rds".

## Usage

``` r
extractObjectFromFile(pathToUnzipped, what = NULL)
```

## Arguments

- pathToUnzipped:

  (character) path to the folder were the model was unzipped

- what:

  (character) DEPRECATED. Argument will be ignored! what should be
  extracted, one of "model" or "inputs"

## Value

(list) model object
