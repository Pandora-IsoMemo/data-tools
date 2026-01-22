# Update User Inputs

Update values from 'input' with all values from userInputs. Entries that
are NULL are removed. Only updates entries that are present in 'input'.
Copy of
[`shinyTools::updateUserInputs()`](https://pandora-isomemo.github.io/shiny-tools/reference/updateUserInputs.html)
to avoid dependency to another helper package. Export from 'DataTools'
will be removed in future versions.

## Usage

``` r
updateUserInputs(id, input, output, session, userInputs, inDataTools = FALSE)
```

## Arguments

- id:

  module id

- input:

  input object from server function

- output:

  output object from server function

- session:

  session from server function

- userInputs:

  (list) list of inputs to be updated

- inDataTools:

  (logical) internal
