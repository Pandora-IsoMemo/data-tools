# Import Options

Extra options for the import module.

## Usage

``` r
importOptions(rPackageName = "", githubRepo = "", customHelpText = NULL)
```

## Arguments

- rPackageName:

  (character) name of the package (as in the Description file) in which
  this module is called. This value is needed to determine the Github
  repository of the package for loading example models or zip or data
  links.

- githubRepo:

  (character) name of used github repository, e.g. "bpred"

- customHelpText:

  (list) A help text element that can be added to a UI definition.
  Output of `shiny::helpText(...)`.
