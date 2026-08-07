# DataTools Package

<!-- badges: start -->
[![R-CMD-check](https://github.com/Pandora-IsoMemo/data-tools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/data-tools/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/Pandora-IsoMemo/data-tools/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/data-tools/actions/workflows/pkgdown.yaml)
[![docker-publish](https://github.com/Pandora-IsoMemo/data-tools/actions/workflows/docker-publish.yml/badge.svg)](https://github.com/Pandora-IsoMemo/data-tools/actions/workflows/docker-publish.yml)
<!-- badges: end -->

### Contains:

- functions and modules that can be applied across the Isomemo Apps, and
- an app to test the modules.

### Documenation
- https://pandora-isomemo.github.io/data-tools/

### Release notes:
- see `NEWS.md`


## The Import Data Module

Access to uploads from file, url, and the Pandora Platform. Optionally, merge data before the import via UI or SQL.

UI function:

```R
DataTools::importDataUI(id, label = "Import Data")
```

Server function:

```R
DataTools::importDataServer(id, rowNames = NULL, colNames = NULL, customWarningChecks = list(), customErrorChecks = list(), ignoreWarnings = FALSE, defaultSource = "ckan")
```

Example how to apply the UI and the server function in a shiny module:

https://github.com/Pandora-IsoMemo/data-tools/blob/91a16dd78bdada382e378d78eb218bb8b815f1bc/R/01-toolsPanel.R#L13

https://github.com/Pandora-IsoMemo/data-tools/blob/91a16dd78bdada382e378d78eb218bb8b815f1bc/R/01-toolsPanel.R#L31-L37


Functions to check the validity of imports:

```R
DataTools::checkWarningEmptyValues(data)
DataTools::checkAnyNonNumericColumns(data)
DataTools::checkErrorNoNumericColumns(data)
```

---

## Notes for developers

### General helper functions

- function to catch and forward errors and warnings to the app's UI
  ```R
  shinyTools::shinyTryCatch(expr, messagePreError)
  ```

### Naming conventions in this package
Numbers as Prefix. Grouped into files with same type of functionality.  
Lower number indicate that the functions are closer to the functionality of the app / higher abstraction level

00: Start application / main functionality / Namespace  
01: Main shiny modules (basically tabs in the app)  
02: Helper shiny modules (files include ui + server component)  
03: Helper functions

### Documentaion 

When adding information to the _help_ sites, _docstrings_ or the _vignette_ of this 
package, please update documentation locally as follows. The documentation of
the main branch is built automatically via GitHub Actions.

```R
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```

### Docker

When testing with a local docker container, please make sure to rebuild the docker image after changes in the R code or dependencies. You can do this from the root of the repository via:

```bash
docker build -t datatools-test-app:latest .
```

After that, start the container as usual via:

```bash
docker run -p 3838:3838 datatools-test-app:latest
```

and access the app in your browser at `http://localhost:3838/`. Stop the container with `CTRL + C` in the terminal.

**Optional:**

Add `-it` for interactive mode, or `--rm` to remove the container after stopping.

