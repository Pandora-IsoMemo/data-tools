# DataTools Package

Contains 

- functions and modules that can be applied across the Isomemo Apps, and
- an app to test the modules.

## The Import Data Module

Access to uploads from file, url, and the Pandora Platform.

### UI function

```R
DataTools::importDataUI(id, label = "Import Data")
```

Example how to apply the UI function in a shiny module:

https://github.com/Pandora-IsoMemo/data-tools/blob/91a16dd78bdada382e378d78eb218bb8b815f1bc/R/01-toolsPanel.R#L13

### Server function

```R
DataTools::importDataServer(id, rowNames = NULL, colNames = NULL, customWarningChecks = list(), customErrorChecks = list(), ignoreWarnings = FALSE, defaultSource = "ckan")
```

Example how to apply the server function in a shiny module:

https://github.com/Pandora-IsoMemo/data-tools/blob/91a16dd78bdada382e378d78eb218bb8b815f1bc/R/01-toolsPanel.R#L31-L37


#### Functions to check validity of imports

```R
DataTools::checkWarningEmptyValues(data)
DataTools::checkAnyNonNumericColumns(data)
DataTools::checkErrorNoNumericColumns(data)
```

---

### Naming Conventions in this package
Numbers as Prefix. Grouped into files with same type of functionality.  
Lower number indicate that the functions are closer to the functionality of the app / higher abstraction level

00: Start application / main functionality / Namespace  
01: Main shiny modules (basically tabs in the app)  
02: Helper shiny modules (files include ui + server component)  
03: Helper functions
