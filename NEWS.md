# DataTools

## Version 23.02.3

### Updates
- import of Pandora dataset: keep string in input even if not found in choices (#19, PR #21)

## Version 23.02.2

### Updates
- import of Pandora dataset: empty input as default input (#19)

### Bug fixes
- disable _"Accept Merged"_ / _"Accept Query"_ buttons if merge / query failed

## Version 23.02.1

### Bug fixes
- wrong warnings when switching on/of 'First column contains rownames' 

## Version 23.01.7

### Updates
- new custom check to ensure that all columns are numeric

## Version 23.01.6

### Updates
- renaming package to DataTools

## Version 23.01.5

### Updates
- export of function `cutAllLongStrings()`
- new parameter `defaultSource` in `importDataServer()`

## Version 23.01.4

### Bug fixes
- fix typo in _Merge Data_
- fix disappearance of buttons after opening pop-up a second time

## Version 23.01.3

### Bug fixes
- reset fields in _Prepare Data_ when switching source to file or url

## Version 23.01.2

### New features
- custom checks for warnings and errors added that can be applied in the importData module

## Version 23.01.1

### New features
- module `importData` 
  - import from local files, url, or using the pandora api
  - prepare data
  - merge data
  - query data
- `importData` can be integrated into different apps
  - already present in _Pandora & IsoMemo spatiotemporal modeling_ (`MpiIsoApp`)
- app to test modules locally: _Test App_ 
