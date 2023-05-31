# DataTools

## Version: 23.05.5

### New Features
- _Import of Pandora datasets_: info icon next to the meta data filter with examples how to filter for several keywords (#18)

### Bug Fixes
- catch api connection errors and output them in UI
- catch errors for wrong meta filters and output them in UI

## Version: 23.05.4

### New Features
- _Import of Pandora datasets_:
  - option to filter the list of Pandora datasets for a string in their metadata (#18)
  - option to select a ckan group to filter the list of Pandora datasets (#20)

## Version: 23.05.3

### New Features
- _Import of Pandora datasets_:
  - alphabetical order of datasets and resources (#18)
  - select resource types to be displayed (xls, xlsx, csv, odt, txt, all) (#18)

## Version: 23.05.2

### Bug Fixes
- fixes issues when trying to reach the isomemo, github and pandorra API without internet connection

## Version 23.05.1

### New Features
- _Import Data_ module
  - in the tab _Prepare_: option to select the file that should be updated
  - in the tab _Merge_: option to merge several files
    - merge two files, save with a new file name, select the new file and another table, merge,
    repeat, ...
  - in the tab _Query with SQL_: option to keep a new file and do some data preparation in other tabs

## Version 23.05.0

### New Features
- new function to update names of lists if the names occur in another list, 
  see `updateListNamesIfDuplicate()`

## Version 23.04.3

### New Features
- new functions to reach the isomemo API that were extracted from the iso-app

## Version 23.04.2

### Updates
- module _downUploadButton_: update of titles, progress bar for upload, close pop up after download

## Version 23.04.1

### New Features
- new module _downUploadButton_: A button that opens a pop up with the download and upload UI for
models. The upload contains upload from local files (3) or upload from a remote github folder. If
there is no access to the github folder, a note is displayed.

### Updates
- `remoteModelsServer`: new parameter to set the folder for remote models on github if it differs
from the default folder

## Version 23.03.3

### New Features
- _Down- and Upload of Models_ module: UI and server functions for down and upload of user data, 
user inputs and model output

## Version 23.03.2

### New Features
- _Remote Models_ module: UI and server functions for selecting a remote (or local) model and
obtaining the remote path on github (or the local path) to the file of the selected model

## Version 23.03.1

### Updates
- _Import Data_ module in the tab _Query with SQL_: before using the GPT-3 feature a confirmation 
is required (#15)

## Version 23.02.8

### New Features
- _Import Data_ module in the tab _Query with SQL_: option to use GPT-3 for creation of SQL
queries (#15)

## Version 23.02.7

### New Features
- New function to catch and forward errors and warnings to the UI of the app. If a warning is 
triggered the result of the evaluated expression is kept and returned.

## Version 23.02.6

### Updates
- factors are converted to character in cutAllLongStrings (#25)

## Version 23.02.5

### Bug Fixes
- apply output format before applying custom checks

## Version 23.02.4

### New Features
- UI for batch mode (requirement for ReSources app)
- option to format output as matrix (requirement for ReSources app)

### Updates
- update logic for assignment of colnames and rownames because of matrix outputs

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
