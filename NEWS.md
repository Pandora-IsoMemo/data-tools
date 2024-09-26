# DataTools 24.09.1

## Updates
- _Query with SQL_: option to select available gpt models for the creation of SQL queries (#78)

# DataTools 24.09.0

## Updates
- when importing models:
  - additionally to the display of warning and error messages, a new message
  displays the version number under which a model was saved
  - the version is now also part of the returned import object

# DataTools 24.08.2

## Updates
- update 'deprecated' warnings for several deprecated arguments

# DataTools 24.08.1

## Updates
- maintenance update for the selectData module, split of the module into two modules to reduce the complexity
  - _Configure Data_: configure data from clv, xlsx imports
  - _Configure File_: configure file from model, zip, list imports

# DataTools 24.08.0

## Updates
- maintenance update for the import module, preparations for the split of the import module into 
  two modules to reduce the complexity of the import data module (which will increase the speed)
  - _Import Data_: import data from local files, url, or using the pandora api
  - _Import Model/Zip_: import model objects, zip from local files, url, or using the pandora api
  - part 1: extraction of the import of model objects and zips from the importData module into a 
    separate module 

# DataTools 24.07.0

## New Features
- _Import Data_: 
  - option to import a list from a json file from url, Pandora or file (#91)
  - option to import user inputs together with a json file in order to load a previous session (#91)

# DataTools 24.06.0

## New Features
- _Download Model_: new optional parameter `pathToOtherZip` to add the content of a zip file to the
  the zip file to be downloaded (which contains also model, data and inputs objects).
  This is needed for _MapR_ where pictures are stored in zip files and needed for plots (instead of 
  model outputs)  (#89)

## Updates
- export of functions to extract notes and inputs from an uploaded file (#89)

# DataTools 24.05.2

## Updates
- _Import Data_ module:
  - updated paths to find example models locally to enable compatibility to external apps 
    (e.g. CausalR) (#86)

# DataTools 24.05.1

## Updates
- _tryCatchWithWarningsAndErrors()_:
  - shifted function to the package `shinyTools` and renamed to `shinyTryCatch()`
  - improved error handling for the function `shinyTools::shinyTryCatch()`, now all
    errors and warnings of an expression are caught and forwarded to the UI of an app
  - deprecated the function `tryCatchWithWarningsAndErrors()` in the package `DataTools`, added a
    warning message to use the function `shinyTools::shinyTryCatch()` instead

# DataTools 24.05.0

## Updates
- _Down- and Upload of Models_ module:
  - optionally, specify a filename for model download
  - store the filename of an uploaded model when reading the path

# DataTools 24.04.3

## Updates
- export of module previewData for the app 'Data search and Spatiotemporal modeling' (MpiIsoApp)

# DataTools 24.04.2

## Bug Fixes
- fix import of model zips from url (#80)
- fix issue when trying to end an open rgpt session

# DataTools 24.04.1

## Bug Fixes
- fix config of path to the zip examples

# DataTools 24.04.0

## Updates
- integration of the most recent version of rgpt functions into data tools package

# DataTools 24.03.0

## New Features
- option to download 'data queries' as .json file if data loading was successful in advance (#55)
  - stores only user inputs and file path information, not the data itself
  - save data queries that were created under 'Query with SQL'
- option to upload 'data queries' containing user inputs for data imports from a .json file
- option to upload 'data queries' containing user inputs for data imports from github from .json 
file

# DataTools 24.01.0

## Updates
- Integration of rgpt3 functions into data tools package
  - **Note:** The functions were copied from <https://github.com/ben-aaron188/rgpt3> 
& <https://github.com/gasparl/rgpt3/tree/main>
    Please refer to the original R-Packages for more details.

# DataTools 23.12.2

## Bug Fixes
- _Import of models from Pandora_: 
  - an error message occured when trying to load a model from pandora.
  - fix: adding the missing download of the zip file from the url before unpacking the zip

# DataTools 23.12.0

## New Features
- _Import Data_ module: display of "About" information that is associated to a selected Pandora 
  Repository (#62)

# DataTools 23.11.0

## Updates
- removing functions for "Pandora" API, updated functionality was moved to the package `Pandora`

# DataTools 23.10.3

## Updates
- removing functions for "IsoMemo" API, updated functionality was moved to the package `IsoMemo`

# DataTools 23.10.1 & 23.10.2

## Updates
- export of functions `fillIsoData()`, `handleDescription()`, `has_internet()` which will be needed
  for the Isomemo app

# DataTools 23.10.0
## New Features
- _Import Data_ module: A modification that is required to apply the import module within the app `MapR`.
  - new value "zip" for parameter `importType`
  - new optional parameter `expectedFileInZip`: This parameter is ignored if importType != "zip". 
  Name files that must be contained in the zip upload. 

# DataTools 23.09.0

## New Features
- _Import Data_ module: new optional parameter `fileExtension` which filters files for the import of
model objects for exactly the specified type, e.g. "zip", "resources", "bpred", "bmsc"

## Bug Fixes
- _Import Data_ module: Modal kept closing when trying to import data a second time

# DataTools 23.06.3

## New Features
- _Import Data_ module: functionality to import model objects by setting a new parameter `importType = "model"` (#50)

# DataTools 23.06.2

## New Features
- Add  a new parameter "mappingId" to all functions that are used to reach the isomemo API (#36)

# DataTools 23.06.1

## Bug Fixes
- _Import of Pandora datasets_:
  - fix and optimize checks for internet connection
  - fix issue with sheet selection if no internet
  - set 'file' as initial source if no internet

# DataTools 23.06.0

## Updates
- _Import of Pandora datasets_: rename "Pandora group" to "Pandora network", and "Pandora dataset" to "Pandora repository" (#18)

## Bug Fixes 
- contains fixes regarding issues with loading data and updates of the lists of user inputs (#47)

# DataTools 23.05.5

## New Features
- _Import of Pandora datasets_: info icon next to the meta data filter with examples how to filter for several keywords (#18)

## Bug Fixes
- catch api connection errors and output them in UI
- catch errors for wrong meta filters and output them in UI

# DataTools 23.05.4

## New Features
- _Import of Pandora datasets_:
  - option to filter the list of Pandora datasets for a string in their metadata (#18)
  - option to select a ckan group to filter the list of Pandora datasets (#20)

# DataTools 23.05.3

## New Features
- _Import of Pandora datasets_:
  - alphabetical order of datasets and resources (#18)
  - select resource types to be displayed (xls, xlsx, csv, odt, txt, all) (#18)

# DataTools 23.05.2

## Bug Fixes
- fixes issues when trying to reach the isomemo, github and pandorra API without internet connection

# DataTools 23.05.1

## New Features
- _Import Data_ module
  - in the tab _Prepare_: option to select the file that should be updated
  - in the tab _Merge_: option to merge several files
    - merge two files, save with a new file name, select the new file and another table, merge,
    repeat, ...
  - in the tab _Query with SQL_: option to keep a new file and do some data preparation in other tabs

# DataTools 23.05.0

## New Features
- new function to update names of lists if the names occur in another list, 
  see `updateListNamesIfDuplicate()`

# DataTools 23.04.3

## New Features
- new functions to reach the isomemo API that were extracted from the iso-app

# DataTools 23.04.2

## Updates
- module _downUploadButton_: update of titles, progress bar for upload, close pop up after download

# DataTools 23.04.1

## New Features
- new module _downUploadButton_: A button that opens a pop up with the download and upload UI for
models. The upload contains upload from local files (3) or upload from a remote github folder. If
there is no access to the github folder, a note is displayed.

## Updates
- `remoteModelsServer`: new parameter to set the folder for remote models on github if it differs
from the default folder

# DataTools 23.03.3

## New Features
- _Down- and Upload of Models_ module: UI and server functions for down and upload of user data, 
user inputs and model output

# DataTools 23.03.2

## New Features
- _Remote Models_ module: UI and server functions for selecting a remote (or local) model and
obtaining the remote path on github (or the local path) to the file of the selected model

# DataTools 23.03.1

## Updates
- _Import Data_ module in the tab _Query with SQL_: before using the GPT-3 feature a confirmation 
is required (#15)

# DataTools 23.02.8

## New Features
- _Import Data_ module in the tab _Query with SQL_: option to use GPT-3 for creation of SQL
queries (#15)

# DataTools 23.02.7

## New Features
- New function to catch and forward errors and warnings to the UI of the app. If a warning is 
triggered the result of the evaluated expression is kept and returned.

# DataTools 23.02.6

## Updates
- factors are converted to character in cutAllLongStrings (#25)

# DataTools 23.02.5

## Bug Fixes
- apply output format before applying custom checks

# DataTools 23.02.4

## New Features
- UI for batch mode (requirement for ReSources app)
- option to format output as matrix (requirement for ReSources app)

## Updates
- update logic for assignment of colnames and rownames because of matrix outputs

# DataTools 23.02.3

## Updates
- import of Pandora dataset: keep string in input even if not found in choices (#19, PR #21)

# DataTools 23.02.2

## Updates
- import of Pandora dataset: empty input as default input (#19)

## Bug fixes
- disable _"Accept Merged"_ / _"Accept Query"_ buttons if merge / query failed

# DataTools 23.02.1

## Bug fixes
- wrong warnings when switching on/of 'First column contains rownames' 

# DataTools 23.01.7

## Updates
- new custom check to ensure that all columns are numeric

# DataTools 23.01.6

## Updates
- renaming package to DataTools

# DataTools 23.01.5

## Updates
- export of function `cutAllLongStrings()`
- new parameter `defaultSource` in `importDataServer()`

# DataTools 23.01.4

## Bug fixes
- fix typo in _Merge Data_
- fix disappearance of buttons after opening pop-up a second time

# DataTools 23.01.3

## Bug fixes
- reset fields in _Prepare Data_ when switching source to file or url

# DataTools 23.01.2

## New features
- custom checks for warnings and errors added that can be applied in the importData module

# DataTools 23.01.1

## New features
- module `importData` 
  - import from local files, url, or using the pandora api
  - prepare data
  - merge data
  - query data
- `importData` can be integrated into different apps
  - already present in _Pandora & IsoMemo spatiotemporal modeling_ (`MpiIsoApp`)
- app to test modules locally: _Test App_ 
