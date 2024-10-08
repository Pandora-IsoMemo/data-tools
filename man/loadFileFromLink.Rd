% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-importData-linkToData.R
\name{loadFileFromLink}
\alias{loadFileFromLink}
\title{Load File From Link}
\usage{
loadFileFromLink(
  values = reactiveValues(warnings = list(), errors = list(), fileName = NULL,
    fileImportSuccess = NULL, dataImport = NULL, preview = NULL, data = list()),
  loadedSourceInputs,
  loadedFileInputs,
  customNames
)
}
\arguments{
\item{values}{(reactiveValues)}

\item{loadedSourceInputs}{(list) user inputs from the dataLink file specifying the source}

\item{loadedFileInputs}{(list) user inputs from the dataLink file specifying the file}

\item{customNames}{settings for custom column and row names}
}
\description{
Load a file a link points to
}
