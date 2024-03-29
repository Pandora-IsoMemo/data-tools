% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-loadImport.R
\name{selectImportParams}
\alias{selectImportParams}
\title{Select Import params}
\usage{
selectImportParams(params, importType)
}
\arguments{
\item{params}{(list) named list of parameters required to import data or a model}

\item{importType}{(character) type of import, either "data" or "model" or "zip".
ImportType == "zip" enables the optional parameter 'expectedFileInZip'.}
}
\value{
(list) named list of parameters required for the particular importType
}
\description{
Select Import params
}
