% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-loadModel.R
\name{extractDataFromModel}
\alias{extractDataFromModel}
\title{Extract Data From Model}
\usage{
extractDataFromModel(modelImport, rPackageName)
}
\arguments{
\item{modelImport}{(list) model object}

\item{rPackageName}{(character) name of the app}
}
\value{
(list) data object
}
\description{
Dependent on the app, the data object is stored in different places. This function extracts the
data object from the model object.
}
