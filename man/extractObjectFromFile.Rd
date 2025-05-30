% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-loadModel.R
\name{extractObjectFromFile}
\alias{extractObjectFromFile}
\title{Extract Object From File}
\usage{
extractObjectFromFile(pathToUnzipped, what = NULL)
}
\arguments{
\item{pathToUnzipped}{(character) path to the folder were the model was unzipped}

\item{what}{(character) DEPRECATED. Argument will be ignored! what should be extracted, one of "model" or "inputs"}
}
\value{
(list) model object
}
\description{
Extract the inputs and data objects from from either a "inputs.rds" file or from a "model.Rdata" file.
Extracts the model object from either a "model.rds" file or from a "model.Rdata" file.
Recent model objects are only stored as "model.rds".
}
