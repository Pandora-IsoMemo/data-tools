% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-loadModel.R
\name{loadModelWrapper}
\alias{loadModelWrapper}
\title{Load Model Wrapper}
\usage{
loadModelWrapper(
  values,
  filepath,
  filename,
  subFolder,
  rPackageName,
  onlySettings,
  fileExtension = "zip",
  ...
)
}
\arguments{
\item{values}{(list) list with import specifications}

\item{filepath}{(character) path to the model file}

\item{filename}{(character) name of the model file}

\item{subFolder}{(character) (optional) subfolder containing loadable .zip files}

\item{rPackageName}{(character) name of the package (as in the Description file) in which this
module is called. If not NULL, than the uploaded file must be a downloaded file
from the R package where \code{importDataServer} was called. This parameter is ignored if
\code{importType == "data"}.}

\item{onlySettings}{(logical) if TRUE allow only upload of user inputs and user data}

\item{fileExtension}{(character) (optional) app specific file extension, e.g. "resources",
"bpred", "bmsc"}

\item{...}{parameters for other wrappers}
}
\description{
Wrapper function to load a model from a zip file. File extension may differ from app to app, but
in essence all models are stored in a zip file. The zip file contains a model object, a data
object, an inputs object and potentially help files.
This function unzips the file, extracts the model object, and checks if the model is valid for
the app.
It returns a list with the model object, the data object, the inputs object and gives a message
if the model was successfully loaded.
}
