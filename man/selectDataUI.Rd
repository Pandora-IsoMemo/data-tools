% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-importData-selectData.R
\name{selectDataUI}
\alias{selectDataUI}
\title{Select Data UI}
\usage{
selectDataUI(id, defaultSource, batch, outputAsMatrix)
}
\arguments{
\item{id}{id of module}

\item{defaultSource}{(character) default source for input "Source", e.g. "ckan", "file", or "url"}

\item{batch}{(logical) use batch import}

\item{outputAsMatrix}{(logical) TRUE if output must be a matrix,
e.g. for batch = TRUE in Resources}
}
\description{
UI of the module
}