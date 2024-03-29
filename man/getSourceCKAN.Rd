% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-dataSource.R
\name{getSourceCKAN}
\alias{getSourceCKAN}
\title{Get Source CKAN}
\usage{
getSourceCKAN(dataSource, input, isInternet)
}
\arguments{
\item{dataSource}{(reactiveValues) path, filename, type and input, output of \code{selectSourceServer()}}

\item{input}{(reactiveValues)}

\item{isInternet}{(logical) set TRUE, if there is an internet connection. This parameter is
ignored if \code{type = "file"} or \code{type = "remoteModel"}}
}
\description{
Get Source CKAN
}
