% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-isomemoAPI.R
\name{handleDescription}
\alias{handleDescription}
\title{Handle Description}
\usage{
handleDescription(data, maxChar = 20)
}
\arguments{
\item{data}{(data.frame) Output from IsoMemo::getData(). A data frame containing the
requested databases, category domains, and variables of interest from the user}

\item{maxChar}{(numeric) Cut descriptions after maxChar characters for nicer display of the
data frame in Shiny}
}
\value{
A data frame containing a new column with full description kept while entries of the
 column description are cutted.
}
\description{
Handle Description
}
