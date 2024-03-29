% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-importData-prepareData.R
\name{updateMergeList}
\alias{updateMergeList}
\title{Update Merge List}
\usage{
updateMergeList(mergeList, fileName, newData, notifications = "")
}
\arguments{
\item{mergeList}{list of files that were submitted for data preparation}

\item{fileName}{(character) name of the file to be updated or added to the merge list}

\item{newData}{(list) data and history of the data source and the changes}

\item{notifications}{(character) previous notifications}
}
\description{
Checks if an object that should be added is already existing in mergeList. If so, the existing
 object will be replaced (updated). This is important to keep most recent changes from data
 preparation steps.
}
