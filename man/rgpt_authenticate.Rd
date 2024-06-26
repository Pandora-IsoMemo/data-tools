% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-rgpt3.R
\name{rgpt_authenticate}
\alias{rgpt_authenticate}
\title{Set up the authentication with your API key}
\usage{
rgpt_authenticate(path)
}
\arguments{
\item{path}{The file path to the API key}
}
\value{
A confirmation message
}
\description{
Access to GPT's functions requires an API key that you obtain from
[https://openai.com/api/](https://openai.com/api/). `rgpt_authenticate()`
looks for your API key in a file that you provide the path to and ensures
you can connect to the models. `rgpt_endsession()` overwrites your API key
_for this session_ (it is recommended that you run this when you are done).
`check_apikey_form()` is a simple check if any information has been provided
at all.
This function was copied from https://github.com/ben-aaron188/rgpt3.
}
\details{
The easiest way to store you API key is in a `.txt` file with _only_
the API key in it (without quotation marks or other common string
indicators). `rgpt_authenticate()` reads the single file you point it to and
retrieves the content as authentication key for all requests.
}
