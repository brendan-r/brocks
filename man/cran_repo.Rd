% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{cran_repo}
\alias{cran_repo}
\title{Return a CRAN repo: The user-default, or RStudio's}
\usage{
cran_repo(fallback = "https://cran.rstudio.com/")
}
\value{
Either \code{fallback}, or the result of
  \code{getOption("repos")["CRAN"]}
}
\description{
Package installation on remote machines depends on a CRAN repo, which can be
tricky to set non-interactively. This simple wrapper function looks to see if
a default CRAN mirror has already been set. If it has, it is returned. If
not, \code{fallback} is returned.
}
\keyword{internal}

