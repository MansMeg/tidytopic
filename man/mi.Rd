% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesian_checking.R
\name{mi}
\alias{mi}
\title{Calculate Mutual information between types and documents (or groups)}
\usage{
mi(state, g = "doc")
}
\arguments{
\item{state}{A topic model state file}

\item{g}{A character element as a name of a grouping variable in \code{state} (factor or integer). Default is \code{doc}.}
}
\description{
Function to calculate mutual information between types and documents (MI(D,W|k))
for a given topic.

\code{mi_group()} calculates MI based on a grouping variable g, used for model checking.

See reference for details.
}
\references{
Mimno, D. and Blei, D. Bayesian Checking for Topic Models
}

