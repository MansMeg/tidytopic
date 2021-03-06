% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesian_checking.R
\name{sample_tokens_given_topics}
\alias{sample_tokens_given_topics}
\title{Sample types for a given setup of topic indicators}
\usage{
sample_tokens_given_topics(state, n = 1)
}
\arguments{
\item{state}{a topic model state object}

\item{n}{The number of random draws of tokens given topic.}
}
\value{
A tidy dataset with simulated tokens.
}
\description{
The function is sampling new word types based on posterior mode without hyper parameters.

This function is used for modeling comparisons using Bayesian checking.
See reference for details.
}
\examples{
# Load the state of the union topic model
data("sotu50")
w <- sample_tokens_given_topics(state = sotu50, n = 5)

}
\references{
Mimno, D. and Blei, D. Bayesian Checking for Topic Models
}

