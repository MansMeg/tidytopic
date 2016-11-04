#' Sampling along a topic model
#'
#' #@section Implementations.
#'
#' Vanilla LDA using Gibbs sampler.
#'
#' #@section Evaluation metrics.
#'
#' LogLikelihood
#'
#'
#' @author Mans Magnusson
#' @docType package
#' @name tidytopics
#' @importFrom Rcpp evalCpp
#' @useDynLib tidytopics
NULL

#' US Presidents.
#'
#' A dataset containing US presidents from 1789 to 2015
#'
#' @format A \code{\link[tibble]{tibble}} \code{data.frame} with 44 rows and 7 variables:
#' \describe{
#'   \item{no}{President number.}
#'   \item{president}{Name of the president.}
#'   \item{born}{The year the president was born.}
#'   \item{died}{The year the president died. \code{NA} if alive in 2015.
#'   \item{presidency_start}{The date the presidency started.}
#'   \item{presidency_end}{The date the presidency ended.}
#'   \item{party}{The party of the president.}
#' }
#' 
#' @source \url{https://en.wikipedia.org/wiki/State_of_the_Union}
"uspresidents"


#' State of the Union Adresses.
#'
#' A dataset containing State of the Union Adresses by paragraph from 1790 to 2009.
#'
#' @format A \code{\link[tibble]{tibble}} \code{data.frame} with 6359 rows and 3 variables:
#' \describe{
#'   \item{year}{Year of the adress.}
#'   \item{president}{President that gave the address.}
#'   \item{paragraph}{The paragraph of the address.}
#'   \item{text}{The address content.}
#' }
#' @source \url{https://en.wikipedia.org/wiki/State_of_the_Union}
"sotu"

#' Topic model of State of the Union Adresses.
#'
#' A dataset containing a topic model using 50 topics for State of the Union Adresses.
#' Stop words has been removed using the Mallet english stop word list.
#'
#' @format A \code{\link[tibble]{tibble}} \code{data.frame} with 1,263,832 rows and 3 variables:
#' \describe{
#'   \item{doc}{Document id (paragraph),}
#'   \item{type}{Type used.}
#'   \item{text}{Topic indicator.}
#' }
#' 
#' @seealso \code{\link{sotu}}
#' 
#' @source \url{https://en.wikipedia.org/wiki/State_of_the_Union}
"sotu50"



#' 2000 question posts from stats.stackexchange.com
#' 
#' @format A \code{\link[tibble]{tibble}} \code{data.frame} with 2,000 rows and 1 variables:
#' \describe{
#'   \item{text}{Textual content in post.}
#' }
#' 
#' @source \url{https://stats.stackexchange.com}
"statse"

