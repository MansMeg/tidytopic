#' Weighted top terms
#'
#' @description
#' There are multiple ways to reweight topics. The following ones are implemented:
#' 
#' \describe{
#'   \item{\code{type_probability}}{\eqn{p(w|k)} The probability of a type given the topic. The most common weighting scheme.}
#'   \item{\code{topic_probability}}{\eqn{p(k|w)} The probability of a topic given a term.}
#'   \item{\code{term_score}}{\eqn{p(w|k) * log(p(w|k) / (\prod p_k(w|k))^(1/K)} A weighting scheme inspired by tf-idf proposed by Lafferty and Blei (2009).}
#'   \item{\code{relevance}}{\eqn{log(p(w|k)/ (\sum p_k(w)^(1-\lambda))} A weighting scheme proposed by Sievert and Shirley (2014)}
#'   \item{\code{n_wk}}{\eqn{n_wk} Order by number of topic indicators. Give same result as \code{type_probability} but is faster.}
#' }
#'
#' @details
#' Only returning values for type-topic combination that exist in the model is returned.
#' This means that unless \code{beta} is set to 0, the returning probabilities
#' will not sum to 1.
#' 
#' If ties in weight/probability, the original order is returned.
#' 
#' \code{relevance} weighting uses the additional parameter \code{lambda}. Default is 0.6.
#' 
#' @references
#' Blei, D. M., & Lafferty, J. D. (2009). Topic models. Text mining: classification, clustering, and applications, 10(71), 34.
#' 
#' Sievert, C., & Shirley, K. E. (2014). LDAvis: A method for visualizing and interpreting topics. In Proceedings of the workshop on interactive language learning, visualization, and interfaces (pp. 63-70).
#'
#' @param x A \code{tidy_topic_state}
#' @param j The number of types to return. Default is 10.
#' @param scheme The weight scheme to use. Default is \code{type_probability}.
#' @param beta Beta hyper parameter. Default is 0 (no prior smoothing).
#' @param ... additional parameters used by weighting schemes. See details.
#'
#' @return
#' Returns a \code{\link[tibble]{tibble}} with topic and top terms and weights
#'
#' @examples
#' # Load the state of "the State of the Union" topic model
#' load(system.file("extdata/sotu50.Rdata", package = "tidytopics"))
#' w <- top_terms(x = sotu50, "n_wk")
#' w <- top_terms(x = sotu50, beta = 0.01)
#' 
#' @export
top_terms <- function(x, scheme = "type_probability", j = 10, beta = 0, ...){
  checkmate::assert(is.tidy_topic_state(x), is.tidy_topic_matrix(x))
  if(is.tidy_topic_matrix(x)) checkmate::assert_subset("type", names(x))
  checkmate::assert_int(j, lower = 1)
  checkmate::assert_number(beta, lower = 0)
  checkmate::assert_choice(scheme, c("n_wk", "type_probability", "topic_probability", "term_score", "relevance"))
  
  if(is.tidy_topic_state(x)) x <- type_topic_matrix(x)
  class(x) <- c(scheme, class(x))
  x <- term_weight(x, beta, ...)
  x <- top_j_types(x, j)
  names(x)[which(names(x) == "p")] <- scheme
  x
}

#' @keywords internal
term_weight <- function(x, beta, ...){
  UseMethod("term_weight", x)
}

#' @keywords internal
term_weight.type_probability <- function(x, beta, ...){
  p_w_given_k(x, beta)
}

#' @keywords internal
term_weight.n_wk <- function(x, beta, ...){
  names(x)[which(names(x) == "n")] <- "p"
  x
}


#' @keywords internal
term_weight.type_probability <- function(x, beta, ...){
  p_w_given_k(x, beta)
}


#' @keywords internal
term_weight.topic_probability <- function(x, beta, ...){
  p_k_given_w(x, beta)
}

#' @keywords internal
term_weight.term_score <- function(x, beta, ...){
  checkmate::assert(beta > 0, .var.name = "beta > 0")

  K <- length(unique(x$topic))
  phi <- p_w_given_k(x, beta)
  phi <- dplyr::mutate(phi, lp = log(p))
  lbeta <- log(beta)
  sum_phi <- dplyr::group_by(phi, type)
  sum_phi <- dplyr::summarise(sum_phi, sum_lp = sum(lp), n = n())
  sum_phi <- dplyr::mutate(sum_phi, sum_lp = sum_lp + (K - n) * lbeta, mean_lp = sum_lp / K, n = NULL, sum_lp = NULL)
  
  phi <- dplyr::left_join(phi, sum_phi, by = "type")
  phi <- dplyr::mutate(phi, p = p * (lp - mean_lp), lp = NULL, mean_lp = NULL)
  phi
}


#' @keywords internal
term_weight.relevance <- function(x, beta, ...){
  param <- list(...)
  if(is.null(param$lambda)) {
    message("lambda is set to 0.6")
    param$lambda <- 0.6
  }
  lambda <- param$lambda 
  checkmate::assert_number(lambda, lower = 0, upper = 1)

  phi <- p_w_given_k(x, beta)
  pw <- p_w(x, beta)
  pw <- dplyr::mutate(pw, lpwl = log(p) * (1-param$lambda), p = NULL)
  phi <- dplyr::left_join(phi, pw, by = "type")
  phi <- dplyr::mutate(phi, p = log(p) - lpwl)
  phi
}



# Below are helper functions used in multiple reweighting methods

#' Return top j terms by weight
#' 
#' @param x a weighted tidy type topic matrix
#' @param j top j types to return (min 1 type)
#' @keywords internal
top_j_types <- function(x, j){
  x <- dplyr::group_by(x, topic)
  x <- dplyr::arrange(x, topic, desc(p))
  x <- dplyr::mutate(x, rn = row_number())
  x <- dplyr::top_n(x, n = j, wt = desc(rn))
  x <- dplyr::mutate(x, rn = NULL)
  dplyr::ungroup(x)
}

p_w_given_k <- function(x, beta){
  # if(beta != 0) stop("beta != 0 is not implemented yet")
  
  V <- length(levels(x$type))
  x <- dplyr::group_by(x, topic) 
  topic_mass <- dplyr::summarise(x, weight = sum(n))
  topic_mass <- dplyr::mutate(topic_mass, weight = weight + beta * V)

  x <- dplyr::left_join(x, topic_mass, by = "topic")
  x <- dplyr::mutate(x, p = (n + beta) / weight, weight = NULL, n = NULL)
  x <- dplyr::ungroup(x)
  x
}

p_k_given_w <- function(x, beta){

  K <- length(unique(x$topic))

  type_mass <- dplyr::summarise(dplyr::group_by(x, type), weight = sum(n))
  type_mass <- dplyr::mutate(type_mass, weight = weight + beta * K)

  x <- dplyr::left_join(x, type_mass, by = "type")
  x <- dplyr::mutate(x, p = (n + beta) / weight, weight = NULL, n = NULL)
  x <- dplyr::ungroup(x)
  x
}

p_wk <- function(x, beta){

  V <- length(levels(x$type))
  K <- length(unique(x$topic))
  total_mass <- dplyr::summarise(x, weight = sum(n)) 
  total_mass <- dplyr::mutate(total_mass, weight = weight + beta * K * V)

  x <- dplyr::mutate(x, p = (n + beta) / total_mass$weight, n = NULL)
  x <- ungroup(x)
  x
}

p_w <- function(x, beta){
  
  V <- length(levels(x$type))
  K <- length(unique(x$topic))
  total_mass <- dplyr::summarise(x, weight = n()) 
  total_mass <- dplyr::mutate(total_mass, weight = weight + beta * K * V)
  
  x <- dplyr::summarise(dplyr::group_by(x, type), counts = n())
  x <- dplyr::mutate(x, p = (counts + beta * K) / total_mass$weight, counts = NULL)
  x
}

p_k <- function(x, beta){

  V <- length(levels(x$type))
  K <- length(unique(x$topic))
  
  total_mass <- dplyr::summarise(x, weight = sum(n)) 
  total_mass <- dplyr::mutate(total_mass, weight = weight + beta * K * V)
  
  x <- dplyr::summarise(dplyr::group_by(x, topic), n = sum(n))
  x <- dplyr::mutate(x, p = (n + beta * V) / total_mass$weight, n = NULL)
  x
}

