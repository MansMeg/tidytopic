#' Topic model key term summaries
#'
#' @description
#' The classical way of representing topics by order by the probability
#' for a type within a topic.
#'
#' @details
#' To save space the calculations are done using a sparse tidy format,
#' only returning values for type-topic combination that exist in the model.
#' This means that unless \code{beta} is set to 0, the returning probabilities
#' will not sum to 1.
#'
#' Not all reweighting schemes return a probability (such as KR1, KR2 and relevance)
#'
#' If ties in weight/probability, the original order is used.
#'
#' @references
#' Topic and keyword re-ranking for LDA-based topic modeling (2009)
#' LDAvis: A method for visualizing and interpreting topics (2014)
#'
#' @param state A tidy topic model state file
#' @param j The number of types to return
#' @param beta Beta hyper parameter. Default is 0 (no prior smoothing).
#' @param lambda Relevance weight. Default is 0.6.
#'
#' @return
#' Returns a data_frame with topic and top terms
#'
#' @examples
#' # Load the state of the union topic model
#' load(system.file("extdata/sotu50.Rdata", package = "tidytopics"))
#' w <- type_probability(state = sotu50, j = 5, beta = 0.01)
#'
#' @export
type_probability <- function(state, j, beta = 0){
  checkmate::assert(is.tidy_topic_state(state))
  checkmate::assert_int(j, lower = 1)
  checkmate::assert_number(beta, lower = 0)
  top_j_types(p_w_given_k(state, beta), j)
}


#' @rdname type_probability
#' @export
topic_probability <- function(state, j, beta = 0){
  checkmate::assert(is.tidy_topic_state(state))
  checkmate::assert_int(j, lower = 1)
  checkmate::assert_number(beta, lower = 0)
  top_j_types(p_k_given_w(state, beta), j)
}

#' @rdname type_probability
#' @export
KR1 <- function(state, j, beta = 0){
  library(dplyr)

  checkmate::assert(is.tidy_topic_state(state))
  checkmate::assert_int(j, lower = 1)
  checkmate::assert_number(beta, lower = 0)

  K <- length(unique(state$topic))
  phi <- p_w_given_k(state, beta)
  phi <- dplyr::left_join(phi, dplyr::summarise(dplyr::group_by(phi, type), sum_p = sum(p), n = n()), by = "type")
  phi <- dplyr::mutate(phi, sum_p = sum_p + (K - n) * beta, n = NULL, p = p/sum_p, sum_p = NULL)
  phi
}

#' @rdname type_probability
#' @export
KR2 <- function(state, j, beta = 0){
  library(dplyr)

  checkmate::assert(is.tidy_topic_state(state))
  checkmate::assert_int(j, lower = 1)
  checkmate::assert_number(beta, lower = 0)

  stop("not fixed yet")
  phi <- p_w_given_k(x)
  lphi <- log(phi)

  clphi <- matrix(colMeans(lphi), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
  res <- phi * (lphi - clphi)

  return_top_k_by_column(x = res, k)
}


#' @rdname type_probability
#' @export
relevance <- function(state, j, beta = 0, lambda = 0.6){
  library(dplyr)

  checkmate::assert(is.tidy_topic_state(state))
  checkmate::assert_int(j, lower = 1)
  checkmate::assert_number(beta, lower = 0)
  checkmate::assert_number(lambda, lower = 0, upper = 1)

  stop("not fixed yet")
  checkmate::assert_number(beta, lower = 0)
  checkmate::assert_number(lambda, lower = 0, upper = 1)
  phi <- p_w_given_k(x)
  p <- p_w(x)
  log_p_mat <- matrix(log(p), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
  res <- log(phi) - (1 - lambda) * log_p_mat
  return_top_k_by_column(x = res, k)
}



# Below are helper functions used in multiple reweighting methods

#' @title Return top j terms by weight
#' 
#' @param wttm a weighted tidy type topic matrix
#' @param j top j types to return (min 1 type)
top_j_types <- function(wtttm, j){
  checkmate::assert_class(wtttm, "tbl_df")
  checkmate::assert_integerish(j, lower = 1)
  checkmate::assert_subset(names(wtttm), c("p", "topic", "type"))
  wtttm <- dplyr::group_by(wtttm, topic)
  wtttm <- dplyr::arrange(wtttm, topic, desc(p))
  wtttm <- dplyr::mutate(wtttm, rn = row_number())
  wtttm <- dplyr::top_n(wtttm, n = j, wt = desc(rn))
  wtttm <- dplyr::mutate(wtttm, rn = NULL)
  dplyr::ungroup(wtttm)
}

p_w_given_k <- function(state, beta){
  # if(beta != 0) stop("beta != 0 is not implemented yet")
  
  V <- length(levels(state$type))
  state <- dplyr::group_by(state, topic) 
  topic_mass <- dplyr::summarise(state, weight = n())
  topic_mass <- dplyr::mutate(topic_mass, weight = weight + beta * V)

  state <- dplyr::group_by(state, topic, type) 
  state <- dplyr::summarise(state, counts = n())
  state <- dplyr::left_join(state, topic_mass, by = "topic")
  state <- dplyr::mutate(state, p = (counts + beta) / weight, weight = NULL, counts = NULL)
  state <- dplyr::ungroup(state)
  state
}

p_k_given_w <- function(state, beta){
  # if(beta != 0) stop("beta != 0 is not implemented yet")
  
  K <- length(unique(state$topic))

  type_mass <- dplyr::summarise(dplyr::group_by(state, type), weight = n())
  type_mass <- dplyr::mutate(type_mass, weight = weight + beta * K)

  state <- dplyr::group_by(state, topic, type) 
  state <- dplyr::summarise(state, counts = n())
  state <- dplyr::left_join(state, type_mass, by = "type")
  state <- dplyr::mutate(state, p = (counts + beta) / weight, weight = NULL, counts = NULL)
  state <- ungroup(state)
  state
}

p_wk <- function(state, beta){
  # if(beta != 0) stop("beta != 0 is not implemented yet")

  V <- length(levels(state$type))
  K <- length(unique(state$topic))
  total_mass <- dplyr::summarise(state, weight = n()) 
  total_mass <- dplyr::mutate(total_mass, weight = weight + beta * K * V)

  state <- dplyr::group_by(state, topic, type)
  state <- dplyr::summarise(state, counts = n())
  state <- dplyr::mutate(state, p = (counts + beta) / total_mass$weight, counts = NULL)
  state <- ungroup(state)
  state
}

p_w <- function(state, beta){
  # if(beta != 0) stop("beta != 0 is not implemented yet")
  
  V <- length(levels(state$type))
  K <- length(unique(state$topic))
  total_mass <- dplyr::summarise(state, weight = n()) 
  total_mass <- dplyr::mutate(total_mass, weight = weight + beta * K * V)
  
  state <- dplyr::summarise(dplyr::group_by(state, type), counts = n())
  state <- dplyr::mutate(state, p = (counts + beta * K) / total_mass$weight, counts = NULL)
  state
}

p_k <- function(state, beta){
  # if(beta != 0) stop("beta != 0 is not implemented yet")

  V <- length(levels(state$type))
  K <- length(unique(state$topic))
  
  total_mass <- dplyr::summarise(state, weight = n()) 
  total_mass <- dplyr::mutate(total_mass, weight = weight + beta * K * V)
  
  state <- dplyr::summarise(dplyr::group_by(state, topic), counts = n())
  state <- dplyr::mutate(state, p = (counts + beta * V) / total_mass$weight, counts = NULL)
  state
}

