#' Calculate the topic entropy
#'
#' @details 
#' Calculates the entropy over topics.
#' 
#' Assumes that of beta is set to 0 the probability 0 is the limit when p -> 0+ i.e 0.
#' 
#' @param x a \code{tidy_topic_array}.
#' @param prior a smoothing constant to use (often alpha). Not currently implemented.
#'
#' @examples 
#' data("sotu50")
#' dtm <- doc_topic_matrix(sotu50)
#' H_0 <- topic_entropy(x = dtm)
#' H_doc <- topic_entropy(x = dplyr::group_by(dtm, doc))
#' 
#' @export
topic_entropy <- function(x, prior = 0){
  checkmate::assert(is.tidy_topic_array(x))
  stopifnot(prior == 0)

  se_groups <- as.character(dplyr::groups(x))
  se_topic_groups <- c(se_groups, "topic")
  K <- length(unique(x$topic))
  
  n_topic <- dplyr::summarise(dplyr::group_by_(x, .dots = se_topic_groups), n=sum(n))
  n_sum <- dplyr::summarise(x, n_sum=sum(n))
  
  if(length(se_groups) > 0){
    entr <- dplyr::left_join(n_topic, n_sum, by = se_groups)
  } else {
    entr <- n_topic
    entr$n_sum <- n_sum[["n_sum"]]
  }
  entr <- dplyr::mutate(entr, p = n / n_sum , hd = p * log(p), p = NULL, n_sum = NULL)

  if(length(se_groups) > 0) entr <- dplyr::group_by_(entr, .dots = se_groups)
  entr <- dplyr::summarise(entr, H = - sum(hd))
  entr
}

