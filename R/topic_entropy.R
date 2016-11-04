#' Calculate the Jensen-Shannon Distance between groups
#'
#' @details 
#' Calculates the Jensen-Shannon distance for a grouped variable.
#' 
#' @param x a possibly grouped \code{tidy_topic_array}.
#' @param dist a categorical variable to calculate JSD between group 
#' @param prior probability smoothing/dirichlet prior. Default is 0.
#' 
#' @details 
#' If prior is 0, 0 * log(0) is assumed to be 0 (ie the limit).
#' Assumes that of beta is set to 0 the probability 0 is the limit when p -> 0+ i.e 0.
#'
#' @export
topic_jsd <- function(x, dist, prior = 0){
  checkmate::assert_choice(dist, choices = names(x))
  stopifnot(prior == 0) # Not implemented for other smoothing parameters yet
  
  se_groups <- as.character(dplyr::groups(x))
  se_dist_groups <- c(se_groups, dist)
  
  tpr <- topic_probability(x = dplyr::group_by_(x,.dots = se_dist_groups), prior = 0)
  
  tpr <- dplyr::full_join(tpr, tpr, by = c(se_groups, "topic"))
  tpr <- dplyr::group_by_(tpr, .dots = c(se_groups, "topic"))
  distxy <- unlist(lapply(dist, FUN= function(x) paste0(x, c(".x", ".y"))))
  tpr <- dplyr::filter_(tpr, .dots = paste(distxy, collapse = " != "))
  
  # Calculate Jensen-Shannon Distance
  tpr <- mutate(tpr, 
                p_mean = (p.x + p.y) / 2,
                kl_x_mean = p.x * log(p.x / p_mean), 
                kl_y_mean = p.y * log(p.y / p_mean))
  tpr$kl_x_mean[is.nan(tpr$kl_x_mean)] <- 0
  tpr$kl_y_mean[is.nan(tpr$kl_y_mean)] <- 0
  
  # JSD
  sum_groups <- c(se_groups, distxy)
  tpr <- dplyr::group_by_(tpr, .dots = sum_groups)
  tpr <- dplyr::summarise(tpr,
                          kl_x_mean = sum(kl_x_mean),
                          kl_y_mean = sum(kl_y_mean))
  tpr <- dplyr::mutate(tpr,
                       jsd = 0.5 * kl_x_mean + 0.5 * kl_y_mean, 
                       kl_y_mean = NULL, kl_x_mean = NULL)
  tpr  
}

#' Calculates the probability over topics
#'
#' @param x a possibly grouped \code{tidy_topic_array}.
#' @param prior probability smoothing/dirichlet prior. Default is NULL, no smoothing and sparse results.
#' 
topic_probability <- function(x, prior = NULL){
  checkmate::assert(is.tidy_topic_array(x))
  checkmate::assert(is.null(prior) || prior == 0) # Temporary limit
  checkmate::assert_number(prior, lower = 0, null.ok = TRUE)
  
  se_groups <- as.character(dplyr::groups(x))
  se_topic_groups <- c(se_groups, "topic")
  
  n_topic <- dplyr::summarise(dplyr::group_by_(x, .dots = se_topic_groups), n=sum(n))
  if(!is.null(prior)){
    K <- length(unique(n_topic$topic))
    eval(parse(text=paste0("n_topic <- tidyr::complete(n_topic, tidyr::nesting(", paste(se_groups, collapse = ","),"), topic=1:K, fill = list(n = 0))")))
  }
  n_sum <- dplyr::summarise(dplyr::group_by_(n_topic, .dots = se_groups), 
                            n_sum=sum(n))
  n_topic <- dplyr::left_join(n_topic, n_sum, by = se_groups)
  n_topic <- dplyr::mutate(n_topic, p = n/n_sum, n = NULL, n_sum = NULL)
  
  n_topic
}


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

