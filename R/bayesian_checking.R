#' Calculate IMI
#'
#' @description 
#' Function to calculate instantanueous mutual information for types for a given topic.
#' See reference for details.
#'
#' @references
#' Mimno, D. and Blei, D. Bayesian Checking for Topic Models
#'
#' @param state a tidy_topic_state object
#' @param g Grouping variable to use to calculate IMI. Default is 'doc'.
#' @param w A \code{\link[tibble]{tibble}} with types and topics to calculate IMI for. Default is NULL, all types.
#'
#' @export
imi <- function(state, g = "doc", w=NULL){
  checkmate::assert(is.tidy_topic_state(state))
  checkmate::assert_choice(g, names(state))
  checkmate::assert(checkmate::check_class(w, "tbl_df"),
                    checkmate::check_character(w, null.ok = TRUE))
  checkmate::assert(!is.grouped_df(w))
  
  # Cleanup state file
  eval(parse(text=paste0("state <- dplyr::transmute(state, doc = as.integer(",g ,"), type, topic)")))
  state <- dplyr::transmute(state, topic, doc, type)

  # Calculate H(D|k)
  HDk <- dplyr::summarise(dplyr::group_by(state, doc, topic), n = n())
  HDk <- dplyr::group_by(HDk, topic)
  HDk <- dplyr::mutate(HDk, sum_n = sum(n), pmi = log(n/sum_n) * (n/sum_n), sum_n = NULL)
  HDk <- dplyr::summarise(HDk, HDk = sum(pmi))
  HDk <- dplyr::mutate(HDk, HDk = -1 * HDk)

  # Calculate H(D|W=w, k)
  imis <- dplyr::summarise(dplyr::group_by(state, doc, type, topic), n = n())
  imis <- dplyr::group_by(imis, type, topic)
  imis <- dplyr::mutate(imis, sum_n = sum(n), pmi = log(n/sum_n) * (n/sum_n), sum_n = NULL)
  imis <- dplyr::summarise(imis, imi = sum(pmi))
  imis <- dplyr::left_join(imis, HDk, by = "topic")
  imis <- dplyr::mutate(imis, imi = imi + HDk, HDk = NULL)
  imis <- dplyr::arrange(imis, topic, desc(imi))
  imis <- dplyr::ungroup(imis)  
  if(!is.null(w)) {
    imis <- dplyr::right_join(imis, w, by = c("topic", "type"))
  }
  imis
}

#' Calculate Mutual information between types and documents (or groups)
#'
#' @description
#' Function to calculate mutual information between types and documents (MI(D,W|k))
#' for a given topic.
#'
#' \code{mi_group()} calculates MI based on a grouping variable g, used for model checking.
#'
#' See reference for details.
#'
#' @references
#' Mimno, D. and Blei, D. Bayesian Checking for Topic Models
#'
#' @param state A topic model state file
#' @param g A character element as a name of a grouping variable in \code{state} (factor or integer). Default is \code{doc}.
#'
#' @export
mi <- function(state, g = "doc"){
  checkmate::assert(is.tidy_topic_state(state))
  checkmate::assert_choice(g, names(state))
  
  eval(parse(text=paste0("state <- dplyr::transmute(state, doc = as.integer(", g ,"), type, topic)")))
  state <- dplyr::transmute(state, topic, doc, type)
  
  state <- dplyr::group_by(state, doc, type, topic)
  state <- dplyr::summarise(state, n = n())
  state <- dplyr::ungroup(state)
  
  Ndk <- dplyr::summarise(dplyr::group_by(state, doc, topic), nd = n())

  Nwk <- dplyr::summarise(dplyr::group_by(state, type, topic), nw = n())

  Nk <- dplyr::summarise(dplyr::group_by(Nwk, topic), nk = sum(nw))
  
  Nwk <- dplyr::left_join(Nwk, Nk, by = c("topic"))
  
  state <- dplyr::group_by(state, topic)
  state <- dplyr::inner_join(state, Ndk, by = c("topic", "doc"))
  state <- dplyr::inner_join(state, Nwk, by = c("topic", "type"))
  state <- dplyr::mutate(state, part_mi = n/nk * log((n * nk)/(nd * nw)))
  state <- dplyr::summarise(state, mi = sum(part_mi))
  
  state
}

#' Calculate MI deviance
#'
#' @details
#' When sampling and deviation calculation this can be done using argument state and
#' and iteration number.
#'
#' @param observed a \code{tbl_df} with \code{mi} by \code{topic} based on the observed data.
#' @param replicated a \code{tbl_df} with \code{mi} by \code{topic} based on multiple replications.
#'
#' @export
mi_deviance <- function(observed, replicated){
  checkmate::assert_class(observed, "tbl_df")
  checkmate::assert_class(replicated, "tbl_df")
  checkmate::assert_subset(names(observed), c("topic","mi"))
  checkmate::assert_subset(names(replicated), c("topic","mi"))
  checkmate::assert_set_equal(unique(observed$topic), unique(replicated$topic))
  checkmate::assert(nrow(observed) < nrow(replicated))

  dev <- dplyr::group_by(replicated, topic)
  dev <- dplyr::summarise(dev, mi_mean = mean(mi), std = stats::sd(mi))
  dev <- dplyr::full_join(dev, observed, by = "topic")
  dev <- dplyr::transmute(dev,deviance = (mi - mi_mean) / std)
  dev
}



#' Create IMI plot
#'
#' @details
#' When sampling and deviation calculation this can be done using argument state and
#' and iteration number.
#'
#' @param observed_imi a \code{tbl_df} with \code{imi}, \code{type} and \code{topic} based on the original data.
#' @param replicated_imi a \code{tbl_df} with \code{imi}, \code{type} and \code{topic} based on multiple replications. Default is NULL, no replications used.
#' @param topic which topic to plot
#'
#' @export
ggplot_imi_type <- function(observed_imi, topic, replicated_imi = NULL){
  checkmate::assert_class(observed_imi, "tbl_df")
  checkmate::assert_subset(c("topic","type","imi"), names(observed_imi))
  checkmate::assert_choice(topic, unique(observed_imi$topic))
  
  if(!is.null(replicated_imi)){
    checkmate::assert_class(replicated_imi, "tbl_df")
    checkmate::assert_subset(c("topic","type","imi"), names(replicated_imi))
    checkmate::assert_set_equal(unique(observed_imi$topic), unique(replicated_imi$topic))
    checkmate::assert(nrow(observed_imi) <= nrow(replicated_imi))
  }
  k <- topic
  observed_k <- filter(observed_imi, topic == k)
  observed_k$type <- factor(observed_k$type,
                            levels = rev(as.character(observed_k$type)))

  if(!is.null(replicated_imi)){
    replicated_k <- filter(replicated_imi, topic == k)
    replicated_k$type <- factor(replicated_k$type,
                                levels = rev(as.character(observed_k$type)))
  }
  
  p <- ggplot(data = observed_k, aes(x = type, y = imi)) 
  if(!is.null(replicated_imi)){
    p <- p + geom_boxplot(data = replicated_k, aes(x = type, y = imi), color = "gray", outlier.shape = NA) + coord_flip()
  }
  p <- p + geom_point(shape = 2L) + coord_flip()
  p + coord_flip()
}
#' Sample types for a given setup of topic indicators
#'
#' @description
#' The function is sampling new word types based on posterior mode without hyper parameters.
#'
#' This function is used for modeling comparisons using Bayesian checking.
#' See reference for details.
#'
#' @references
#' Mimno, D. and Blei, D. Bayesian Checking for Topic Models
#'
#' @param state a topic model state object
#' @param n The number of random draws of tokens given topic.
#'
#' @return
#' A tidy dataset with simulated tokens.
#'
#' @examples
#' # Load the state of the union topic model
#' data("sotu50")
#' w <- sample_tokens_given_topics(state = sotu50, n = 5)
#'
#' @export
sample_tokens_given_topics <- function(state, n = 1){
  checkmate::assert(is.tidy_topic_state(state))
  checkmate::assert_number(n, lower = 1)

  Nkw <- topic_type_matrix(state)
  
  # Calculate the number of tokens by topic
  Nk <- dplyr::group_by(state, topic)
  Nk <- dplyr::summarise(Nk, n = n())
  Nk <- dplyr::ungroup(Nk)
  
  # Calculate probability and split
  pdfs <- split(dplyr::transmute(Nkw, type = as.integer(type), n), Nkw$topic)
  
  w <- dplyr::bind_rows(lapply(1L:n, function(x) {
    state$sample <- x
    state
    }))
  w <- dplyr::mutate(w, type = as.integer(type))

  for(i in 1:nrow(Nk)){
    w[["type"]][state$topic == i] <- 
      sample(pdfs[[i]]$type, size =  Nk$n[i] * n, replace = TRUE, prob = pdfs[[i]]$n)
  }
  
  w[["type"]] <- factor(levels(state$type)[w[["type"]]], levels = levels(state$type))
  w
}
