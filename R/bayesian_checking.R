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
#' @param g groups TODO
#' @param w An ungrouped tbl_df with types and topics to calculate IMI for. Default is NULL.
#'
#'
#' @export
imi <- function(state, w=NULL){
  requireNamespace("dplyr")
  checkmate::assert(is.tidy_topic_state(state))
  checkmate::assert(checkmate::check_class(w, "tbl_df"),
                    checkmate::check_character(w, null.ok = TRUE))
  checkmate::assert(!is.grouped_df(w))

  # Calculate H(D|k)
  HDk <- state %>%
    group_by(doc, topic) %>%
    summarise(n = n()) %>%
    group_by(topic) %>%
    mutate(sum_n = sum(n), pmi = log(n/sum_n) * (n/sum_n), sum_n = NULL) %>%
    summarise(HDk = sum(pmi)) %>%
    mutate(HDk = -1 * HDk)

  # Calculate H(D|W=w, k)
  if(!is.null(w)) {
    state <- state %>%
      right_join(transmute(w, topic, type), by = c("topic", "type"))
  }

  state %>%
    group_by(doc, type, topic) %>%
    summarise(n = n()) %>%
    group_by(topic, type) %>%
    mutate(sum_n = sum(n), pmi = log(n/sum_n) * (n/sum_n), sum_n = NULL) %>%
    summarise(imi = sum(pmi)) %>%
    left_join(HDk, by = "topic") %>%
    mutate(imi = imi + HDk, HDk = NULL) %>%
    ungroup()
}

#' @rdname imi
#' @export
imi_group <- function(state, g, w = NULL){
  checkmate::assert(is.tidy_topic_state(state))
  checkmate::assert_choice(g, names(state))

  eval(parse(text=paste0("state <- dplyr::transmute(state, doc = as.integer(",g ,"), pos, type, topic)")))
  state <- dplyr::transmute(state, topic, doc, type, pos)
  imi(state, w)
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
#' @param g A character element as a name of a grouping variable in \code{state} (factor or integer).
#'
#' @export
mi <- function(state){
  requireNamespace("dplyr")
  checkmate::assert(is.tidy_topic_state(state))

  st <-
    state %>%
    group_by(doc, type, topic) %>%
    summarise(n = n()) %>%
    ungroup()

  Ndk <- st %>%
    group_by(doc, topic) %>%
    summarise(nd = n())

  Nwk <- st %>%
    group_by(type, topic) %>%
    summarise(nw = n())

  Nk <- Nwk %>%
    group_by(topic) %>%
    summarise(nk = sum(nw))

  Nwk <- left_join(Nwk, Nk, by = c("topic"))

  st %>% group_by(topic) %>%
    inner_join(Ndk, by = c("topic", "doc")) %>%
    inner_join(Nwk, by = c("topic", "type")) %>%
    mutate(part_mi = n/nk * log((n * nk)/(nd * nw))) %>%
    summarise(mi = sum(part_mi))
}

#' @rdname mi
#' @export
mi_group <- function(state, g){
  checkmate::assert(is.tidy_topic_state(state))
  checkmate::assert_choice(g, names(state))
  # FIX THIS ASSERTION
#  checkmate::assert_choice(apply(state, 2, class)[g], "factor")

  eval(parse(text=paste0("state <- dplyr::transmute(state, doc = as.integer(", g ,"), pos, type, topic)")))
  state <- dplyr::transmute(state, topic, doc, type, pos)
  mi(state)
}





#' Calculate MI deviance
#'
#' @details
#' When sampling and deviation calculation this can be done using argument state and
#' and iteration number.
#'
#' @param true a \code{tbl_df} with \code{mi} by \code{topic} based on the original data
#' @param rep a \code{tbl_df} with \code{mi} by \code{topic} based on multiple replications
#'
#' @export
mi_deviance <- function(true, rep){
  checkmate::assert_class(true, "tbl_df")
  checkmate::assert_class(rep, "tbl_df")
  checkmate::assert_subset(names(true), c("topic","mi"))
  checkmate::assert_subset(names(rep), c("topic","mi"))
  checkmate::assert_set_equal(unique(true$topic), unique(rep$topic))
  checkmate::assert(nrow(true) < nrow(rep))

  group_by(rep, topic) %>%
    summarise(mean = mean(mi), sd = stats::sd(mi)) %>%
    full_join(true, by = "topic") %>%
    transmute(deviance = (mi - mean) / sd)
}



#' Create IMI plot
#'
#' @details
#' When sampling and deviation calculation this can be done using argument state and
#' and iteration number.
#'
#' @param true a \code{tbl_df} with \code{imi}, \code{type}, \code{topic} and \code{rank} based on the original data.
#' @param rep a \code{tbl_df} with \code{imi}, \code{type} by \code{topic} based on multiple replications.
#' @param k which topic to plot
#'
#' @export
plot_imi_type <- function(true, rep, k){
  checkmate::assert_class(true, "tbl_df")
  checkmate::assert_class(rep, "tbl_df")
  checkmate::assert_subset(names(true), c("topic","type","rank","imi"))
  checkmate::assert_subset(names(rep), c("topic","type","rank","imi"))
  checkmate::assert_choice(k, unique(true$topic))
  checkmate::assert_set_equal(unique(true$topic), unique(rep$topic))
  checkmate::assert(nrow(true) < nrow(rep))

  true_k <- filter(true, topic == k)
  true_k$type <- factor(true_k$type,
                        levels = as.character(true_k$type)[order(true_k$rank, decreasing = FALSE)], ordered = TRUE)

  rep_k <- filter(rep, topic == k)
  rep_k$type <- factor(rep_k$type,
                       levels = as.character(true_k$type)[order(true_k$rank, decreasing = FALSE)], ordered = TRUE)

  p <- ggplot(rep_k, aes(type, imi)) + geom_boxplot(color = "gray", outlier.shape = NA) + coord_flip()
  p + geom_point(data = true_k, aes(type, imi), shape = 2L)
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
#'
#' @return
#' a type vector of length of the state dataset.
#'
#' @examples
#' # Load the state of the union topic model
#' load(system.file("extdata/sotu50.Rdata", package = "tidytopics"))
#' w <- sample_types_given_topic(state = sotu50)
#'
#' @export
sample_types_given_topic <- function(state){
  requireNamespace("dplyr")

  checkmate::assert(is.tidy_topic_state(state))

  Nkw <- state %>%
    dplyr::group_by(topic, type) %>%
    dplyr::summarise(n = n()) %>%
    ungroup()

  # Calculate the number of tokens by topic
  Nk <- Nkw %>%
    dplyr::group_by(topic) %>%
    dplyr::summarise(n = sum(n)) %>%
    ungroup()

  # Calculate probability and split
  pdfs <-
    Nkw %>%
    dplyr::group_by(topic) %>%
    mutate(p = n/sum(n), type = as.integer(type)) %>%
    ungroup() %>%
    transmute(topic, type, p)
  pdfs <- split(transmute(pdfs, type, p), pdfs$topic)

  w <- integer(nrow(state))
  for(i in 1:length(pdfs)){
    w[state$topic == i] <- sample(pdfs[[i]]$type, size =  Nk$n[i], TRUE, pdfs[[i]]$p)
  }
  factor(levels(state$type)[w], levels = levels(state$type))
}
