#' Tidy topic matrices
#'
#' @description
#' Calculate a tidy topic matrix. These are often very sparse.
#' These can easily be converted to a sparse matrix using \code{as.matrix()}.
#'
#' @aliases tidy_topic_matrix
#'
#' @param state a \code{tidy_topic_state} object.
#' @param x a \code{tidy_topic_matrix} object.
#'
#' @export
doc_topic_matrix<-function(state){
  checkmate::assert(is.tidy_topic_state(state))
  tidy_topic_matrix <- group_by(state, doc, topic) %>% summarise(n = n()) %>% ungroup()
  class(tidy_topic_matrix) <- c("tidy_topic_matrix", class(tidy_topic_matrix))
  tidy_topic_matrix
}

#' @rdname doc_topic_matrix
#' @export
topic_doc_matrix<-function(state){
  checkmate::assert(is.tidy_topic_state(state))
  tidy_topic_matrix <- group_by(state, topic, doc) %>% summarise(n = n()) %>% ungroup()
  class(tidy_topic_matrix) <- c("tidy_topic_matrix", class(tidy_topic_matrix))
  tidy_topic_matrix
}

#' @rdname doc_topic_matrix
#' @export
type_topic_matrix<-function(state){
  checkmate::assert(is.tidy_topic_state(state))
  tidy_topic_matrix <- group_by(state, type, topic) %>% summarise(n = n()) %>% ungroup()
  class(tidy_topic_matrix) <- c("tidy_topic_matrix", class(tidy_topic_matrix))
  tidy_topic_matrix
}

#' @rdname doc_topic_matrix
#' @export
topic_type_matrix<-function(state){
  checkmate::assert(is.tidy_topic_state(state))
  tidy_topic_matrix <- group_by(state, topic, type) %>% summarise(n = n()) %>% ungroup()
  class(tidy_topic_matrix) <- c("tidy_topic_matrix", class(tidy_topic_matrix))
  tidy_topic_matrix
}

#' @rdname doc_topic_matrix
#' @export
doc_topic_matrix<-function(state){
  checkmate::assert(is.tidy_topic_state(state))
  tidy_topic_matrix <- group_by(state, doc, topic) %>% summarise(n = n()) %>% ungroup()
  class(tidy_topic_matrix) <- c("tidy_topic_matrix", class(tidy_topic_matrix))
  tidy_topic_matrix
}

#' @rdname doc_topic_matrix
#' @export
as.matrix.tidy_topic_matrix <- function(x){
  dims <- c(0L, 0L)
  is_row_factor <- is.factor(x[,1])
  is_col_factor <- is.factor(x[,2])
  if(is_row_factor) {
    row_labels <- levels(x[,1])
    dims[1] <- length(row_labels)
    x[,1] <- as.integer(x[,1])
  } else {
    dims[1] <- max(x[,1])
    row_labels <- 1:dims[1]
  }
  if(is_col_factor) {
    col_labels <- levels(x[,2])
    dims[2] <- length(col_labels)
    x[,2] <- as.integer(x[,2])
  } else {
    dims[2] <- max(x[,2])
    col_labels <- 1:dims[2]
  }

  Matrix::sparseMatrix(i = x[[1]], j = x[[2]], x = x[[3]], dims = dims, dimnames = list(row_labels, col_labels))
}
