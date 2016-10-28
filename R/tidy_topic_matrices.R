#' Tidy topic matrices
#'
#' @description
#' Calculate a tidy topic document matrix, document topic matrix. 
#' These matrices are often very sparse.
#' These can easily be converted to a real sparse matrix using \code{as.sparseMatrix}.
#'
#' @param state a \code{tidy_topic_state} object.
#' 
#' @seealso \code{as.sparseMatrix}
#' 
#' @examples 
#' # Load the state of the union topic model
#' load(system.file("extdata/sotu50.Rdata", package = "tidytopics"))
#' dtm <- doc_topic_matrix(sotu50)
#' tdm <- topic_doc_matrix(sotu50)
#' ttm <- type_topic_matrix(sotu50)
#' ttm <- topic_type_matrix(sotu50)
#' sp_mat <- as.sparseMatrix(x = ttm)
#'
#' @export
doc_topic_matrix<-function(state){
  checkmate::assert(is.tidy_topic_state(state))
  
  tidy_topic_matrix <- dplyr::group_by(state, doc, topic) 
  tidy_topic_matrix <- dplyr::summarise(tidy_topic_matrix, n = n())
  tidy_topic_matrix <- dplyr::ungroup(tidy_topic_matrix)
  
  checkmate::assert(is.tidy_topic_matrix(tidy_topic_matrix))
  tidy_topic_matrix
}

#' @rdname doc_topic_matrix
#' @export
topic_doc_matrix<-function(state){
  doc_topic_matrix(state)[,c(2,1,3)]
}

#' @rdname doc_topic_matrix
#' @export
type_topic_matrix<-function(state){
  checkmate::assert(is.tidy_topic_state(state))
  tidy_topic_matrix <- dplyr::group_by(state, type, topic) 
  tidy_topic_matrix <- dplyr::summarise(tidy_topic_matrix, n = n())
  tidy_topic_matrix <- dplyr::ungroup(tidy_topic_matrix)
  checkmate::assert(is.tidy_topic_matrix(tidy_topic_matrix))
  tidy_topic_matrix
}

#' @rdname doc_topic_matrix
#' @export
topic_type_matrix<-function(state){
  type_topic_matrix(state)[,c(2,1,3)]
}


#' Convert a \code{tidy_topic_matrix} to a sparse matrix
#' 
#' @param x a \code{\link[=as.tidy_topic_matrix]{tidy_topic_matrix}}.
#' 
#' @description 
#' Converts a \code{\link[=as.tidy_topic_matrix]{tidy_topic_matrix}} to 
#' a sparse matrix format of \code{\link[Matrix]{sparseMatrix}} in the 
#' \code{Matrix} package.
#' 
#' @seealso \code{\link[Matrix]{sparseMatrix}}
#' 
#' @export
as.sparseMatrix <- function(x){
  checkmate::assert(is.tidy_topic_matrix(x))
  n_id <- which(names(x) == "n")
  row_id <- ifelse(n_id == 1, 2, 1)
  col_id <- ifelse(n_id == 3, 2, 3)

  dims <- c(0L, 0L)
  is_row_factor <- is.factor(x[[row_id]])
  is_col_factor <- is.factor(x[[col_id]])
  if(is_row_factor) {
    row_labels <- levels(x[[row_id]])
    dims[1] <- length(row_labels)
    x[[row_id]] <- as.integer(x[[row_id]])
  } else {
    dims[1] <- max(x[[row_id]])
    row_labels <- 1:dims[1]
  }
  if(is_col_factor) {
    col_labels <- levels(x[[col_id]])
    dims[2] <- length(col_labels)
    x[[col_id]] <- as.integer(x[[col_id]])
  } else {
    dims[2] <- max(x[,col_id])
    col_labels <- 1:dims[2]
  }

  Matrix::sparseMatrix(i = x[[row_id]], j = x[[col_id]], x = x[[n_id]], dims = dims, dimnames = list(row_labels, col_labels))
}
