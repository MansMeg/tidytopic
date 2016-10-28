#' Assert that \code{x} is a tidy topic state, matrix or array
#'
#' @details
#' The tidy topic state is a \code{tibble} dataset with at least the variables
#' \code{doc} (integer or factor), \code{type} (factor) and \code{topic} (integer or factor).
#'
#' @param x Object to check if it is a valid topic_model_state object
#'
#' @export
is.tidy_topic_state <- function(x){
  if(!checkmate::test_class(x, "tbl_df")) return(FALSE)
  if(!checkmate::test_subset(c("doc", "type", "topic"), names(x))) return(FALSE)
  if(!checkmate::assert(checkmate::test_class(x$doc, "integer"),
                        checkmate::test_class(x$doc, "factor"))) return(FALSE)
  if(!checkmate::test_class(x$type, "factor")) return(FALSE)
  if(!checkmate::assert(checkmate::test_class(x$doc, "integer"),
                        checkmate::test_class(x$doc, "factor"))) return(FALSE)
  TRUE
}

#' @rdname is.tidy_topic_state
#' @keywords internal
assert_state <- function(x){
  .Deprecated("is.tidy_topic_state")
  checkmate::assert(is.tidy_topic_state(x))
}


#' Assert that \code{x} is a \code{tidy_topic_matrix} or a \code{tidy_topic_array}
#'
#' @description 
#' The tidy_topic_array/tidy_topic_matrix is a \code{tibble} dataset 
#' with a \code{"topic"} variable and an integer variable \code{"n"}.
#' 
#' The \code{tidy_topic_matrix} do only contain one more variable, 
#' \code{tidy_topic_matrix} an array can contain multiple dimensions.
#' 
#' The datastructure is sparse, meaning 0 counts are not included.
#'
#' @param x Object to check if it is a valid \code{tidy_topic_matrix} 
#' or \code{tidy_topic_array}.
#'
#' @export
is.tidy_topic_array <- function(x){
  if(!checkmate::test_class(x, "tbl_df")) return(FALSE)
  if(!checkmate::test_subset(c("n", "topic"), names(x))) return(FALSE)
  if(!checkmate::test_integer(x$n, lower = 1L)) return(FALSE)
  TRUE
}

#' @rdname is.tidy_topic_array
#' @export
is.tidy_topic_matrix <- function(x){
  if(!is.tidy_topic_array(x)) return(FALSE)
  if(ncol(x) != 3) return(FALSE)
  TRUE
}

#' @rdname is.tidy_topic_state
#' @keywords internal
assert_state_object <- function(x){
  .Deprecated("is.tidy_topic_state")
  checkmate::assert(is.tidy_topic_state(x))
}
