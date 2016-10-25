#' Assert that \code{x} is a tidy topic state
#'
#' @details
#' The tidy topic state is a \code{tibble} dataset with at least the variables
#' \code{doc} (integer or factor), \code{type} (factor) and \code{topic} (integer or factor).
#'
#' @param x Object to check if it is a valid topic_model_state object
#'
#' @export
is.tidy_topic_state <- function(x){
  checkmate::assert_class(x, "tbl_df")
  checkmate::assert_subset(c("doc", "type", "topic"), names(x))
  checkmate::assert(checkmate::check_class(x$doc, "integer"),
                    checkmate::check_class(x$doc, "factor"))
  checkmate::assert_class(x$type, "factor")
  checkmate::assert(checkmate::check_class(x$doc, "integer"),
                    checkmate::check_class(x$doc, "factor"))
}

#' @rdname is.tidy_topic_state
#' @keywords internal
assert_state <- function(x){
  .Deprecated("is.tidy_topic_state")
  is.tidy_topic_state(x)
}

#' @rdname is.tidy_topic_state
#' @export
is.tidy_topic_matrix <- function(x){
  warning("is.tidy_topic_matrix is not implemented yet")
  # Should check that tbl_df,
  # has "topic" and "n"
  # is sparse (no zeroes)

  TRUE
}

#' @rdname is.tidy_topic_state
#' @keywords internal
assert_state_object <- function(x){
  .Deprecated("is.tidy_topic_state")
  is.tidy_topic_state(x)
}
