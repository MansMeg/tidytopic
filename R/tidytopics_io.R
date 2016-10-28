#' Read a Mallet state file into R
#' 
#' @description 
#' This function reads a mallet file into R. the purpose is to enable the full strenght of R to visualize and postprocess sampled data from mallet
#' 
#' @param state_file Path to a mallet state file with \code{.gz} or \code{.txt} suffix.
#' @param type What to return from the state file. Can be \code{state}, \code{alpha} or \code{beta}. Default is \code{state}.
#' 
#' @return 
#' The return value depends on \code{type}. 
#' \code{state} returns the topic indicators, word types and document id in \code{tbl_df} format.
#' \code{alpha} returns a vector of alpha priors values (in character format).
#' \code{beta} returns the beta prior (in character format).
#' 
#' @export
read_mallet_statefile <-function(state_file, type = "state"){
  checkmate::assert_file_exists(state_file)
  checkmate::assert_choice(type, c("state", "alpha", "beta"))
  assert_mallet_state_file_name(state_file)
  
  # Create connection  
  file_suffix <- stringr::str_extract(state_file, "\\.[a-z]+$")
  if(file_suffix == ".gz"){
    connection <- gzcon(file(state_file, "rb"))
  } 
  if(file_suffix == ".txt"){
    connection <- file(state_file, "r")
  }
  
  # Assert file header
  head <- readLines(connection, n = 1)  
  checkmate::assert_character(head, pattern = "#doc source pos typeindex type topic")
  head <- stringr::str_split(head, " |#")[[1]][-1]
  
  # Read alpha
  alpha <- stringr::str_split(stringr::str_trim(readLines(connection, n = 1)), " ")[[1]]
  alpha <- as.numeric(alpha[3:length(alpha)])
  if(type == "alpha") {close(connection); return(alpha)}

  # Read beta
  beta <- stringr::str_split(stringr::str_trim(readLines(connection, n = 1)), " ")[[1]]
  beta <- as.numeric(beta[3])
  if(type == "beta") {close(connection); return(beta)}
  close(connection)
  
  # Read in mallet state file
  st <- suppressMessages(readr::read_delim(file = state_file, delim = " ", skip = 3, col_names = head))
  st <- dplyr::mutate(st, source = NULL)
  st <- dplyr::mutate(st, type = factor(st$type, levels = as.character(unique(st$type))))
  st <- dplyr::mutate(st, doc = doc + 1L, pos = pos + 1L, topic = topic + 1L)
  st <- dplyr::mutate(st, typeindex = NULL)
  
  st
}



#' Write a Mallet state from R
#' 
#' @param state a \code{tidy_topic_state} object in a one-token-per-row format (see \code{tidytext}).
#' @param alpha a numeric vector of length equal one (uniform prior) or to the number of topics. This is the alpha prior for each topic. 
#' @param beta A numeric element with the beta prior (single value).
#' @param state_file a character string naming a file.
#' 
#' @return 
#' \code{NULL} if file was written successfully.
#' 
#' @export
write_mallet_statefile <-function(state, alpha, beta, state_file){
  # Assert output file
  checkmate::assert_path_for_output(state_file)
  assert_mallet_state_file_name(state_file)
  
  # Assert state object
  checkmate::assert(is.tidy_topic_state(state))
  K <- length(unique(state$topic))
  
  # Assert alpha prior
  checkmate::assert(checkmate::check_numeric(alpha, lower = 0, len = 1),
                    checkmate::check_numeric(alpha, lower = 0, len = K))
  if(length(alpha) == 1) alpha <- rep(alpha, K) 
    
  # Assert beta prior
  checkmate::assert_number(beta, lower = 0)

  # Create connection
  file_suffix <- stringr::str_extract(state_file, "\\.[a-z]+$")
  if(file_suffix == ".gz"){
    connection <- gzfile(state_file, "ab")
  } 
  if(file_suffix == ".txt"){
    connection <- file(state_file, "a")
  }
  
  # Compute pos
  state <- dplyr::group_by(state, doc)
  state <- dplyr::mutate(state, pos = row_number())
  state <- dplyr::ungroup(state)
  
  cat("#doc source pos typeindex type topic", file = connection, sep = "\n")
  cat(paste("#alpha :", paste(alpha, collapse = " "), ""), file = connection, sep = "\n")
  cat(paste("#beta :", beta), file = connection, sep = "\n")
  cat(paste(state$doc - 1L, 
            rep(NA, nrow(state)),
            state$pos - 1L,
            as.integer(state$type) - 1L,
            as.character(state$type), 
            state$topic - 1L),
      file = connection, sep = "\n")
  close(connection)
  NULL
}

#' Assert that \code{file_path} is a valide mallet state file
#' 
#' @param file_path File path to check
#' 
#' @keywords internal
assert_mallet_state_file_name <- function(file_path){
  checkmate::assert_character(file_path, len = 1)
  file_suffix <- stringr::str_extract(file_path, "\\.[a-z]+$")
  checkmate::assert_subset(file_suffix, c(".gz", ".txt"))
}
