context("is.tidy_topic_state")

load(system.file("extdata/sotu50.Rdata", package = "tidytopics"))

test_that(desc="is.tidy_topic_state",{
  state <- dplyr::mutate(sotu50, pos = NULL)
  expect_warning(assert_state(state))
  expect_warning(assert_state_object(state))
  expect_silent(is.tidy_topic_state(state))
})

context("is.tidy_topic_matrix/array")

test_that(desc="is.tidy_topic_matrix/array",{
  dtm <- doc_topic_matrix(sotu50)
  expect_silent(is.tidy_topic_array(dtm))
  expect_true(is.tidy_topic_array(dtm))
  
  expect_silent(is.tidy_topic_matrix(dtm))
  expect_true(is.tidy_topic_array(dtm))

  expect_false(is.tidy_topic_matrix(dplyr::mutate(dtm, pos = 1:nrow(dtm))))    
  expect_true(is.tidy_topic_array(dplyr::mutate(dtm, pos = 1:nrow(dtm))))    
})

