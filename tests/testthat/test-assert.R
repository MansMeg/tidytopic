context("is.tidy_topic_state")

load(system.file("extdata/sotu50.Rdata", package = "tidytopics"))

test_that(desc="imi",{
  state <- dplyr::mutate(sotu50, pos = NULL)
  expect_warning(assert_state(state))
  expect_warning(assert_state_object(state))
  expect_silent(is.tidy_topic_state(state))
})
