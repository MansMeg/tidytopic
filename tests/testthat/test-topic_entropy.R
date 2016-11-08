context("topic_entropy")

data("sotu50")

test_that(desc="topic_entropy",{
  dtm <- doc_topic_matrix(sotu50)
  expect_silent(H_0 <- topic_entropy(dtm))
  expect_equal(H_0[["H"]], 3.692852) # Manually calculated
  
  expect_silent(H_doc <- topic_entropy(x = dplyr::group_by(dtm, doc)))
  expect_equal(H_doc[["H"]][1], 0) # Manually calculated
  expect_equal(H_doc[["H"]][3], 0.6904678) # Manually calculated
  
})

test_that(desc="topic_probability",{
  skip_on_travis()
  expect_true(FALSE, "FIX THIS FUNCTION WITH PRIOR")
})


test_that(desc="topic_jsd",{
  skip_on_travis()
  expect_true(FALSE, "FIX THIS FUNCTION WITH PRIOR")

})