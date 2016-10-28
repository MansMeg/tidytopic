context("topic_entropy")

load(system.file("extdata/sotu50.Rdata", package = "tidytopics"))

test_that(desc="topic_entropy",{

  dtm <- doc_topic_matrix(sotu50)
  expect_silent(H_0 <- topic_entropy(dtm))
  expect_silent(H_doc <- topic_entropy(x = dplyr::group_by(dtm, doc)))
  expect_equal(H_doc[["H"]][1], 0.4505612)

})
