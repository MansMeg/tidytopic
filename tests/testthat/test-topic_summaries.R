context("type_topic_reweighting")

load(system.file("extdata/sotu50.Rdata", package = "tidytopics"))

test_that(desc="base functions are correct",{
  K <- length(unique(sotu50$topic))
  V <- length(levels(sotu50$type))
  N <- nrow(sotu50)
  N_k <- dplyr::summarise(dplyr::group_by(sotu50, topic), n = n())
  N_w <- dplyr::summarise(dplyr::group_by(sotu50, type), n = n())
  beta <- 0.1
  ttm <- type_topic_matrix(sotu50)

  expect_equal(sum(p_wk(ttm, beta = 0)$p), 1)
  expect_silent(ps <- p_wk(ttm, beta))
  mass <- (N + V * K * beta)
  expect_equal(sum(ps$p) + ((V * K - nrow(ps)) * beta) / mass, 1)

  expect_equal(sum(p_w_given_k(ttm, beta = 0)$p), K)
  expect_silent(ps <- p_w_given_k(ttm, beta))
  p_k <- dplyr::left_join(dplyr::summarise(dplyr::group_by(ps, topic), p = sum(p), non_zero = n()), N_k, by = "topic")
  p_k <- dplyr::mutate(p_k, p_zero = (V - non_zero) * beta / (n + V * beta), mass = NULL, n = NULL, non_zero = NULL)
  expect_equal(p_k$p + p_k$p_zero, rep(1, K))

  expect_equal(sum(p_k_given_w(ttm, beta = 0)$p), length(levels(sotu50$type)))
  ps <- p_k_given_w(ttm, beta)
  p_w_test <- dplyr::left_join(dplyr::summarise(dplyr::group_by(ps, type), p = sum(p), non_zero = n()), N_w, by = "type")
  p_w_test <- dplyr::mutate(p_w_test,  p_zero = (K - non_zero) * beta / (n + K * beta), n = NULL, non_zero = NULL)
  expect_equal(p_w_test$p + p_w_test$p_zero, rep(1, V))

  expect_equal(sum(p_w(ttm, beta = 0)$p), 1)
  expect_equal(sum(p_w(ttm, beta)$p), 1)

  expect_equal(sum(p_k(ttm, beta = 0)$p), 1)
  expect_equal(sum(p_k(ttm, beta)$p), 1)

})

test_that(desc="reweighting methods",{


  
  j <- 5
  K <- length(unique(sotu50$topic))
  ttm <- type_topic_matrix(sotu50)
  
  expect_silent(tp1 <- top_terms(ttm, "type_probability", j))
  expect_silent(tp2 <- top_terms(sotu50, "type_probability", j))
  expect_identical(tp1, tp2)
  expect_equal(nrow(tp1), j * K)

  expect_silent(tp <- top_terms(ttm, "topic_probability", j))
  expect_equal(nrow(tp), j * K)

  expect_silent(tp <- top_terms(ttm, "n_wk", j))
  expect_equal(nrow(tp), j * K)

  expect_silent(tp <- top_terms(ttm, "relevance", j, lambda = 0.5))
  expect_message(tp <- top_terms(ttm, "relevance", j), regexp = "0\\.6")
  expect_equal(nrow(tp), j * K)

  expect_error(tp <- top_terms(ttm, "term_score", j))
  expect_silent(tp <- top_terms(ttm, "term_score", j, beta))
  expect_equal(nrow(tp), j * K)

})

