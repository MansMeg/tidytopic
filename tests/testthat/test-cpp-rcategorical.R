context("rcategorical")

test_that(desc="Probabilistic test",{
  set.seed(4711)
  unnormalized_prob <- c(0.7, 1.7, 1)
  prob <- unnormalized_prob / sum(unnormalized_prob)
  count <- integer(3)
  for(i in 1:10000){
    x <- tidytopics:::rcategorical(unnormalized_prob) + 1
    count[x] <- count[x] + 1
  }
  expect_gt(chisq.test(count, p=prob)$p.value, 0.0001)
  expect_equal(unnormalized_prob, c(0.7, 1.7, 1))

  # Check that set.seed works
  set.seed(4711)
  count2 <- integer(3)
  for(i in 1:10000){
    x <- topicmodelsamplers:::rcategorical(unnormalized_prob) + 1
    count2[x] <- count2[x] + 1
  }
  expect_equal(count2, count)
})

