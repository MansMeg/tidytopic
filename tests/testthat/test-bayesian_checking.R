context("mi/imi")

data("sotu50")

test_that(desc="TODO",{
  skip_on_travis()
  expect_true(FALSE, info = "combine imi and imi_group to one function")
  expect_true(FALSE, info = "combine mi and mi_group to one function")
})

test_that(desc="imi",{
  skip_on_travis()
  imi_test <- imi(sotu50)
  expect_equal(imi_test$imi[1:2], c(0.2901117, 0.7248372), tolerance = .00001)

  w <- top_terms(sotu50, scheme = "type_probability", 3)
  w <- dplyr::group_by(w, type)
  expect_error(imi_test <- imi(sotu50, w))

  w <- ungroup(w)
  expect_silent(imi_test <- imi(sotu50, w))
  expect_equal(nrow(imi_test), nrow(w))
})

test_that(desc="group_imi",{
  skip_on_travis()
  sotu50$group <- sample(1:4, size = nrow(sotu50), replace = TRUE)
  # TODO
  expect_error(imi_test <- imi_group(sotu50, "group"))
  sotu50$group <- as.factor(sotu50$group)
  expect_silent(imi_test <- imi_group(sotu50, "group"))
})

test_that(desc="mi",{
  skip_on_travis()
  mi_test <- mi(state = sotu50)
  expect_equal(mi_test$mi[1:2], c(3.737234, 4.032572), tolerance = .00001)
})

test_that(desc="group_mi",{
  skip_on_travis()
  sotu50$group <- sample(1:4, size = nrow(sotu50), replace = TRUE)
  expect_error(mi_test <- mi_group(sotu50, "group"))
  sotu50$group <- as.factor(sotu50$group)
  expect_silent(mi_test <- mi_group(sotu50, "group"))
})

