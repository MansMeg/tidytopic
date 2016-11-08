context("mi/imi")

data("sotu50")
suppressPackageStartupMessages(library(dplyr))

test_that(desc="imi",{
  imi_test <- imi(sotu50)
  
  imi_senate11 <- filter(imi_test, type %in% c("senate"), topic == 11)
  expect_equal(imi_senate11[["imi"]], 1.019471, tolerance = .00001)
  
  imi_satisfaction10 <- filter(imi_test, type == c("satisfaction"), topic == 10)
  expect_equal(imi_satisfaction10[["imi"]], 3.423595, tolerance = .00001)
  
  w <- top_terms(sotu50, scheme = "type_probability", 3)
  expect_error(imi_test <- imi(sotu50, w))

  expect_silent(imi_test <- imi(sotu50, w = w))
  expect_equal(nrow(imi_test), nrow(w))
  
  # Check that the same order is used
  expect_equal(imi_test$type, w$type)
})

test_that(desc="grouped imi",{
  set.seed(4711)
  sotu50$group <- sample(c(1.01, 0.99, 2), size = nrow(sotu50), replace = TRUE)
  expect_error(imi_test <- imi(sotu50, "group"))
  sotu50$group <- as.factor(sotu50$group)
  expect_silent(imi_test <- imi(sotu50, "group"))
  
  imi_senate11 <- filter(imi_test, type %in% c("senate"), topic == 11)
  expect_equal(imi_senate11[["imi"]], 0.003525558, tolerance = .00001)
  
  imi_satisfaction10 <- filter(imi_test, type == c("satisfaction"), topic == 10)
  expect_equal(imi_satisfaction10[["imi"]], 0.009833588, tolerance = .00001)
  
})

test_that(desc="mi",{
  mi_test <- mi(state = sotu50)

  mi_11 <- filter(mi_test, topic == 11)
  expect_equal(mi_11[["mi"]], 3.262207, tolerance = .00001)
  
  mi_33 <- filter(mi_test, topic == 33)
  expect_equal(mi_33[["mi"]], 4.094353, tolerance = .00001)
  
})

test_that(desc="grouped mi",{
  set.seed(4711)
  sotu50$group <- sample(c(1.01, 0.99, 2), size = nrow(sotu50), replace = TRUE)
  expect_error(mi_test <- mi(sotu50, "group"))
  sotu50$group <- as.factor(sotu50$group)
  expect_silent(mi_test <- mi(sotu50, "group"))
  sotu50$group <- as.integer(sotu50$group)
  expect_silent(mi_test <- mi(sotu50, "group"))
  sotu50$group <- as.character(sotu50$group)
  expect_silent(mi_test <- mi(sotu50, "group"))
  
  mi_11 <- filter(mi_test, topic == 11)
  expect_equal(mi_11[["mi"]], 12.92542, tolerance = .00001)
  
  mi_33 <- filter(mi_test, topic == 33)
  expect_equal(mi_33[["mi"]], 5.065181, tolerance = .00001)
})

