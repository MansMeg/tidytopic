context("mallet-io")

state_file <- system.file("extdata/sotu_years_50_1.txt.gz", package = "tidytopics")
tmp_dir <- tempdir()
dir.create(tmp_dir)
tmp_txt <- file.path(tmp_dir, "sotu_tmp.txt") 
tmp_gz <- file.path(tmp_dir, "sotu_tmp.txt.gz") 

test_that(desc="mallet-io",{
  
  expect_silent(capture.output(state <- read_mallet_statefile(state_file)))
  expect_silent(alpha <- read_mallet_statefile(state_file, "alpha"))
  expect_silent(beta <- read_mallet_statefile(state_file, "beta"))
  
  expect_silent(write_mallet_statefile(state, alpha[1], beta, tmp_gz))
  unlink(tmp_gz)
  expect_silent(write_mallet_statefile(state, alpha, beta, tmp_gz))
  expect_silent(write_mallet_statefile(state, alpha, beta, tmp_txt))
  
  expect_silent(state2 <- read_mallet_statefile(tmp_gz))
  expect_silent(alpha2 <- read_mallet_statefile(tmp_gz, "alpha"))
  expect_silent(beta2 <- read_mallet_statefile(tmp_gz, "beta"))
  
  expect_silent(state3 <- read_mallet_statefile(tmp_txt))
  expect_silent(alpha3 <- read_mallet_statefile(tmp_txt, "alpha"))
  expect_silent(beta3 <- read_mallet_statefile(tmp_txt, "beta"))
  
  expect_equal(alpha, alpha2, tolerance = 0.000001)
  expect_equal(alpha, alpha3, tolerance = 0.000001)
  expect_equal(beta, beta2, tolerance = 0.000001)
  expect_equal(beta, beta3, tolerance = 0.000001)
  
  expect_identical(state, state2)
  expect_identical(state, state3)
})

unlink(tmp_dir, recursive = TRUE, force = TRUE)