context("mallet-io")

tmp_dir <- tempdir()
dir.create(tmp_dir, showWarnings = FALSE)
tmp_txt <- file.path(tmp_dir, "sotu_tmp.txt") 
tmp_gz <- file.path(tmp_dir, "sotu_tmp.txt.gz") 
data("sotu50")

test_that(desc="mallet-io",{

  state <- sotu50
  alpha_prior <- 
    c(0.29086113, 0.02444446, 0.03161273, 0.01723782, 0.07462461, 0.04539733, 
      0.09358508, 0.05666769, 0.07400546, 0.09371108, 0.01920851, 0.06307651, 
      0.06651574, 0.14376023, 0.11661917, 0.02849356, 0.03395349, 0.07415586, 
      0.04554275, 0.02449970, 0.02941570, 0.02123418, 0.11736314, 0.05787614, 
      0.12496781, 0.02267272, 0.03654363, 0.06726874, 0.06028669, 0.21446219, 
      0.05052153, 0.06216679, 0.10779823, 0.06225405, 0.16680843, 0.02384183, 
      0.03609049, 0.05058202, 0.03235920, 0.01970245, 0.05856258, 0.02690204, 
      0.12907164, 0.01989379, 0.07178854, 0.18448336, 0.02455222, 0.05687149, 
      0.06363865, 0.03725172)
  beta_prior <- 0.009949733

  expect_silent(write_mallet_statefile(state = state, alpha = alpha_prior[1], beta = beta_prior, state_file = tmp_gz))
  unlink(tmp_gz)
  expect_silent(write_mallet_statefile(state = state, alpha = alpha_prior, beta = beta_prior, state_file = tmp_gz))
  expect_silent(write_mallet_statefile(state = state, alpha = alpha_prior, beta = beta_prior, state_file = tmp_txt))
  
  expect_silent(capture_output(state2 <- read_mallet_statefile(tmp_gz)))
  expect_silent(alpha2 <- read_mallet_statefile(tmp_gz, "alpha"))
  expect_silent(beta2 <- read_mallet_statefile(tmp_gz, "beta"))
  
  expect_silent(capture_output(state3 <- read_mallet_statefile(tmp_txt)))
  expect_silent(alpha3 <- read_mallet_statefile(tmp_txt, "alpha"))
  expect_silent(beta3 <- read_mallet_statefile(tmp_txt, "beta"))
  
  expect_equal(alpha_prior, alpha2, tolerance = 0.000001)
  expect_equal(alpha_prior, alpha3, tolerance = 0.000001)
  expect_equal(beta_prior, beta2, tolerance = 0.000001)
  expect_equal(beta_prior, beta3, tolerance = 0.000001)
  
  state2$pos <- NULL
  state3$pos <- NULL
  
  expect_identical(state, state2)
  expect_identical(state, state3)
  
})

unlink(tmp_gz)
unlink(tmp_txt)
