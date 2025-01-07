test_that("PoolPrev does not return an error for large pool sizes", {
  # Previously, for pools larger than ~300, with a mix of positive and negative
  # pools, there would be an error 'Error: Initialization failed from the
  # optimize routine to determine the MLE
  
  min_error_data <- data.frame(Result = c(0,1), NumInPool = c(300,300))
  
  

  prev <- PoolPrev(min_error_data, "Result", "NumInPool", bayesian = TRUE)
  expect_named(prev, 
               expected = c("PrevMLE", "CILow", "CIHigh",
                            "PrevBayes", "CrILow", "CrIHigh",
                            "ProbAbsent", "NumberOfPools", "NumberPositive"),
               ignore.order = TRUE)
  
  #For MLE the 'expected' should be the exact value (besides small numerical error)
  expect_equal(prev$PrevMLE[[1]] * 1e3,   (1-0.5^(1/300)) * 1e3, tolerance = 1e-5)
  #For Bayesian point estimate, 'expected' is only approximately what one should
  #get, due to RNG and the true values being somewhat different. Therefore we
  #use much higher tolerance
  expect_equal(prev$PrevBayes[[1]] * 1e3, (1-0.5^(1/300)) * 1e3, tolerance = 1e-1)
})


test_that("PoolPrev returns no errors/warnings/messages for SimpleExampleData", {
  expect_no_message(
    expect_no_warning(
      expect_no_error(
        PoolPrev(
          data = SimpleExampleData, 
          result = "Result", 
          poolSize = "NumInPool", 
          bayesian = FALSE
        )
      )
    )
  )
  expect_no_message(
    expect_no_warning(
      expect_no_error(
        PoolPrev(
          data = SimpleExampleData, 
          result = "Result", 
          poolSize = "NumInPool", 
          "Region",
          bayesian = FALSE)
      )
    )
  )
})


test_that("PoolPrev returns correct ML point estimate", {
  # Mean prevalence for 100 runs was 0.05722 (4 SF)
  # Range in prevalence for 100 runs was 0.05722 - 0.05723 (4 SF)
  prev <- PoolPrev(SimpleExampleData, "Result", "NumInPool", bayesian = FALSE)
  expect_named(prev,
               expected = c("PrevMLE", "CILow", "CIHigh",
                            "NumberOfPools", "NumberPositive"),
               ignore.order = TRUE)
  expect_equal(round(prev$PrevMLE[[1]], digits = 4), 0.0572)
  expect_lte(round(prev$PrevMLE[[1]], digits = 4), 0.0573)
  expect_gte(round(prev$PrevMLE[[1]], digits = 4), 0.0572)
})

test_that("PoolPrev returns correct ML and Bayesian estimates", {
  skip_on_cran()
  # Time consuming to run Bayesian analysis
  prev <- PoolPrev(SimpleExampleData, "Result", "NumInPool", 
                   bayesian = TRUE)
  # PrevMLE:   mean = 0.05722, range = (0.05722 - 0.05723) - (20 replicates)
  # Differences should be minor - fix to 4 or 5 d.p.
  # PrevBayes: mean = 0.05726, range = (0.05704 - 0.05755) - (20 replicates)
  # Allow values within 5x the range for PrevBayes
  expect_named(prev, 
               expected = c("PrevMLE", "CILow", "CIHigh",
                            "PrevBayes", "CrILow", "CrIHigh",
                            "ProbAbsent", "NumberOfPools", "NumberPositive"),
               ignore.order = TRUE)
  expect_equal(round(prev$PrevMLE[[1]], digits = 4), 0.0572)
  expect_lte(round(prev$PrevMLE[[1]], digits = 4), 0.0573)
  expect_gte(round(prev$PrevMLE[[1]], digits = 4), 0.0572)
  expect_equal(round(prev$PrevBayes[[1]], digits = 3), 0.057)
  expect_lte(round(prev$PrevBayes[[1]], digits = 4), 0.0576)
  expect_gte(round(prev$PrevBayes[[1]], digits = 4), 0.0572)
})


