test_that("HierPoolPrev() raises no errors", {
  ## TODO - fix this test so it passes when there are errors/warnings raised by
  ##        the STAN model
  ## Output:
  # Expected `expect_no_warning(...)` to run without any errors.
  # i Actually got a <expectation_failure> with text:
  #     Expected `HierPoolPrev(...)` to run without any warnings.
  # i Actually got a <simpleWarning> with text:
  #   There were 8 divergent transitions after warmup. 
  #   See https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
  #   to find out why this is a problem and how to eliminate them.
  skip_on_cran()
  ShortExampleData <- 
    SimpleExampleData %>%
    filter(.data$NumInPool == 10) %>%
    filter(.data$Year == 0) %>%
    filter(.data$Region %in% c("A", "B")) %>%
    filter(.data$Village %in% c("A-1", "A-2", "B-1", "B-2"))
  expect_no_message(
    expect_no_error(
      expect_no_warning(
        HierPoolPrev(data = ShortExampleData, 
                     result = "Result", poolSize = "NumInPool",
                     hierarchy = c("Village", "Site"),
                     cores = 4,
                     verbose = T)
      )
    )
  )
})
