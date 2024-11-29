test_that("SimpleExampleData returns no errors or warnings", {
  expect_no_error(check_data(SimpleExampleData, "Result", "NumInPool"))
  expect_no_warning(check_data(SimpleExampleData, "Result", "NumInPool"))
})


