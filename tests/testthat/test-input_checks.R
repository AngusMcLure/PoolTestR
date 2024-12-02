test_that("SimpleExampleData returns no errors or warnings", {
  expect_no_message(
    checkInputData(SimpleExampleData, "Result", "NumInPool")
  )
  expect_no_error(
    checkInputData(SimpleExampleData, "Result", "NumInPool")
  )
  expect_no_warning(
    checkInputData(SimpleExampleData, "Result", "NumInPool")
  )
})

test_that("Missing results colum returns error", {
  expect_error(
    checkInputData(SimpleExampleData, "WrongResultColumnName", "NumInPool")
  )
})

test_that("Missing poolSize column returns error", {
  expect_error(
    checkInputData(SimpleExampleData, "Result", "WrongNumInPoolColumnName")
  )
})

test_that("Character class for results column returns error", {
  char_df <- SimpleExampleData %>%
    mutate(across("Result", as.character))
  expect_error(
    checkInputData(char_df, "Result", "NumInPool")
  )
})

test_that("Character class for poolSize column returns error", {
  char_df <- SimpleExampleData %>%
    mutate(across("NumInPool", as.character))
  expect_error(
    checkInputData(char_df, "Result", "NumInPool")
  )
})

test_that("Empty rows in dataframe returns error and warning", {
  empty_row <- rep("", ncol(SimpleExampleData))
  names(empty_row) <- names(SimpleExampleData)
  empty_df <- rbind(SimpleExampleData %>%
                      mutate(across(everything(), as.character)),
                    empty_row,
                    empty_row,
                    empty_row)
  # Expect error (Result column is class "character") and warning (empty rows)
  expect_error(
    expect_warning(
      checkInputData(empty_df, "Result", "NumInPool")
    )
  )
})

test_that("NA rows in dataframe returns error and warning", {
  NA_row <- rep(NA, ncol(SimpleExampleData))
  names(NA_row) <- names(SimpleExampleData)
  NA_df <- rbind(SimpleExampleData,
                 NA_row,
                 NA_row,
                 NA_row)
  # Expect error (Results must be numeric 0 or 1 only) and warning (NA rows)
  expect_error(
    expect_warning(
      checkInputData(NA_df, "Result", "NumInPool")
    )
  )
})

test_that("NA rows in dataframe returns error and warning", {
  NA_row <- rep(NA, ncol(SimpleExampleData))
  names(NA_row) <- names(SimpleExampleData)
  NA_df <- rbind(SimpleExampleData,
                 NA_row,
                 NA_row,
                 NA_row)
  # Expect error (Results must be numeric 0 or 1 only) and warning (NA rows)
  expect_error(
    expect_warning(
      checkInputData(NA_df, "Result", "NumInPool")
    )
  )
})

test_that("Missing values in every column returns error and warning", {
  missing_df <- SimpleExampleData
  missing_df[1,1] <- ""
  missing_df[2,2] <- ""
  missing_df[3,3] <- ""
  missing_df[4,4] <- ""
  missing_df[6,6] <- ""
  # Expect error (Results must be numeric 0 or 1 only) and warning (missing values)
  expect_error(
    expect_warning(
      checkInputData(missing_df, "Result", "NumInPool")
    )
  )
})

test_that("NA values in every column returns error and warning", {
  NA_df <- SimpleExampleData
  NA_df[1,1] <- NA
  NA_df[2,2] <- NA
  NA_df[3,3] <- NA
  NA_df[4,4] <- NA
  NA_df[5,5] <- NA
  NA_df[6,6] <- NA
  # Expect error (Results must be numeric 0 or 1 only) and warning (missing values)
  expect_error(
    expect_warning(
      checkInputData(NA_df, "Result", "NumInPool")
    )
  )
})


test_that("Missing values in hierarchy columns returns warning", {
  missing_df <- SimpleExampleData
  missing_df[2,2] <- ""
  missing_df[3,3] <- ""
  missing_df[4,4] <- ""
  # Expect warning (missing values)
  expect_warning(
    checkInputData(missing_df, "Result", "NumInPool")
  )
})

test_that("NA values in hierarchy columns returns warning", {
  NA_df <- SimpleExampleData
  NA_df[2,2] <- NA
  NA_df[3,3] <- NA
  NA_df[4,4] <- NA
  NA_df[5,5] <- NA
  # Expect warning (missing values)
  expect_warning(
    checkInputData(NA_df, "Result", "NumInPool")
  )
})

test_that("hier_check = TRUE without input hierarchy columns returns error", {
 expect_error(
   checkInputData(SimpleExampleData, "Result", "NumInPool",
                  hier_check = TRUE, location = NULL)
 )
})
