test_that("SimpleExampleData returns no errors or warnings", {
  expect_no_message(
    CheckInputData(SimpleExampleData, "Result", "NumInPool")
  )
  expect_no_error(
    CheckInputData(SimpleExampleData, "Result", "NumInPool")
  )
  expect_no_warning(
    CheckInputData(SimpleExampleData, "Result", "NumInPool")
  )
})


test_that("Missing results column returns error", {
  expect_error(
    CheckInputData(SimpleExampleData, "WrongResultColumnName", "NumInPool"),
    class = "DataCheck_missing_column"
  )
})


test_that("Missing poolSize column returns error", {
  expect_error(
    CheckInputData(SimpleExampleData, "Result", "WrongNumInPoolColumnName"),
    class = "DataCheck_missing_column"
  )
})


test_that("Character class for results column returns error", {
  char_df <- SimpleExampleData %>%
    mutate(across("Result", as.character))
  expect_error(
    CheckInputData(char_df, "Result", "NumInPool"),
    class = "DataCheck_col_not_numeric"
  )
})


test_that("Character class for poolSize column returns error", {
  char_df <- SimpleExampleData %>%
    mutate(across("NumInPool", as.character))
  expect_error(
    CheckInputData(char_df, "Result", "NumInPool"),
    class = "DataCheck_col_not_numeric"
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
      CheckInputData(empty_df, "Result", "NumInPool"),
      class = "DataCheck_empty_rows"
    ),
    class = "DataCheck_col_not_numeric"
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
      CheckInputData(NA_df, "Result", "NumInPool"),
      class = "DataCheck_NA_rows"
    ),
    class = "DataCheck_invalid_results_values"
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
      CheckInputData(missing_df, "Result", "NumInPool"),
      class = "DataCheck_missing_values"
    ),
    class = "DataCheck_col_not_numeric"
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
      CheckInputData(NA_df, "Result", "NumInPool"),
      class = "DataCheck_missing_values"
    ),
    class = "DataCheck_invalid_results_values"
  )
})


test_that("Missing values in hierarchy columns returns warning", {
  missing_df <- SimpleExampleData
  missing_df[2,2] <- ""
  missing_df[3,3] <- ""
  missing_df[4,4] <- ""
  # Expect warning (missing values)
  expect_warning(
    CheckInputData(missing_df, "Result", "NumInPool"),
    class = "DataCheck_missing_values"
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
    CheckInputData(NA_df, "Result", "NumInPool"),
    class = "DataCheck_missing_values"
  )
})


test_that("CheckClusterVars() with SimpleExampleData has no errors or warnings", {
  #TODO
  expect_no_message(
    CheckClusterVars(SimpleExampleData, "Result", "NumInPool",
                     "Region", "Village", "Site")
  )
  expect_no_error(
    CheckClusterVars(SimpleExampleData, "Result", "NumInPool",
                     "Region", "Village", "Site")
  )
  expect_no_warning(
    CheckClusterVars(SimpleExampleData, "Result", "NumInPool",
                     "Region", "Village", "Site")
  )
})


test_that("Clustering by variable with missing values raises error", {
  #TODO
  # Site col includes NA
  site_df <- SimpleExampleData
  site_df$Site[1] <- NA
  # Village col includes empty string ""
  village_df <- SimpleExampleData
  village_df$Village[1] <- ""
  # Region col includes NA as factor level
  region_df <- SimpleExampleData
  region_df$Region[c(1, 289, 577, 865)] <- NA
  # Year col is all NA
  nosite_df <- SimpleExampleData
  nosite_df$Site <- NA
  # Call function
  expect_error(
    CheckClusterVars(site_df, "Result", "NumInPool",
                     "Region", "Village", "Site")
  )
  expect_error(
    CheckClusterVars(village_df, "Result", "NumInPool",
                     "Region", "Village", "Site")
  )
  expect_error(
    CheckClusterVars(region_df, "Result", "NumInPool",
                     "Region", "Village", "Site")
  )
  expect_error(
    CheckClusterVars(nosite_df, "Result", "NumInPool",
                     "Region", "Village", "Site")
  )
})


test_that("Correct nesting within hierarchy columns raises no errors", {
  # TODO
  nest_df <- data.frame(
    Region = c("A", "A", "A", "A", "B", "B", "B", "B"),
    Village = c("W", "W", "X", "X", "Y", "Y", "Z", "Z"),
    Site = c(1, 2, 3, 4, 5, 6, 7, 8),
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8))
  expect_no_error(
    CheckClusterVars(nest_df, "Result", "NumInPool",
                     "Region", "Village", "Site")
  )
})




test_that("Incorrect nesting within hierarchy columns raises errors", {
  #TODO
  bad_sites_df <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(c("W", "X", "Y", "Z"), each = 2),
    Site = rep(1:2, 4),
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8)
  )
  expect_error(
    CheckClusterVars(bad_sites_df, "Result", "NumInPool",
                     "Region", "Village", "Site")
  )
  bad_villages_df <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(rep(c("W", "X"), each = 2), 2),
    Site = 1:8,
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8)
  )
  expect_error(
    CheckClusterVars(bad_villages_df, "Result", "NumInPool",
                     "Region", "Village", "Site")
  )
})


test_that("check_nesting_levels() returns expected output", {
  # Check call with good nesting
  good_sites_df <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(c("W", "X", "Y", "Z"), each = 2),
    Site = 1:8,
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8)
  )
  good_sites_op <- NULL
  expect_identical(
    check_nesting_levels(data = good_sites_df, 
                         outer_cluster = "Village", 
                         inner_cluster = "Site"), 
    good_sites_op
  )
  # Check call with bad nesting
  bad_sites_df <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(c("W", "X", "Y", "Z"), each = 2),
    Site = rep(1:2, 4),
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8)
  )
  bad_sites_op <- as.data.frame(
    do.call(rbind,
            list(
              list(inner_vals = 1, 
                   outer_vals = c("W", "X", "Y", "Z"), 
                   inner_cluster = "Site", 
                   outer_cluster = "Village"),
              list(inner_vals = 2, 
                   outer_vals = c("W", "X", "Y", "Z"), 
                   inner_cluster = "Site", 
                   outer_cluster = "Village")
            )
    )
  )
  expect_equal(
    check_nesting_levels(data = bad_sites_df, 
                         outer_cluster = "Village", 
                         inner_cluster = "Site"), 
    bad_sites_op
  )
})

