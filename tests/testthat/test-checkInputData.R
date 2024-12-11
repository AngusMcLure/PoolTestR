test_that("CheckInputData - SimpleExampleData returns no errors/warnings", {
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


test_that("CheckInputData - missing results column returns error", {
  expect_error(
    CheckInputData(SimpleExampleData, "WrongResultColumnName", "NumInPool"),
    class = "DataCheck_missing_column"
  )
})


test_that("CheckInputData - missing poolSize column returns error", {
  expect_error(
    CheckInputData(SimpleExampleData, "Result", "WrongNumInPoolColumnName"),
    class = "DataCheck_missing_column"
  )
})


test_that("CheckInputData - character class for results column returns error", {
  char_df <- SimpleExampleData %>%
    mutate(across("Result", as.character))
  expect_error(
    CheckInputData(char_df, "Result", "NumInPool"),
    class = "DataCheck_col_not_numeric"
  )
})


test_that("CheckInputData - character class for poolSize column returns error", {
  char_df <- SimpleExampleData %>%
    mutate(across("NumInPool", as.character))
  expect_error(
    CheckInputData(char_df, "Result", "NumInPool"),
    class = "DataCheck_col_not_numeric"
  )
})


test_that("CheckInputData - empty rows in dataframe returns error and warning", {
  empty_row <- rep("", ncol(SimpleExampleData))
  names(empty_row) <- names(SimpleExampleData)
  empty_df <- rbind(SimpleExampleData %>%
                      mutate(across(everything(), as.character)),
                    empty_row,
                    empty_row,
                    empty_row)
  expect_error(
    expect_warning(
      CheckInputData(empty_df, "Result", "NumInPool"),
      class = "DataCheck_empty_rows"
    ),
    class = "DataCheck_col_not_numeric"
  )
})


test_that("CheckInputData - NA rows in dataframe returns error and warning", {
  NA_row <- rep(NA, ncol(SimpleExampleData))
  names(NA_row) <- names(SimpleExampleData)
  NA_df <- rbind(SimpleExampleData,
                 NA_row,
                 NA_row,
                 NA_row)
  expect_error(
    expect_warning(
      CheckInputData(NA_df, "Result", "NumInPool"),
      class = "DataCheck_NA_rows"
    ),
    class = "DataCheck_invalid_results_values"
  )
})


test_that("CheckInputData - missing column values returns error and warning", {
  missing_df <- SimpleExampleData
  missing_df[1,1] <- ""
  missing_df[2,2] <- ""
  missing_df[3,3] <- ""
  missing_df[4,4] <- ""
  missing_df[6,6] <- ""
  expect_error(
    expect_warning(
      CheckInputData(missing_df, "Result", "NumInPool"),
      class = "DataCheck_missing_values"
    ),
    class = "DataCheck_col_not_numeric"
  )
})


test_that("CheckInputData - NA column values returns error and warning", {
  NA_df <- SimpleExampleData
  NA_df[1,1] <- NA
  NA_df[2,2] <- NA
  NA_df[3,3] <- NA
  NA_df[4,4] <- NA
  NA_df[5,5] <- NA
  NA_df[6,6] <- NA
  expect_error(
    expect_warning(
      CheckInputData(NA_df, "Result", "NumInPool"),
      class = "DataCheck_missing_values"
    ),
    class = "DataCheck_invalid_results_values"
  )
})


test_that("CheckInputData - no hierarchy columns input raises error", {
  expect_error(
    CheckInputData(SimpleExampleData, "Result", "NumInPool",
                   hier_check = TRUE),
    class = "DataCheck_missing_hierarchy"
  )
})


test_that("CheckInputData - missing values in hierarchy columns returns warning", {
  missing_df <- SimpleExampleData
  missing_df[2,2] <- ""
  missing_df[3,3] <- ""
  missing_df[4,4] <- ""
  expect_warning(
    CheckInputData(missing_df, "Result", "NumInPool"),
    class = "DataCheck_missing_values"
  )
})


test_that("CheckInputData - NA values in hierarchy columns returns warning", {
  NA_df <- SimpleExampleData
  NA_df[2,2] <- NA
  NA_df[3,3] <- NA
  NA_df[4,4] <- NA
  NA_df[5,5] <- NA
  expect_warning(
    CheckInputData(NA_df, "Result", "NumInPool"),
    class = "DataCheck_missing_values"
  )
})


test_that("CheckClusterVars() with SimpleExampleData has no errors/warnings", {
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


test_that("CheckClusterVars - hierarchy columns with missing values raise error", {
  ## Site col includes NA
  test_df <- SimpleExampleData
  test_df$Site[1] <- NA
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     "Region", "Village", "Site"),
    class = "CheckClusterVars_missing_vals"
  )
  ## Village col includes empty string ""
  test_df <- SimpleExampleData
  test_df$Village[1] <- ""
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     "Region", "Village", "Site"),
    class = "CheckClusterVars_missing_vals"
  )
  ## Region col includes NA as factor level
  test_df <- SimpleExampleData
  test_df$Region[c(1, 289, 577, 865)] <- NA  
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     "Region", "Village", "Site"),
    class = "CheckClusterVars_missing_vals"
  )
  ## Site, Village and Region columns include NA
  test_df <- SimpleExampleData
  test_df$Site[1:5] <- NA
  test_df$Village[6:10] <- NA
  test_df$Region[11:15] <- NA
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     "Region", "Village", "Site"),
    class = "CheckClusterVars_missing_vals"
  )
  ## Site, Village and Region columns include ""
  test_df <- SimpleExampleData
  test_df$Site[1:5] <- ""
  test_df$Village[6:10] <- ""
  test_df$Region <- as.character(test_df$Region)
  test_df$Region[11:15] <- ""
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     "Region", "Village", "Site"),
    class = "CheckClusterVars_missing_vals"
  )
  ## Site col is all NA
  test_df <- SimpleExampleData
  test_df$Site <- NA
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     "Region", "Village", "Site"),
    class = "CheckClusterVars_missing_vals"
  )
  ## Site, Village and Region columns are all NA
  test_df <- SimpleExampleData
  test_df$Site <- NA
  test_df$Village <- NA
  test_df$Region <- NA
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     "Region", "Village", "Site"),
    class = "CheckClusterVars_missing_vals"
  )
})


test_that("CheckClusterVars - correct hierarchy nesting scheme has no errors", {
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


test_that("CheckClusterVars - incorrect hierarchy nesting raises errors", {
  bad_sites_df <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(c("W", "X", "Y", "Z"), each = 2),
    Site = rep(1:2, 4),
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8)
  )
  expect_warning(
    CheckClusterVars(bad_sites_df, "Result", "NumInPool",
                     "Region", "Village", "Site"),
    class = "CheckClusterVars_nesting"
  )
  bad_villages_df <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(rep(c("W", "X"), each = 2), 2),
    Site = 1:8,
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8)
  )
  expect_warning(
    CheckClusterVars(bad_villages_df, "Result", "NumInPool",
                     "Region", "Village", "Site"),
    class = "CheckClusterVars_nesting"
  )
  bad_sites_villages_df <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(rep(c("W", "X"), each = 2), 2),
    Site = c(1:4, 4:1),
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8)
  )
  expect_warning(
    CheckClusterVars(bad_sites_villages_df, "Result", "NumInPool",
                     "Region", "Village", "Site"),
    class = "CheckClusterVars_nesting"
  )
  # This test is particularly tricky to detangle 
  # Each "Village" is associated with multiple "Regions"
  # However, this is acceptable "Site" input as each "Site" is associated with 
  # only one "Village". 
  # TODO - potential cross-levels check (i.e., check "Site" and "Region")
  bad_sites_villages_df <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(rep(c("W", "X"), each = 2), 2),
    Site = c(1:4, 1:4),
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8)
  )
  expect_warning(
    CheckClusterVars(bad_sites_villages_df, "Result", "NumInPool",
                     "Region", "Village", "Site"),
    class = "CheckClusterVars_nesting"
  )
})


test_that("check_nesting_levels() returns expected output", {
  # Check good nesting returns 12 comparisons with no duplicates
  good_sites_df <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(c("W", "X", "Y", "Z"), each = 2),
    Site = 1:8,
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8)
  )
  good_sites_op <- check_nesting_levels(
    data = good_sites_df, 
    hierarchy_scheme = c("Region", "Village", "Site")
  )
  expect_equal(
    length(which(good_sites_op$num_outer_val == 1)),
    12
  )
  # Check bad nesting at "Site" level returns 6 comparisons, 2 with duplicates
  bad_sites_df <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(c("W", "X", "Y", "Z"), each = 2),
    Site = rep(1:2, 4),
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8)
  )
  bad_sites_op <- check_nesting_levels(
    data = bad_sites_df, 
    hierarchy_scheme = c("Region", "Village", "Site")
  )
  expect_equal(
    length(which(bad_sites_op$num_outer_val == 1)),
    4
  )
  expect_equal(
    length(which(bad_sites_op$num_outer_val > 1)),
    2
  )
  # Check bad nesting at "Site" level returns 10 comparisons, 2 with duplicates
  bad_villages_df <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(rep(c("W", "X"), each = 2), 2),
    Site = 1:8,
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8)
  )
  bad_villages_op <-  check_nesting_levels(
    data = bad_villages_df, 
    hierarchy_scheme = c("Region", "Village", "Site")
  )
  expect_equal(
    length(which(bad_villages_op$num_outer_val == 1)),
    8
  )
  expect_equal(
    length(which(bad_villages_op$num_outer_val > 1)),
    2
  )
  # Check bad nesting at "Site" and "Villages" levels returns 6 comparisons, 6 with duplicates
  bad_sites_villages_df <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(rep(c("W", "X"), each = 2), 2),
    Site = c(1:4, 4:1),
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = rep(0, 8)
  )
  bad_sites_villages_op <- check_nesting_levels(
    data = bad_sites_villages_df, 
    hierarchy_scheme = c("Region", "Village", "Site")
  )
  expect_equal(
    length(which(bad_sites_villages_op$num_outer_val == 1)),
    0
  )
  expect_equal(
    length(which(bad_sites_villages_op$num_outer_val > 1)),
    6
  )
})

## TODO test CheckClusterVars for different num of cols in scheme 
# 2 cols 
# 4 cols
# 5 cols

# TODO test PrepareClusterData output
# test output
# test warnings
# test output ignoring warnings

