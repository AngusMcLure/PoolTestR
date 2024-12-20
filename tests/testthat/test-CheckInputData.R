test_that("CheckInputData - SimpleExampleData returns no errors/warnings", {
  expect_no_message(
    expect_no_error(
      expect_no_warning(
        CheckInputData(SimpleExampleData, "Result", "NumInPool")
      )
    )
  )
  expect_no_message(
    expect_no_error(
      expect_no_warning(
        CheckInputData(as_tibble(SimpleExampleData), "Result", "NumInPool")
      )
    )
  )
})

test_that("CheckInputData returns error for non-data.frame input", {
  # numeric
  expect_error(
    CheckInputData(1, result = "Result", poolSize = "NumInPool"),
    class = "DataCheck_input_class"
  )
  # character
  expect_error(
    CheckInputData("a", "Result", "NumInPool"),
    class = "DataCheck_input_class"
  )
  # function
  expect_error(
    CheckInputData(sum(), "Result", "NumInPool"),
    class = "DataCheck_input_class"
  )
  # matrix
  expect_error(
    CheckInputData(as.matrix(SimpleExampleData), "Result", "NumInPool"),
    class = "DataCheck_input_class"
  )
})

test_that("CheckInputData - missing result and poolSize columns returns error", {
  expect_error(
    CheckInputData(SimpleExampleData, "WrongResultColumnName", "WrongNumInPoolName"),
    class = "DataCheck_missing_column"
  )
})


test_that("CheckInputData - missing result column returns error", {
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


test_that("CheckInputData - missing stratifying columns returns error", {
  expect_error(
    CheckInputData(SimpleExampleData, "Result", "NumInPool",
                   "Region", "WrongYearColumnName"),
    class = "DataCheck_missing_groupVar"
  )
  expect_error(
    CheckInputData(SimpleExampleData, "Result", "NumInPool",
                   "WrongRegionColumnName", "Year"),
    class = "DataCheck_missing_groupVar"
  )
  expect_error(
    CheckInputData(SimpleExampleData, "Result", "NumInPool",
                   "WrongRegionColumnName", "WrongYearColumnName"),
    class = "DataCheck_missing_groupVar"
  )
})


test_that("CheckInputData - all Result 0 works", {
  zero_df <- 
    SimpleExampleData %>%
    mutate("Result" = 0)
  expect_no_warning(
    expect_no_error(
      CheckInputData(zero_df, "Result", "NumInPool")
    )
  )
})

test_that("CheckInputData - all Result 1 works", {
  one_df <- 
    SimpleExampleData %>%
    mutate("Result" = 1)
  expect_no_warning(
    expect_no_error(
      CheckInputData(one_df, "Result", "NumInPool")
    )
  )
})


test_that("CheckInputData - character class for results column returns error", {
  char_df <- 
    SimpleExampleData %>%
    mutate(across("Result", as.character))
  expect_error(
    CheckInputData(char_df, "Result", "NumInPool"),
    class = "DataCheck_col_not_numeric"
  )
})


test_that("CheckInputData - character class for poolSize column returns error", {
  char_df <- 
    SimpleExampleData %>%
    mutate(across("NumInPool", as.character), .keep = "all")
  expect_error(
    CheckInputData(char_df, "Result", "NumInPool"),
    class = "DataCheck_col_not_numeric"
  )
})


test_that("CheckInputData - negative values in poolSize column returns error", {
  negative_df <- 
    SimpleExampleData %>%
    mutate(NumInPool = (-1*.data$NumInPool),
           .keep = "all")
  expect_error(
    CheckInputData(negative_df, "Result", "NumInPool"),
    class = "DataCheck_col_not_positive"
  )
})


test_that("CheckInputData - empty rows in dataframe returns error and warning", {
  empty_row <- rep("", ncol(SimpleExampleData))
  names(empty_row) <- names(SimpleExampleData)
  empty_df <- rbind(
    SimpleExampleData %>%
      mutate(
        across(everything(), as.character)
      ),
    empty_row,
    empty_row,
    empty_row
  )
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
  NA_df <- rbind(
    SimpleExampleData,
    NA_row,
    NA_row,
    NA_row
  )
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


test_that("utils function check_nesting_levels() returns expected output", {
  # Check good nesting returns 12 comparisons with no duplicates
  good_sites_df <-
    data.frame(
      Region = rep(c("A", "B"), each = 4),
      Village = rep(c("W", "X", "Y", "Z"), each = 2),
      Site = 1:8,
      Year = rep(0, 8),
      NumInPool = rep(10, 8),
      Result = rep(0, 8)
    )
  good_sites_op <- 
    check_nesting_levels(
      data = good_sites_df, 
      hierarchy = c("Region", "Village", "Site")
    )
  expect_equal(
    length(which(good_sites_op$num_outer_val == 1)),
    12
  )
  # Check bad nesting at "Site" level returns 6 comparisons, 2 with duplicates
  bad_sites_df <- 
    data.frame(
      Region = rep(c("A", "B"), each = 4),
      Village = rep(c("W", "X", "Y", "Z"), each = 2),
      Site = rep(1:2, 4),
      Year = rep(0, 8),
      NumInPool = rep(10, 8),
      Result = rep(0, 8)
    )
  bad_sites_op <- 
    check_nesting_levels(
      data = bad_sites_df, 
      hierarchy = c("Region", "Village", "Site")
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
  bad_villages_df <- 
    data.frame(
      Region = rep(c("A", "B"), each = 4),
      Village = rep(rep(c("W", "X"), each = 2), 2),
      Site = 1:8,
      Year = rep(0, 8),
      NumInPool = rep(10, 8),
      Result = rep(0, 8)
    )
  bad_villages_op <-  
    check_nesting_levels(
      data = bad_villages_df, 
      hierarchy = c("Region", "Village", "Site")
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
  bad_sites_villages_df <- 
    data.frame(
      Region = rep(c("A", "B"), each = 4),
      Village = rep(rep(c("W", "X"), each = 2), 2),
      Site = c(1:4, 4:1),
      Year = rep(0, 8),
      NumInPool = rep(10, 8),
      Result = rep(0, 8)
    )
  bad_sites_villages_op <- 
    check_nesting_levels(
      data = bad_sites_villages_df, 
      hierarchy = c("Region", "Village", "Site")
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


test_that("CheckClusterVars() with SimpleExampleData has no errors/warnings", {
  expect_no_error(
    expect_no_warning(
      expect_no_message(
        CheckClusterVars(SimpleExampleData, "Result", "NumInPool",
                         hierarchy = c("Region", "Village", "Site"))
      )
    )
  )
})


test_that("CheckClusterVars() returns error when no hierarchy input", {
  expect_error(
    CheckClusterVars(SimpleExampleData, "Result", "NumInPool"),
    class = "CheckClusterVars_no_hierarchy"
  )
})


test_that("CheckClusterVars returns error for non-data.frame input", {
  # numeric
  expect_error(
    CheckClusterVars(1, "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_input_class"
  )
  # character
  expect_error(
    CheckClusterVars("a", "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_input_class"
  )
  # function
  expect_error(
    CheckClusterVars(sum(), "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_input_class"
  )
})


test_that("CheckClusterVars - hierarchy columns with missing values raise error", {
  ## Site column includes NA
  test_df <- SimpleExampleData
  test_df$Site[1] <- NA
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_missing_vals"
  )
  ## Village col includes empty string ""
  test_df <- SimpleExampleData
  test_df$Village[1] <- ""
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_missing_vals"
  )
  ## Region col includes NA as factor level
  test_df <- SimpleExampleData
  test_df$Region[c(1, 289, 577, 865)] <- NA  
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_missing_vals"
  )
  ## Site, Village and Region columns include NA
  test_df <- SimpleExampleData
  test_df$Site[1:5] <- NA
  test_df$Village[6:10] <- NA
  test_df$Region[11:15] <- NA
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
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
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_missing_vals"
  )
  ## Site col is all NA
  test_df <- SimpleExampleData
  test_df$Site <- NA
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_missing_vals"
  )
  ## Site, Village and Region columns are all NA
  test_df <- SimpleExampleData
  test_df$Site <- NA
  test_df$Village <- NA
  test_df$Region <- NA
  expect_error(
    CheckClusterVars(test_df, "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_missing_vals"
  )
})


test_that("CheckClusterVars - correct hierarchy nesting scheme has no errors", {
  nest_df <- 
    data.frame(
      Region = c("A", "A", "A", "A", "B", "B", "B", "B"),
      Village = c("W", "W", "X", "X", "Y", "Y", "Z", "Z"),
      Site = c(1, 2, 3, 4, 5, 6, 7, 8),
      Year = rep(0, 8),
      NumInPool = rep(10, 8),
      Result = rep(0, 8)
    )
  expect_no_error(
    CheckClusterVars(nest_df, "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site"))
  )
})


test_that("CheckClusterVars - incorrect hierarchy nesting raises errors", {
  bad_sites_df <- 
    data.frame(
      Region = rep(c("A", "B"), each = 4),
      Village = rep(c("W", "X", "Y", "Z"), each = 2),
      Site = rep(1:2, 4),
      Year = rep(0, 8),
      NumInPool = rep(10, 8),
      Result = rep(0, 8)
    )
  expect_warning(
    CheckClusterVars(bad_sites_df, "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_nesting"
  )
  bad_villages_df <- 
    data.frame(
      Region = rep(c("A", "B"), each = 4),
      Village = rep(rep(c("W", "X"), each = 2), 2),
      Site = 1:8,
      Year = rep(0, 8),
      NumInPool = rep(10, 8),
      Result = rep(0, 8)
    )
  expect_warning(
    CheckClusterVars(bad_villages_df, "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_nesting"
  )
  bad_sites_villages_df <- 
    data.frame(
      Region = rep(c("A", "B"), each = 4),
      Village = rep(rep(c("W", "X"), each = 2), 2),
      Site = c(1:4, 4:1),
      Year = rep(0, 8),
      NumInPool = rep(10, 8),
      Result = rep(0, 8)
    )
  expect_warning(
    CheckClusterVars(bad_sites_villages_df, "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_nesting"
  )
  bad_sites_villages_df <- 
    data.frame(
      Region = rep(c("A", "B"), each = 4),
      Village = rep(rep(c("W", "X"), each = 2), 2),
      Site = c(1:4, 1:4),
      Year = rep(0, 8),
      NumInPool = rep(10, 8),
      Result = rep(0, 8)
    )
  expect_warning(
    CheckClusterVars(bad_sites_villages_df, "Result", "NumInPool",
                     hierarchy = c("Region", "Village", "Site")),
    class = "CheckClusterVars_nesting"
  )
})


test_that("CheckClusterVars() works with 1 hierarchy column", {
  expect_no_message(
    expect_no_warning(
      expect_no_error(
        CheckClusterVars(SimpleExampleData, "Result", "NumInPool",
                         hierarchy = c("Site"))
      )
    )
  )
}
)


test_that("CheckClusterVars() works with 2 hierarchy columns", {
  expect_no_message(
    expect_no_warning(
      expect_no_error(
        CheckClusterVars(SimpleExampleData, "Result", "NumInPool",
                         hierarchy = c("Village", "Site"))
      )
    )
  )
}
)


test_that("CheckClusterVars() works with 3 hierarchy columns", {
  expect_no_message(
    expect_no_warning(
      expect_no_error(
        CheckClusterVars(SimpleExampleData, "Result", "NumInPool",
                         hierarchy = c("Region", "Village", "Site")),
        class = "CheckClusterVars_nesting"
      )
    )
  )
}
)


test_that("CheckClusterVars() works with 4 hierarchy columns", {
  hier_df <- SimpleExampleData %>%
    mutate("Country" = case_match(
      .data$Region,
      c("A", "B") ~ "C1",
      c("C", "D") ~ "C2"
    ),
    .keep = "all")
  expect_no_message(
    expect_no_warning(
      expect_no_error(
        CheckClusterVars(hier_df, "Result", "NumInPool",
                         hierarchy = c("Country", "Region", "Village", "Site")),
        class = "CheckClusterVars_nesting"
      )
    )
  )
}
)


test_that("PrepareClusterData() raises error when no hierarchy input", {
  expect_error(
    PrepareClusterData(
      data = SimpleExampleData, 
      result = "Result", poolSize = "NumInPool"),
    class = "PrepareClusterData_no_hierarchy"
  )
})

test_that("PrepareClusterData() raises error for incorrect hierarchy column names", {
  expect_error(
    PrepareClusterData(
      data = SimpleExampleData, 
      result = "Result", poolSize = "NumInPool",
      hierarchy = c("WrongVillageName", "WrongSiteName")
      ),
    class = "PrepareClusterData_missing_hier_cols"
  )
})

test_that("PrepareClusterData() raises error for incorrect stratifying column names", {
  expect_error(
    PrepareClusterData(
      data = SimpleExampleData, 
      result = "Result", poolSize = "NumInPool",
      hierarchy = c("Village", "Site"),
      "WrongRegionName", "WrongYearName"
    ),
    class = "DataCheck_missing_groupVar"
  )
})


test_that("PrepareClusterData() raises no errors for SimpleExampleData", {
  expect_no_message(
    expect_no_error(
      expect_no_warning(
        PrepareClusterData(
          data = SimpleExampleData, 
          result = "Result", poolSize = "NumInPool", 
          hierarchy = c("Site") 
        )
      )
    )
  )
  expect_no_message(
    expect_no_error(
      expect_no_warning(
        PrepareClusterData(
          data = SimpleExampleData, 
          result = "Result", poolSize = "NumInPool", 
          hierarchy = c("Village", "Site")
        )
      )
    )
  )
  expect_no_message(
    expect_no_error(
      expect_no_warning(
        PrepareClusterData(
          data = SimpleExampleData, 
          result = "Result", poolSize = "NumInPool", 
          hierarchy = c("Region", "Village", "Site") 
        )
      )
    )
  )
  expect_no_message(
    expect_no_error(
      expect_no_warning(
        PrepareClusterData(
          data = SimpleExampleData, 
          result = "Result", poolSize = "NumInPool", 
          hierarchy = c("Region", "Village", "Site"),
          "Year"
        )
      )
    )
  )
  expect_no_message(
    expect_no_error(
      expect_no_warning(
        PrepareClusterData(
          data = SimpleExampleData, 
          result = "Result", poolSize = "NumInPool", 
          hierarchy = c("Village", "Site"),
          "Region", "Year"
        )
      )
    )
  )
})


test_that("PrepareClusterData() returns SimpleExampleData unchanged", {
  SimpleExample_output <- 
    PrepareClusterData(
      data = SimpleExampleData, 
      result = "Result", 
      poolSize = "NumInPool", 
      hierarchy = c("Site") 
    )
  expect_identical(
    SimpleExample_output,
    SimpleExampleData
  )
})


test_that("PrepareClusterData() works when hierarchy values inadequately nested", {
  bad_sites_villages_df <- 
    data.frame(
      Region = rep(c("A", "B"), each = 4),
      Village = rep(rep(c("W", "X"), each = 2), 2),
      Site = c(1:4, 4:1),
      Year = rep(0, 8),
      NumInPool = rep(10, 8),
      Result = c(rep(0, 8))
    )
  expect_message(
    expect_warning(
      PrepareClusterData(data = bad_sites_villages_df, 
                         result = "Result", poolSize = "NumInPool", 
                         hierarchy = c("Village", "Site") ),
      class = "PrepareClusterData_nesting"
    ),
    class = "PrepareClusterData_output"
  )
})


test_that("PrepareClusterData() adds new column when hierarchy is inadequately nested", {
  bad_input_df <- 
    data.frame(
      Region = rep(c("A", "B"), each = 4),
      Village = rep(rep(c("W", "X"), each = 2), 2),
      Site = c(1:4, 4:1),
      Year = rep(0, 8),
      NumInPool = rep(10, 8),
      Result = c(rep(0, 8))
    )
  expect_message(
    expect_warning(
      bad_output <- PrepareClusterData(
        data = bad_input_df, 
        result = "Result", 
        poolSize = "NumInPool", 
        hierarchy = c("Region", "Village", "Site") 
      ),
      class = "PrepareClusterData_nesting"
    ),
    class = "PrepareClusterData_output"
  )
  expect_equal(
    bad_output$PoolTestR_ID,
    paste0(bad_input_df$Region, "_", bad_input_df$Village, "_", bad_input_df$Site)
  )
  expect_equal(
    nrow(bad_input_df),
    nrow(bad_output)
  )
  expect_equal(
    ncol(bad_input_df) + 1,
    ncol(bad_output)
  )
})

