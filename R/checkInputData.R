#' Checking input data for errors and inconsistencies
#' 
#'
#' Internal function to test the input data and return relevant errors.
#'
#' @param data A \code{data.frame} with one row for each pooled sampled and
#'   columns for the size of the pool (i.e., the number of specimens / isolates /
#'   insects pooled to make that particular pool), the result of the test of the
#'   pool. It may also contain additional columns with additional information
#'   (e.g. location where pool was taken) which can optionally be used for
#'   stratifying the data into smaller groups and calculating prevalence by
#'   group (e.g. calculating prevalence for each location).
#' @param result The name of column with the result of each test on each pooled
#'   sample. The result must be stored with 1 indicating a positive test result
#'   and 0 indicating a negative test result.
#' @param poolSize The name of the column with number of
#'   specimens/isolates/insects in each pool.
#' @param ... Optional name(s) of columns included in the hierarchical sampling
#'   scheme, listed from largest to smallest. Only used if 
#'   \code{hier_check = TRUE}.
#' @param hier_check Logical. Default is \code{FALSE}. If \code{TRUE}, checks
#'  the hierarchy using column names provided in \code{...}.
#' @param excludeCols Character vector containing the name(s) of columns to 
#'   exclude from the input checks (e.g., notes columns). If no character
#'   vector is provided, \code{excludeCols = NULL}.
#'
#' @return Returns \code{data} invisibly, using \code{invisible(x)}
#' 
#' For the \code{SimpleExampleData} data included in this package, the 
#' hierarchical sampling scheme is \code{Region} > \code{Village} > \code{Site}.
#' The function call would be:
#' \code{checkInputData(SimpleExampleData, "Result", "NumInPool",
#' "Region", "Village", "Site", hier_check = TRUE)}
#'
#' @keywords internal
#' @noRd

CheckInputData <- function(data, result, poolSize, ...,  
                           hier_check = FALSE, excludeCols = NULL){
  # Extract name(s) of columns to group by
  hier_vars <- as.character(list(...)) 
  
  # Remove any columns flagged for exclusion
  if (! is.null(excludeCols) ){
    test_data <- data %>% select(! excludeCols)
  } else (
    test_data <- data
  )
  
  ## Warnings
  # Check whether empty rows are present
  empty_rows <- which(apply(test_data == "", 1, all) == TRUE)
  if (length(empty_rows) > 0){
    rlang::warn(
      message = paste0("Empty rows are present within the dataset.\n",
                       "Rows: ", 
                       paste(empty_rows, collapse = ", ")
      ),
      class = c("DataCheck_empty_rows", "warning", "condition")
    )
  }
  
  # Check whether rows containing only NA are present
  NA_rows <- which(apply(is.na(test_data), 1, all) == TRUE)
  if (length(NA_rows) > 0){
    rlang::warn(
      message = paste0("Rows containing only NA are present within the dataset.\n",
                       "Rows: ", 
                       paste(NA_rows, collapse = ", ") 
      ),
      class = c("DataCheck_NA_rows", "warning", "condition")
    )
  }
  
  # Check whether note rows or missing values are present
  # i.e., whether rows are present that have 1 or more missing values 
  #.      (but are not a fully empty row)
  missing_value_rows <- sort(
    unique(
      c(
        which(rowSums(is.na(test_data)) != 0  
              & rowSums(is.na(test_data)) < ncol(test_data)),
        which(rowSums(test_data == "") != 0  
              & rowSums(test_data == "") < ncol(test_data))
      )
    )
  )
  if (length(missing_value_rows) > 0){
    rlang::warn(
      message = paste0("Rows containing missing values are present within the dataset.\n",
                       "Rows: ", 
                       paste(missing_value_rows, collapse = ", ") 
      ),
      class = c("DataCheck_missing_values", "warning", "condition")
    )
  }
  
  ## Errors
  # Check whether result and poolSize variables are present within column names
  if (! (result %in% names(test_data)) ){
    rlang::abort(
      message = "result column not included in dataframe",
      class = c("DataCheck_missing_column", "error", "condition")
    )
  }
  if (! (poolSize %in% names(test_data)) ){
    rlang::abort(
      message = "poolSize column not included in dataframe",
      class = c("DataCheck_missing_column", "error", "condition")
    )
  }
  
  # Check whether results column values are numeric or integer
  if (! (class(test_data[,result]) == "numeric" || 
         class(test_data[,result]) == "integer") ){
    rlang::abort(
      message = paste0(
        'Results of each test must be stored as class "numeric" with only ',
        'values 0 (negative test) or 1 (positive test)'
      ),
      class = c("DataCheck_col_not_numeric", "error", "condition")
    )
  }
  
  # Check whether poolSize column values are numeric or integer
  if (! (class(test_data[,poolSize]) == "numeric" || 
         class(test_data[,poolSize]) == "integer") ){
    rlang::abort(
      message = 'Pool size column should be class "numeric"',
      class = c("DataCheck_col_not_numeric", "error", "condition")
    )
  }
  
  # Check whether result column contains only 0 and 1
  result_vals <- unique(test_data[, result])
  if (! setequal(c(0,1), result_vals)){
    rlang::abort(
      message = paste0(
        'Results of each test must only be values 0 (negative test) ',
        'or 1 (positive test)'
      ),
      class = c("DataCheck_invalid_results_values", "error", "condition")
    )
  }
  
  # Check lower levels of hierarchy - lowest level (e.g., house) should look at next level
  # i.e., village and each househould should only return within one village
  # For that check to work need to know the order of the hierarchy i.e., Site, Village
  # OK to just check clustering variables 
  # Most common mistake is order houses 1-10 in each village, have houses 1-10 in village A, B, C
  # Will assume that house 10 is in all the villages (doesn't make sense)
  # PoolTools -> listing from biggest to smallest
  # Check whether Fred implemented check on PoolTools to check data size
  # Loop through different levels -> check each house in 1 village, each village in 1 region
  # Helper function to help people tidy up their names - return error and return helper function
  # Make a warning on PoolTools - names not unique, we have provided unique names and run through this
  # Check hierarchy column names too 
  # NOTE: See function below `CheckClusterVars()`
  
  # Check hierarchy columns 
  if (hier_check == TRUE & length(hier_vars) > 0) {
    # Check that names of hierarchy columns are present in data
    missing_hier_cols <- hier_vars[! (hier_vars %in% names(test_data)) ]
    if (length(missing_hier_cols) > 0){
      rlang::abort(
        message = paste0(
          'Data frame does not include the following columns: ',
          paste(missing_hier_cols, collapse = ", ")
        ),
        class = c("DataCheck_missing_hier_cols", "error", "condition"),
        missing_cols = missing_hier_cols
      )
    }
    
    # Check values of each column in the hierarchy 
    
    
    
    # Check whether each row has a unique site column value
    # Complicated - cannot make assumptions about the rest of the data, which
    # means you cannot simply compare the expected and actual number of values
    # Need to think about this
    
    
    
  }
  
  
  # Return data, invisibly, if all checks succeed
  return(invisible(data))
}


#' Checking hierarchy columns for missing values
#' 
#' 
#' Internal function to test the input data and return relevant errors.
#'
#' @param data A \code{data.frame} with one row for each pooled sampled and
#'   columns for the size of the pool (i.e., the number of specimens / isolates /
#'   insects pooled to make that particular pool), the result of the test of the
#'   pool. It may also contain additional columns with additional information
#'   (e.g. location where pool was taken) which can optionally be used for
#'   stratifying the data into smaller groups and calculating prevalence by
#'   group (e.g. calculating prevalence for each location).
#' @param result The name of column with the result of each test on each pooled
#'   sample. The result must be stored with 1 indicating a positive test result
#'   and 0 indicating a negative test result.
#' @param poolSize The name of the column with number of 
#'   specimens/isolates/insects in each pool.
#' @param ... Optional name(s) of columns with variables to stratify the data
#' by. If omitted the complete dataset is used to estimate a single
#' prevalence. If included, prevalence is estimated separately for each group
#' defined by these columns
#'
#' @return Returns \code{data} invisibly, using \code{invisible(x)}
#' 
#' Iterate through each column in \code{...}. If any values in those columns
#' are missing (i.e., \code{""}, \code{NA}, \code{NULL}), an error is raised.
#' 
#' Checks nesting of the columns in \code{...}. If the given hierarchy/clustering 
#' scheme is Region > Village > Site, this function checks that each Site appears
#' within only one Village, and that each Village appears within only one region.
#' 
#' For the \code{SimpleExampleData} data included in this package, the 
#' hierarchical sampling scheme is \code{Region} > \code{Village} > \code{Site}.
#' The function call would be:
#' \code{checkClusterVars(SimpleExampleData, "Region", "Village", "Site")}
#'
#' @keywords internal
#' @noRd
CheckClusterVars <- function(data, result, poolSize, ...){
  ## Extract name(s) of columns to group by
  groupVar <- as.character(list(...)) 
  
  ## Check that each column exists
  missing_hier_cols <- groupVar[! (groupVar %in% names(data))]
  if (length(missing_hier_cols) > 0){
    rlang::abort(
      message = paste0(
        'Data frame does not include the following columns: ',
        paste(missing_hier_cols, collapse = ", ")
      ),
      class = c("CheckClusterVars_missing_cols", "error", "condition"),
      missing_cols = missing_hier_cols
    )
  }
  
  ## Identify missing values in cluster/hierarchy columns
  missing_list <- vector(mode="list", length=length(groupVar))
  names(missing_list) <- groupVar
  for (i in groupVar){
    i_vals <- data[, i]
    if (class(i_vals) == "character"){
      # Missing char vals = "", NA, NULL
      missing_i_vals <- which(i_vals == "" | is.na(i_vals) | is.null(i_vals))
      if (length(missing_i_vals) > 0){
        missing_list[[i]] <- missing_i_vals
      }
    } else if (class(i_vals) == "integer" | class(i_vals) == "numeric"){
      # Missing num/int vals = NA, NULL
      missing_i_vals <- which(is.na(i_vals) | is.null(i_vals))
      if (length(missing_i_vals) > 0){
        missing_list[[i]] <- missing_i_vals
      }
    } else if (class(i_vals) == "factor"){
      # Missing factor levels = "", NA, NULL
      missing_i_vals <- which(i_vals == "" | is.na(i_vals) | is.null(i_vals))
      if (length(missing_i_vals) > 0){
        missing_list[[i]] <- missing_i_vals
      }
    }
  }
  # Extract the missing values for each column in the hierarchy/sampling scheme
  bad_cols <- groupVar[! unlist(lapply(groupVar, function(x){is.null(missing_list[[x]])}))]
  if (length(bad_cols) > 0){
    # Raise error and output missing values and corresponding column name
    output_list <- missing_list[bad_cols]
    output_messages <- unlist(
      lapply(
        names(output_list), 
        function(x){paste0(x, ": ", paste(output_list[[x]], collapse = ", "))}
      )
    )
    rlang::abort(
      message = paste0("Some columns in the hierarchy/sampling scheme have missing values\n",
                       "Please correct the following values in each of these columns:\n",
                       paste(output_messages, collapse = "\n")),
      class = c("CheckClusterVars_missing_vals", "error", "condition"),
      missing_vals = output_list
    )
  }
  
  ## Check nesting within hierarchy/sampling scheme
  # Set nesting levels
  nesting_list <- vector(mode="list", length = (length(groupVars) - 1) )
  for (i in 1:length(nesting_list)){
    # Use rev(groupVars) so hierarchy columns ordered from smallest to largest 
    temp_nest_vector <- c("outer" = rev(groupVars)[i+1],
                          "inner" = rev(groupVars)[i])
    nesting_list[[i]] <- temp_nest_vector
  }
  # Check each level
  check_nests <- 
    lapply(
      1:length(nesting_list), 
      function(i){
        check_nesting_levels(
          data = data,
          outer_cluster = nesting_list[[i]][["outer"]], 
          inner_cluster = nesting_list[[i]][["inner"]]
        )
      }
    )
  # Return errors for poor nesting
  # TODO

  
  # Return data, invisibly, if check succeeds
  return(invisible(data))
}


#' Prepare hierarchical/clustered survey data for analysis
#' 
#' Helper function to prepare data for analysis with \code{HierPoolPrev()} by
#' ensuring that every location has a unique identifier.
#'
#' @param data A \code{data.frame} with one row for each pooled sampled and
#'   columns for the size of the pool (i.e. the number of specimens / isolates /
#'   insects pooled to make that particular pool), the result of the test of the
#'   pool. It may also contain additional columns with additional information
#'   (e.g. location where pool was taken) which can optionally be used for
#'   stratifying the data into smaller groups and calculating prevalence by
#'   group (e.g. calculating prevalence for each location)
#' @param result The name of column with the result of each test on each pooled
#'   sample. The result must be stored with 1 indicating a positive test result
#'   and 0 indicating a negative test result.
#' @param poolSize The name of the column with number of
#'   specimens/isolates/insects in each pool
#' @param ... Optional name(s) of columns included in the hierarchical sampling
#'   scheme, listed from largest to smallest. 
#'
#' @return An object of class \code{data.frame}, containing the same columns as
#' the input, with a single new column containing a unique identifier for
#' each location in the survey.
#' 
#' This function checks that each site in the data has a unique identifying
#' variable.
#' 
#' For example, take a hypothetical survey with two villages \code{A} and 
#' \code{B}, and two households are sampled within each village. In this survey, 
#' the \code{"household"} variable is nested inside the \code{"village"} 
#' variable. Each of the four households must have a unique identifier. 
#' 
#' A common mistake is to reuse names across different locations, e.g., to
#' name the two households in Village A \code{H1} and \code{H2}, and to name
#' the two households in Village B \code{H1} and \code{H2}. The household names
#' are duplicated - there are two houses named \code{H1} and two named 
#' \code{H2}. To ensure unique variables for each location, the houses could be 
#' numbered sequentially e.g., \code{c("H1", "H2", "H3", "H4")}, or each house
#' could be labelled with the corresponding village e.g., 
#' \code{c("A-H1", "A-H2", "B-H1", "B-H2")}.
#' 
#' The tools in PoolTools apply for all cases where a hierarchical sampling 
#' frame is involved. The functions do not make assumptions about the 
#' number of levels present or the names of hierarchical columns. 
#' 
#' Note that each hierarchical variable must have its own column. If the 
#' sampling scheme is "District" > "Subdistrict" > "Street" > "Unit", there 
#' should be one column in the data for each of the columns \code{"District"},
#' \code{"Subdistrict"}, \code{"Street"}, and \code{"Unit"}. 
#' 
#' For the \code{SimpleExampleData} data included in this package, the 
#' hierarchical sampling scheme is \code{Region} > \code{Village} > \code{Site}.
#' The function call would be:
#' \code{prepareClusterData(SimpleExampleData, "Result", "NumInPool",
#' "Region", "Village", "Site")}
#
#' @export
#' 
#' @seealso \code{\link{HierPoolPrev}}, \code{\link{getPrevalence}}
#'
# TODO Need to add examples here!
# #' @examples
# TODO Complete documentation and remove example from doco, use example from @examples instead
PrepareClusterData <- function(data, result, poolSize, ...){
  # TODO write function
}

