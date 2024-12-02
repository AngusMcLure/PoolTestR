#' Checking input data for errors and inconsistencies
#' 
#' 
#' Internal function to test the input data and return relevant errors.
#'
#' @param data A \code{data.frame} with one row for each pooled sampled and
#'   columns for the size of the pool (i.e. the number of specimens / isolates /
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
#' @param excludeCols Character vector containing the name(s) of columns to 
#'   exclude from the input checks (e.g., notes columns).
#' @param hier_check Logical. Default is \code{FALSE}. If \code{TRUE}, checks
#'  whether each row in the specified \code{location}.
#' @param location The name of the column that uniquely identifies each 
#' location. Must be specified when \code{hier_check = TRUE}.
#'
#' @return Returns \code{data} invisibly, using \code{invisible(x)}
#'
#' @keywords internal
#' @noRd

checkInputData <- function(data, result, poolSize, excludeCols = NULL, 
                           hier_check = FALSE, location = NULL){
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
      class = "DataCheck_empty_rows"
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
      class = "DataCheck_NA_rows"
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
      class = "DataCheck_missing_values"
    )
  }
  
  ## Errors
  # Check whether result and poolSize variables are present within column names
  if (! (result %in% names(test_data)) ){
    rlang::abort(
      message = "result column not included in dataframe",
      class = "DataCheck_missing_column")
  }
  if (! (poolSize %in% names(test_data)) ){
    rlang::abort(
      message = "poolSize column not included in dataframe",
      class = "DataCheck_missing_column")
  }
  
  # Check whether results column values are numeric or integer
  if (! (class(test_data[,result]) == "numeric" || 
         class(test_data[,result]) == "integer") ){
    rlang::abort(
      message = paste0(
        'Results of each test must be stored as class "numeric" with only ',
        'values 0 (negative test) or 1 (positive test)'
      ),
      class = "DataCheck_col_not_numeric"
    )
  }
  
  # Check whether poolSize column values are numeric or integer
  if (! (class(test_data[,poolSize]) == "numeric" || 
         class(test_data[,poolSize]) == "integer") ){
    rlang::abort(
      message = 'Pool size column should be class "numeric"',
      class = "DataCheck_col_not_numeric")
  }
  
  # Check whether result column contains only 0 and 1
  result_vals <- unique(test_data[, result])
  if (! setequal(c(0,1), result_vals)){
    rlang::abort(
      message = paste0(
        'Results of each test must only be values 0 (negative test) ',
        'or 1 (positive test)'
      ),
      class = "DataCheck_invalid_results_values"
    )
  }
  
  # Check whether each row has a unique site column value
  # Complicated - cannot make assumptions about the rest of the data, which
  # means you cannot simply compare the expected and actual number of values
  # Need to think about this
  if (hier_check == TRUE & is.null(location)){
    rlang::abort(
      message = 'Must specify location in function call when "hier_check = TRUE"',
      class = "DataCheck_no_specified_location_cols")
  }
  
  # Return data, invisibly, if all checks succeed
  return(invisible(data))
}
