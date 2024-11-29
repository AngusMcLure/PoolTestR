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
#'   group (e.g. calculating prevalence for each location)
#' @param result The name of column with the result of each test on each pooled
#'   sample. The result must be stored with 1 indicating a positive test result
#'   and 0 indicating a negative test result.
#' @param poolSize The name of the column with number of
#'   specimens/isolates/insects in each pool
#' @param hier_check Logical. Default is \code{FALSE}. If \code{TRUE}, checks
#'  whether each row in the specified \code{location}
#' @param location The name of the column that uniquely identifies each 
#' location. Must be specified when \code{hier_check = TRUE}.
#'
#' @return Returns \code{data} invisibly, using \code{invisible(x)}
#'
#' @keywords internal
#' @noRd

check_data <- function(data, result, poolSize, 
                       hier_check = FALSE, location = NULL){
  # Check whether result and poolSize variables are present within column names
  if (! (result %in% names(data)) ){
    stop("result column not included in dataframe")
  }
  if (! (poolSize %in% names(data)) ){
    stop("poolSize column not included in dataframe")
  }
  
  # Check whether dataframe columns are all strings
  stop("All dataframe columns are strings.")
  
  # Check whether note rows are appended at bottom of dataframe
  # i.e., whether rows are present that have missing values
  stop("Some rows have missing values")
  
  # Check whether empty rows are present
  warning("Empty rows are present within the dataset.")
  
  # Check whether result column contains only 0 and 1
  result_vals <- unique(data[, result])
  if (! setequal(c(0,1), result_vals)){
    stop("Results of each test must only be stored as 0 (negative test) or 1 (positive test)")
  }
  
  # Check whether each row has a unique site column value
  # Complicated - cannot make assumptions about the rest of the data, which
  # means you cannot simply compare the expected and actual number of values
  # Need to think about this
  if (hier_check == TRUE & is.null(location)){
    stop('Must specify location in function call when "hier_check = TRUE"')
  }
  
  # Return data, invisibly, if all checks succeed
  return(invisible(data))
}
