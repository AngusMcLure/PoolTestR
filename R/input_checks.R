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
#'
#' @return Returns \code{data} invisibly, using \code{invisible(x)}
#'
#' @keywords internal
#' @noRd

check_data <- function(data, result, poolSize){
  print(data)
  return(invisible(x))
}
