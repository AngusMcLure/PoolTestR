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
#' @param hierarchy Optional name(s) of columns included in the hierarchical sampling
#'   scheme, listed from largest to smallest. Only used if 
#'   \code{hier_check = TRUE}.
#' @param hier_check Logical. Default is \code{FALSE}. If \code{TRUE}, checks
#'  the hierarchy using column names provided in \code{hierarchy}.
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

CheckInputData <- function(data, result, poolSize, hierarchy = NULL,  
                           hier_check = FALSE, excludeCols = NULL){
  # Extract name(s) of columns to group by
  if (is.null(hierarchy) == FALSE){
    groupVar <- as.character(hierarchy)  
  }
  
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
  
  # Check whether rows have missing values
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
  # Check whether result and poolSize columns are present
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
  
  # If running hierarchy/clustering checks, ensure groupVar has been provided 
  if (hier_check == TRUE & is.null(hierarchy)) {
    rlang::abort(
      message = "No hierarchical/sampling variable included in function call.",
      class = c("DataCheck_missing_hierarchy", "error", "condition")
    )
  }
  
  # Check hierarchy/clustering scheme
  if (hier_check == TRUE & length(groupVar) > 0) {
    hier_check <- CheckClusterVars(data = test_data, 
                                   result = result, poolSize = poolSize,
                                   hierarchy = groupVar)
  }
  
  # Return data, invisibly, if all checks succeed
  return(invisible(data))
}


#' Checking hierarchy columns for missing values and incorrect nesting
#' 
#' 
#' Internal function to test the input data for problems with the 
#' hierarchy/sampling scheme and raise any relevant errors/warnings.
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
#' @param hierarchy Optional name(s) of columns with variables to stratify the data
#' by. If omitted the complete dataset is used to estimate a single
#' prevalence. If included, prevalence is estimated separately for each group
#' defined by these columns
#'
#' @return Returns \code{data} invisibly, using \code{invisible(x)}
#' 
#' For the \code{SimpleExampleData} data included in this package, the 
#' hierarchical sampling scheme is \code{Region} > \code{Village} > \code{Site}.
#' The function call would be:
#' \code{checkClusterVars(data = SimpleExampleData, 
#' result = "Result", poolSize = "NumInPool"
#' hierarchy = c("Region", "Village", "Site"))}
#'
#' @keywords internal
#' @noRd
CheckClusterVars <- function(data, result, poolSize, hierarchy = NULL){
  if (is.null(hierarchy)){
    rlang::abort(
      message = "No hierarchical/sampling variable included in function call.",
      class = c("CheckClusterVars_no_hierarchy", "error", "condition")
    )
  } else {
    groupVar <- as.character(hierarchy)  
  }
  
  ## Check all hierarchy columns exist
  missing_hier_cols <- groupVar[! (groupVar %in% names(data))]
  if (! identical(character(0), missing_hier_cols) ) {
    rlang::abort(
      message = paste0(
        'Data frame does not include the following columns: ',
        paste(missing_hier_cols, collapse = ", ")
      ),
      class = c("CheckClusterVars_missing_cols", "error", "condition"),
      missing_cols = missing_hier_cols
    )
  }
  
  ## Flag missing values in cluster/hierarchy columns
  missing_list <- vector(mode="list", length=length(groupVar))
  names(missing_list) <- groupVar
  for (x in groupVar){
    x_vals <- data[, x]
    if (class(x_vals) == "character"){
      # Missing char vals = "", NA, NULL
      missing_x_vals <- which(x_vals == "" | is.na(x_vals) | is.null(x_vals))
    } else {
      # Missing vals = NA, NULL
      missing_x_vals <- which(is.na(x_vals) | is.null(x_vals))
    }
    # Check length of vector as columns can have different classes
    if (length(missing_x_vals) == 0) {
      missing_list[[x]] <- NA
    } else if (length(missing_x_vals) == length(x_vals)){
      missing_list[[x]] <- "all values missing"
    } else {
      missing_list[[x]] <- missing_x_vals
    }
  }
  bad_cols <- groupVar[! unlist(lapply(groupVar, function(x){is.na(missing_list[[x]])}))]
  if (! identical(character(0), bad_cols) ){
    output_list <- missing_list[bad_cols]
    output_messages <- unlist(
      lapply(
        names(output_list), 
        function(x){paste0(x, ": ", paste(output_list[[x]], collapse = ", "))}
      )
    )
    rlang::abort(
      message = paste0("Some columns in the hierarchy/sampling scheme have missing values ",
                       "(i.e., NA, NULL or empty strings)\n",
                       paste(output_messages, collapse = "\n")),
      class = c("CheckClusterVars_missing_vals", "error", "condition"),
      missing_vals = output_list
    )
  }
  
  ## Check nesting within hierarchy/sampling scheme
  check_nests <- check_nesting_levels(data = data, hierarchy_scheme = groupVar)
  nesting_errors_df <- as.data.frame(check_nests[which(check_nests$num_outer_val > 1), ])
  if (nrow(nesting_errors_df) > 0){
    rlang::warn(
      message = paste(
        c("Hierarchy/clustered sampling scheme is not nested correctly.",
          create_nesting_error_message(nesting_errors_df)), 
        collapse = "\n"
      ),
      class = c("CheckClusterVars_nesting", "warning", "condition"),
      missing_vals = nesting_errors_df
    )
  }
  
  # Return data, invisibly, if checks succeeds
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
#' @param hierarchy The name of column(s) indicating the group membership, 
#'   ordered from largest to smallest. 
#'
#' @return An object of class \code{data.frame}, containing the same columns as
#' the input, with a single new column \code{PoolTestR_ID} containing a unique 
#' identifier for each location in the survey.
#' 
#' In a nested sampling design with multiple levels of grouping, the 
#' lower-level groups must have names/numbers that differentiate them from all 
#' other groups at the same level. E.g. If sampling was performed at 200 sites 
#' across 10 villages (20 site per village), then there should be 200 unique 
#' names for the sites. If, for instance, the sites are instead numbered 1 to 
#' 20 within each village, the village identifier (e.g. A, B, C...) should be 
#' combined with the site number to create unique identifiers for each site 
#' (e.g. A-1, A-2... for sites in village A and B-1, B-2... for the sites in 
#' village B etc.)
#' 
#' The functions in PoolTools apply for all cases where a hierarchical sampling 
#' frame is involved, and do not make assumptions about the number of levels 
#' present or the names of hierarchical columns. 
#' 
#' For the \code{SimpleExampleData} data included in this package, the 
#' hierarchical sampling scheme is \code{Region} > \code{Village} > \code{Site}.
#' The function call would be:
#' \code{prepareClusterData(SimpleExampleData, "Result", "NumInPool",
#' c("Region", "Village", "Site"))}
#
#' @export
#' 
#' @seealso \code{\link{HierPoolPrev}}, \code{\link{getPrevalence}}
#'
# TODO Need to add examples here!
# #' @examples
# TODO Complete documentation and remove example from doco, use example from @examples instead
PrepareClusterData <- function(data, result, poolSize, hierarchy = NULL){
  
  if (is.null(hierarchy)){
    rlang::abort(
      message = "No hierarchical/sampling variable included in function call.",
      class = c("PrepareClusterData_no_hierarchy", "error", "condition")
    )
  } else {
    groupVar <- as.character(hierarchy)  
  }
  
  # Check for warnings and errors
  CheckInputData(data, result, poolSize, groupVar,  
                 hier_check = TRUE, excludeCols = NULL)
  CheckClusterVars(data, result, poolSize, groupVar)
  
  # Add unique identifier for each location by pasting groupVar columns
  new_data <- data %>%
    rowwise() %>%
    mutate(PoolTestR_ID = create_unique_location_id(.data, groupVar),
           .keep = "all")
  return(new_data)
}

