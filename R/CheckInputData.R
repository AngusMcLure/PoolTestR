#' Checking input data for errors and inconsistencies
#' 
#'
#' Test the input data is adequately formatted for use with \code{PoolPrev()} and 
#' \code{HierPoolPrev()}. 
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
#'   by.
#'
#' @return Returns \code{data} invisibly, using \code{invisible(x)}
#' 
#' This function is used to check the input \code{data} for formatting
#' problems including:
#'   \itemize{
#'     \item{Incorrect class of the input \code{data} }
#'     \item{incorrect class of \code{result} and \code{poolSize} columns}
#'     \item{Missing columns}
#'     \item{Missing values in rows}
#'     \item{Invalid values in rows}
#'   }
#' If any problems are detected, an error or warning will be raised describing
#' the issue.
#'
#' @seealso \code{\link{PrepareClusterData}}, \code{\link{SimpleExampleData}},
#' \code{\link{PoolPrev}}, \code{\link{HierPoolPrev}}
#' 
#' @example examples/CheckInputData.R
#'
#' @export

CheckInputData <- function(data, result, poolSize, ...){
  groupVar <- unlist(list(...)) # optional name(s) of columns with other variable to group by
  
  ## Errors
  # Check class of result and poolSize
  if (
    is(result, "character") == FALSE  & 
    is(poolSize, "character") == FALSE
  ) {
    rlang::abort(
      message = 'Input variables result and poolSize should be of class "character"',
      class = c("DataCheck_input_class", "error", "condition")
    )
  } else if (
    is(result, "character") == FALSE  & 
    is(poolSize, "character") == TRUE
  ) {
    rlang::abort(
      message = 'Input variable result should be of class "character"',
      class = c("DataCheck_input_class", "error", "condition")
    )
  }else if (
    is(result, "character") == TRUE  & 
    is(poolSize, "character") == FALSE
  ) {
    rlang::abort(
      message = 'Input variable poolSize should be of class "character"',
      class = c("DataCheck_input_class", "error", "condition")
    )
  }
  
  # Check class of input data
  if (! inherits(SimpleExampleData, "data.frame")){
    rlang::abort(
      message = 'Input data should be class "data.frame"',
      class = c("DataCheck_input_class", "error", "condition")
    )
  }
  
  # Check whether result and poolSize columns are present
  if ( 
    (result %in% names(data) == FALSE) &
    (poolSize %in% names(data) == FALSE )
  ) {
    rlang::abort(
      message = "result and poolSize columns not included in dataframe. Check column names.",
      class = c("DataCheck_missing_column", "error", "condition")
    )
  } else if ( 
    (result %in% names(data) == FALSE) &
    (poolSize %in% names(data) == TRUE )
  ) {
    rlang::abort(
      message = "result column not included in dataframe. Check column name.",
      class = c("DataCheck_missing_column", "error", "condition")
    )
  } else if ( 
    (result %in% names(data) == TRUE) &
    (poolSize %in% names(data) == FALSE )
  ) {
    rlang::abort(
      message = "poolSize column not included in dataframe. Check column name.",
      class = c("DataCheck_missing_column", "error", "condition")
    )
  }
  
  # Check whether grouping columns are present
  if (! is.null(groupVar)){
    missing_groupVar <- groupVar[! groupVar %in% names(data)]
    if (! identical(character(0), missing_groupVar)){
      rlang::abort(
        message = paste0(
          'Stratification column not present within data\n',
          'Missing column(s): ', paste(missing_groupVar, collapse = ", ")
        ),
        class = c("DataCheck_missing_groupVar", "error", "condition")
      )
    }
  }
  
  # Check whether results column values are numeric or integer
  if (! 
      (
        inherits(data[,result], "integer") || 
        inherits(data[,result], "numeric")
      )  
  ){
    rlang::abort(
      message = paste0(
        'Results of each test must be stored as class "numeric" or "integer" ',
        'with only values 0 (negative test) or 1 (positive test)'
      ),
      class = c("DataCheck_col_not_numeric", "error", "condition")
    )
  }
  
  # Check whether poolSize column values are numeric or integer
  if (! 
      (
        inherits(data[,poolSize], "integer") || 
        inherits(data[,poolSize], "numeric")
      )
  ){
    rlang::abort(
      message = 'Pool size column should be positive values of class "numeric" or "integer"',
      class = c("DataCheck_col_not_numeric", "error", "condition")
    )
  }
  
  # Check whether poolSize column values are positive
  negative_pool_size <- which(data[,poolSize] < 0)
  if (! identical(integer(0), negative_pool_size) ){
    rlang::abort(
      message = 'Pool size column should contain only positive values',
      class = c("DataCheck_col_not_positive", "error", "condition")
    )
  }
  
  # Check whether result column contains only 0 and 1
  result_vals <- unique(data[, result])
  allowed_result_vals <- c(0, 1)
  check_result_vals <- setdiff(result_vals, allowed_result_vals) # output vector of values that aren't 0 and 1
  if (! identical(check_result_vals, numeric(0)) ){
    rlang::abort(
      message = paste0(
        'Results of each test must only be values 0 (negative test) ',
        'or 1 (positive test)'
      ),
      class = c("DataCheck_invalid_results_values", "error", "condition")
    )
  }
  
  ## Warnings
  # Check whether empty rows are present
  empty_rows <- which(apply(data == "", 1, all) == TRUE)
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
  NA_rows <- which(apply(is.na(data), 1, all) == TRUE)
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
        which(rowSums(is.na(data)) != 0  
              & rowSums(is.na(data)) < ncol(data)),
        which(rowSums(data == "") != 0  
              & rowSums(data == "") < ncol(data))
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
    hierarchy <- as.character(hierarchy)
  }
  
  ## Check class of input data
  if (! inherits(data, "data.frame")){
    rlang::abort(
      message = 'Input data should be class "data.frame"',
      class = c("CheckClusterVars_input_class", "error", "condition")
    )
  }
  
  ## Check all hierarchy columns exist
  missing_hier_cols <- hierarchy[! (hierarchy %in% names(data))]
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
  missing_list <- vector(mode="list", length=length(hierarchy))
  names(missing_list) <- hierarchy
  for (x in hierarchy){
    x_vals <- data[, x]
    if ( is(x_vals, "character") ){
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
  bad_cols <- hierarchy[! unlist(lapply(hierarchy, function(x){is.na(missing_list[[x]])}))]
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
  
  ## If hierarchy has >1 column, check nesting within hierarchy/sampling scheme 
  if (length(hierarchy) > 1){
    check_nests <- check_nesting_levels(data = data, hierarchy = hierarchy)
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
  }
  
  # Return data, invisibly, if checks succeeds
  return(invisible(data))
}


#' Prepare hierarchical/clustered survey data for analysis
#' 
#' Helper function to prepare data for analysis with \code{HierPoolPrev()}.
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
#' @param ... Optional name(s) of columns with variables to stratify the data
#'   by.
#'
#' @return An object of class \code{data.frame}, identical to the input 
#' \code{data}. If there were issues with nesting inside the hierarchy, the 
#' output will have a single additional column \code{PoolTestR_ID}, containing 
#' unique identifier for each location in the survey (created by concatenating
#' the hierarchy column values within each row).
#' 
#' This function can be used to check levels of the hierarchical/clustering 
#' scheme. For example, the \code{SimpleExampleData} has the scheme 
#' \code{Region} > \code{Village} > \code{Site}. The full geographical 
#' hierarchy/clustering scheme can be tested using 
#' \code{hierarchy = c("Region", "Village", "Site")}. This function can also be 
#' used to check only the levels of the hierarchical/clustering scheme that 
#' will be included in prevalence estimates, e.g., if planning to stratify 
#' \code{SimpleExampleData} by \code{Region}, the hierarchy will be 
#' \code{hierarchy = c("Village", "Site")}.
#' 
#' Functions in PoolTools do not make assumptions about the number of levels 
#' present or the names of hierarchical columns. They can be applied in any
#' cases where a hierarchical sampling frame is involved. 
#' 
#' @details When including information about hierarchical sampling frame 
#' (e.g. where units were sampled from sites and sites were selected from a 
#' broader region), it is critical that each cluster can be uniquely identified. 
#' It's not enough for the combination of columns specifying the hierarchy to 
#' be unique. Each different location needs to have a unique label. 
#' It's not enough for the combination of columns specifying the hierarchy to 
#' be unique. Each different location needs to have a unique label.
#' 
#' This function is provided to assist users by detecting certain types of 
#' nesting and clustering issues within data. As we do not make assumptions 
#' about the hierarchical/clustering scheme, we cannot guarantee that 
#' this function will detect all errors in hierarchical/clustering schemes.
#' This function checks for the most common type of error, which is multiple 
#' different locations (e.g., collection sites) having the same value within a 
#' hierarchical/clustering column. 
#' 
#' @seealso \code{\link{CheckInputData}}, \code{\link{HierPoolPrev}}, 
#' \code{\link{getPrevalence}}, \code{\link{SimpleExampleData}}
#' 
#' @example examples/PrepareUserData.R
#'
#' @export
#' 
PrepareClusterData <- function(data, result, poolSize, 
                               hierarchy = NULL, ...){
  # groupVar not used in this function - doesn't impact clustering
  # Allow it as input to keep all functions consistent
  groupVar <- unlist(list(...)) # optional name(s) of columns with other variable to group by
  
  if (is.null(hierarchy)){
    rlang::abort(
      message = "No hierarchical/sampling variable included in function call.",
      class = c("PrepareClusterData_no_hierarchy", "error", "condition")
    )
  } else {
    hierarchy <- as.character(hierarchy)  
  }
  
  ## Check all hierarchy columns exist
  missing_hier_cols <- hierarchy[! (hierarchy %in% names(data))]
  if (! identical(character(0), missing_hier_cols) ) {
    rlang::abort(
      message = paste0(
        'The hierarchy includes column names that are not present within the input data\n',
        'Column name(s): ',
        paste(missing_hier_cols, collapse = ", ")
      ),
      class = c("PrepareClusterData_missing_hier_cols", "error", "condition"),
      missing_cols = missing_hier_cols
    )
  }
  
  # Check for warnings and errors in the input data
  CheckInputData(data, result, poolSize, groupVar)
  
  # Check for warnings and errors in the hierarchical/clustering scheme
  # If nesting warning is present, add new column of unique location identifiers
  pooltestr_input <- tryCatch(
    expr = {
      op_data <- CheckClusterVars(data, result, poolSize, hierarchy)
    },
    error = function(e){
      e_message <- utils::capture.output(e)
      rlang::abort(message = e_message,
                   class = c("PrepareClusterData_error", "warning", "condition"))
      op_data <- NA
      return(op_data)
    },
    warning = function(w) {
      w_message <- utils::capture.output(w)
      if ("CheckClusterVars_nesting" %in% class(w)){
        w_message <- gsub("<|>|CheckClusterVars_nesting: ", "", w_message)
        rlang::warn(message = w_message,
                    class = c("PrepareClusterData_nesting", "warning", "condition"))
        rlang::inform("Note: Unique identification for each location added in new dataframe column `PoolTestR_ID`",
                      class = c("PrepareClusterData_output", "message", "condition"))
        op_data <- data %>%
          rowwise() %>%
          mutate(PoolTestR_ID = create_unique_location_id(.data, hierarchy),
                 .keep = "all")
      } else {
        rlang::warn(message = w_message,
                    class = c("PrepareClusterData_warning", "warning", "condition"))
        op_data <- NA
      }
      return(op_data)
    }
  )
  
  return(pooltestr_input)
}

