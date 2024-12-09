#' Extract the correlation columns for a single clustering variable
#'
#' @param cluster_var a single clustering variable (i.e., the name of the one
#'  matrix-column)
#' @param x an object of class "HierPoolPrevOutput"
#'
#' @keywords internal
#' @noRd
#' 
extract_matrix_column_ICC <- function(cluster_var, x){
  all_cluster_vars <- attr(x$ICC, "dimnames")[[2]]
  if (cluster_var %in% all_cluster_vars){
    # Extract only the columns for this clustering variable
    matrix_cols <- x %>%
      select(grep("ICC", names(x), value = TRUE))
    cluster_cols <- tibble::as_tibble(
      lapply(
        names(matrix_cols),
        function(x){matrix_cols[[x]][ , which(all_cluster_vars == cluster_var)]}
      ),
      .name_repair = "minimal")
    names(cluster_cols) <- paste0(cluster_var, ".", names(matrix_cols))
    formatted_cluster_cols <- pretty_format_ICC_column(cluster_cols) # Pretty print
    return(formatted_cluster_cols)
  } else {
    return(NULL)
  }
}


#' Take a column and its confidence/credibility intervals and format into
#'  pretty, human-readable format
#'
#' @param var_df a \code{data.frame} with three columns:
#'   \itemize{\item{Parameter estimate (e.g., PrevMLE, PrevBayes, ICC)}
#'            \item{Lower confidence interval (e.g., CILow) or credibility
#'              interval (e.g., CrILow)}
#'            \item{Upper confidence interval (e.g., CIHigh) or credibility
#'              interval (e.g., CrIHigh)}
#'            }
#'
#' @keywords internal
#' @noRd
pretty_format_ICC_column <- function(var_df){
  # Record original column names
  col_names <- names(var_df)
  # Set new column names
  names(var_df)[grep("low", names(var_df), ignore.case = TRUE)] <- "low"
  names(var_df)[grep("high", names(var_df), ignore.case = TRUE)] <- "high"
  names(var_df)[grep("low|high", names(var_df), ignore.case = TRUE, invert = TRUE)] <- "param"
  # Create formatted output column
  formatted_df <- var_df %>% 
    mutate(
      output = paste0(" ",
                      custom_round(.data$param),
                      " (",
                      custom_round(.data$low),
                      " - ",
                      custom_round(.data$high),
                      ")"),
      .keep = "none"
    )
  # Rename new column as "ICC.<cluster.name>"
  names(formatted_df) <- grep("low|high", col_names, ignore.case = TRUE, invert = TRUE, value = TRUE)
  return(formatted_df)
}


#' Custom round for ICC columns - maintain 4 sig figs across different
#' magnitudes of values
#'
#' @param x a vector of numeric values
#'
#' @keywords internal
#' @noRd
#' 
custom_round <- function(x) {
  min_x <- min(x)
  # Get absolute value of greatest magnitude in column
  #   (e.g., for 0.01, magnitude = -2 and absolute magnitude = 2)
  biggest_mag <- max(abs(round(log10(x))))
  if (biggest_mag <= 3){
    # Round to 4dp
    x_formatted <- sprintf(paste0("%.", 3, "f"), round(x, 3) )
  } else {
    # Scientific format
    x_formatted <- sprintf(paste0("%.", (3 - 1), "e"), signif(x, 3) )
  }
  return(x_formatted)
}


#' Check nesting for hierarchy/cluster variables within a data frame
#' 
#' Given a dataframe and two columns from the hierarchy/
#' sampling scheme, this function checks whether any variables in the inner
#' level are present for multiple outer levels.
#' 
#' @param data A \code{data.frame} with one row for each pooled sampled and
#'   columns for the size of the pool (i.e., the number of specimens / isolates /
#'   insects pooled to make that particular pool), the result of the test of the
#'   pool. It may also contain additional columns with additional information
#'   (e.g. location where pool was taken) which can optionally be used for
#'   stratifying the data into smaller groups and calculating prevalence by
#'   group (e.g. calculating prevalence for each location).
#' @param outer_cluster The outer (i.e., larger) cluster. Must be a column name 
#' from \code{data}.
#' @param inner_cluster The inner (i.e., smaller) cluster. Nested within the 
#' \code{outer_cluster}. Must be a column name from \code{data}.
#' 
#' Assume we have Houses 1 and 2 in Village A and Houses 1 and 2 in Village B. 
#' This function will return the values 1 and 2 for the Houses level, as there 
#' are identical houses in both Villages.
#' 
#' To correct this, each location (i.e., in this example each House) should have
#' a unique identifier. The houses could have Village names appended (e.g., A-1,
#' A-2, B-1, B-2) or each house could be numbered sequentially (e.g., 1, 2, 3, 
#' 4).
#' 
#' In the example above the \code{outer_cluster} is "Village" and the 
#' \code{inner_cluster} is "House".
#' 
#' As in \code{PrepareClusterData()} and \code{CheckClusterVars()}, the 
#' clustering/hierarchical columns are input into the function from largest to 
#' smallest.
#'
#' @keywords internal
#' @noRd
#' 
check_nesting_levels <- function(data, outer_cluster, inner_cluster) {
  # Keep only unique rows for the two columns in the inner/outer cluster 
  unique_df <- unique(data[, c(outer_cluster, inner_cluster)])
  inner_vals <- unique(unique_df[[inner_cluster]])
  # Check that the values of the inner_cluster column are present only in one
  # outer_cluster 
  inner_list <- vector(mode="list", length = length(inner_vals))
  for (i in 1:length(inner_list) ){
    i_inner <- inner_vals[[i]]
    i_outer <- unique_df[which(unique_df[[inner_cluster]] == i_inner), outer_cluster]
    inner_list[[i]] <- list("inner_vals" = i_inner,
                            "outer_vals" = i_outer,
                            "inner_cluster" = inner_cluster,
                            "outer_cluster" = outer_cluster)
  }
  length_outer_vals <- 
    unlist(
      lapply(
        1:length(inner_list),
        function(x){
          length(inner_list[[i]]$outer_vals)
        }
      )
    )
  repeated_inners <- which(length_outer_vals > 1)
  if (length(repeated_inners) > 0){
    repeated_inner_list <- inner_list[repeated_inners]
    op <- as.data.frame(do.call(rbind, repeated_inner_list))
  } else {
    op <- NULL
  }
  return(op)
}  

