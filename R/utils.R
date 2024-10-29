#' Extract the correlation columns for a single clustering variable
#'
#' @param cluster_var a single clustering variable (i.e., the name of the one
#'  matrix-column)
#' @param x an object of class "HierPoolPrevOutput"
#'
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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
