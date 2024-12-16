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
#' Given a dataframe and the hierarchy/sampling scheme, this function checks 
#' whether any values are incorrectly nested.
#' 
#' Only called from within \code{CheckClusterVars()}
#' 
#' @param data A \code{data.frame} with one row for each pooled sampled and
#'   columns for the size of the pool (i.e., the number of specimens/isolates/insects 
#'   pooled to make that particular pool), the result of the test of the
#'   pool. It may also contain additional columns with additional information
#'   (e.g. location where pool was taken) which can optionally be used for
#'   stratifying the data into smaller groups and calculating prevalence by
#'   group (e.g. calculating prevalence for each location).
#' @param hierarchy Names of columns in the hierarchy/clustering scheme, 
#' ordered from largest to smallest
#' 
#' @return Returns a \code{data.frame} containing details of incorrectly 
#' nested variables.
#'
#' @keywords internal
#' @noRd
#' 
check_nesting_levels <- function(data, hierarchy) {
  hier_df <- unique(data[, hierarchy])
  # Identify pairwise comparisons between adjacent hierarchy levels
  hier_list <- vector(mode="list", length = (length(hierarchy) - 1) )
  for (k in 1:length(hier_list)){
    # Use rev(hierarchy) so hierarchy columns ordered from smallest to largest 
    temp_list <- list("outer" = rev(hierarchy)[k+1],
                      "inner" = rev(hierarchy)[k],
                      "scheme" = hierarchy)
    hier_list[[k]] <- temp_list
  }
  # Check for inner level values with multiple values at the outer level
  check_df <- 
    as.data.frame(
      do.call(
        rbind, 
        lapply(
          1:length(hier_list),
          function(i){
            as.data.frame(
              do.call(
                rbind,
                lapply(
                  unique(hier_df[[hier_list[[i]]$inner]]), 
                  function(j){
                    list(
                      "inner" = hier_list[[i]]$inner,
                      "outer" = hier_list[[i]]$outer,
                      "inner_val" = j,
                      "outer_val" = unique(hier_df[(hier_df[[hier_list[[i]]$inner]] == j), hier_list[[i]]$outer])
                    )
                  }
                )
              )
            )
          }
        )
      )
    )
  check_df <- check_df %>%
    rowwise() %>%
    mutate(num_outer_val = length(outer_val), .keep = "all") %>%
    ungroup()
  return(check_df)
}  


#' Create pretty error message for incorrect nesting 
#' 
#' Only called from within \code{CheckClusterVars()}.
#' 
#' Takes the \code{data.frame} output by \code{check_nesting_levels()} and 
#' turns it into a pretty and informative error message that can be output 
#' to the user.
#' 
#' @param df A \code{data.frame} object. Output from \code{check_nesting_levels()}
#' 
#' @return Returns a \code{character} vector where each element is an error
#' message to return to the user.
#'
#' @keywords internal
#' @noRd
#' 
create_nesting_error_message <- function(df){
  cols_to_check_df <- unique(df[, c("inner", "outer")])
  col_names_warning <- unlist(
    lapply(
      1:nrow(cols_to_check_df),
      function(i){
        paste0(
          "Some values in the '", cols_to_check_df[i, "inner"], 
          "' column are present within multiple levels of the '",
          cols_to_check_df[i, "outer"], "' column."
        )
      }
    )
  )
  row_warnings <- df %>% 
    rowwise() %>%
    mutate(
      error_message = paste0(
        "'", .data$inner, "' value '", .data$inner_val, 
        "' appears in rows for multiple '", .data$outer, "' values: '", 
        paste(.data$outer_val, collapse = "', "), "'"
      ),
      .keep = "all"
    )
  all_warnings <- c(col_names_warning, row_warnings$error_message)
  return(all_warnings)
}


#' Create a unique identifier for each location, given the full 
#' hierarchical/clustering scheme
#' 
#' Only called from within \code{PrepareClusterData()}.
#' 
#' Takes the \code{data.frame} output by \code{check_nesting_levels()} and 
#' turns it into a pretty and informative error message that can be output 
#' to the user.
#' 
#' @param df_row A \code{data.frame} object with one row, taken from the 
#' input \code{data} to \code{PrepareClusterData()}.
#' @param cluster_hierarchy A \code{character} vector containing the full 
#' hierarchical/clustering scheme for the data, ordered from largest to 
#' smallest.
#' 
#' @return Returns a \code{character} vector of length 1, which is the new 
#' unique identifier for the location given by the input \code{df_row}
#'
#' @keywords internal
#' @noRd
#' 
create_unique_location_id <- function(df_row, cluster_hierarchy){
  hier_vals <- unlist(lapply(cluster_hierarchy, function(x){df_row[[x]]}))
  unique_id <- paste0(hier_vals, collapse = "_")
  return(unique_id)
}

