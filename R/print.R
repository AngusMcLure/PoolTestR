#' Print method for HierPoolPrevOutput objects
#' S3 method
#' 
#' @param object An object of class "HierPoolPrevOutput" as returned by HierPoolPrev().
#'
#' @return A \code{data.frame} output by HierPoolPrev, in a human readable format
#'
#' @seealso \code{\link{HierPoolPrev}}
#' 
#' @export print.HierPoolPrevOutput
#' 
#' @noRd
print.HierPoolPrevOutput <- function(x, ...) {
  # This function reformats HierPoolPrevOutput into a human-readable tibble
  icc_names <- attr(x$ICC, "dimnames")[[2]]
  trimmed_object <- x %>% 
    select(grep("ICC", names(x), value = T, invert = T))
  icc_tbls <- lapply(icc_names, extract_matrix_column_ICC, x)
  formatted_output <- as_tibble(bind_cols(trimmed_object, icc_tbls))
  print(formatted_output)
  return(invisible(formatted_output))
}

#' Extract the correlation columns for a single clustering variable
#' Internal function
#' 
#' @param cluster_var a single clustering variable (i.e., the name of the one matrix-column)
#' @param x an object of class "HierPoolPrevOutput"
#' 
#' @noRd
extract_matrix_column_ICC <- function(cluster_var, x){
  all_cluster_vars <- attr(x$ICC, "dimnames")[[2]]
  if (cluster_var %in% all_cluster_vars){
    matrix_cols <- x %>% 
      select(grep("ICC", names(x), value = T))
    i_cols <- as_tibble(lapply(names(matrix_cols), 
                               function(x){matrix_cols[[x]][ , which(all_cluster_vars == cluster_var)]}), 
                        .name_repair = "minimal")
    names(i_cols) <- paste0(cluster_var, ".", names(matrix_cols))
    return(i_cols)
  } else {
    return(NULL)
  }
}
