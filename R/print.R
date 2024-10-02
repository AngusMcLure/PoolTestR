#' Print method for HierPoolPrevOutput objects
#' 
#' @param object An object of class "HierPoolPrevOutput" as returned by HierPoolPrev().
#'
#' @return A \code{data.frame} output by HierPoolPrev, in a human readable format
#'
#' @seealso \code{\link{HierPoolPrev}}, \code{\link{print}}
#' 
#' @export print.HierPoolPrevOutput
#' 
#' @rdname print
#'

print.HierPoolPrevOutput <- function(x, ...) {
  # This function reformats HierPoolPrevOutput into a human-readable tibble
  icc_names <- attr(x$ICC, "dimnames")[[2]]
  trimmed_object <- x %>% 
    select(grep("ICC", names(x), value = T, invert = T))
  icc_tbls <- lapply(1:length(icc_names), extract_matrix_column_ICC, icc_names, x)
  formatted_output <- as_tibble(bind_cols(trimmed_object, icc_tbls))
  print(formatted_output)
  return(invisible(formatted_output))
}

extract_matrix_column_ICC <- function(i, matrix_col_names, x){
  # This function returns the ICC columns for a single variable in one neat, human-readable data.frame
  # Input: an integer, the names of the matrix columns, and the HierPoolPrevOutput
  if (i <= length(matrix_col_names)){
    matrix_cols <- x %>% 
      select(grep("ICC", names(x), value = T))
    i_cols <- as_tibble(lapply(names(matrix_cols), function(x){matrix_cols[[x]][ , i]}), .name_repair = "minimal")
    names(i_cols) <- paste0(matrix_col_names[i], ".", names(matrix_cols))
    return(i_cols)
  } else {
    return(NULL)
  }
}
