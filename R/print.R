#' Print method for HierPoolPrevOutput objects
#' 
#' @param object An object of class "HierPoolPrevOutput" as returned by HierPoolPrev().
#'
#' @return A \code{data.frame} with columns as output by HierPoolPrev, reformatted to be easily human-readable 
#'
#' @seealso \code{\link{HierPoolPrev}}
#' 
#' @export
#' 
#' @rdname print
#'
#' @examples
#' library(dplyr)
#' 
#' # Run HierPoolPrev, accounting for hierarchical sampling frame within each reason
#' prev <- HierPoolPrev(SimpleExampleData, Result, NumInPool, c("Village","Site"), Year)
#' 
#' # Print output
#' print(prev)

print.HierPoolPrevOutput <- function(object) {
  # This function 
  icc_names <- attr(object$ICC, "dimnames")[[2]]
  trimmed_object <- object %>% 
    select(grep("ICC", names(object), value = T, invert = T))
  icc_tbls <- lapply(1:length(icc_names), extract_matrix_column_ICC, icc_names, object)
  formatted_output <- bind_cols(trimmed_object, icc_tbls)
  return(formatted_output)
}

extract_matrix_column_ICC <- function(i, matrix_col_names, object){
  # This function returns the ICC columns for a single variable in one neat, human-readable data.frame
  # Input: an integer, the names of the matrix columns, and the HierPoolPrevOutput
  if (i <= length(matrix_col_names)){
    matrix_cols <- object %>% 
      select(grep("ICC", names(object), value = T))
    i_cols <- as_tibble(lapply(names(matrix_cols), function(x){matrix_cols[[x]][ , i]}), .name_repair = "minimal")
    names(i_cols) <- paste0(matrix_col_names[i], ".", names(matrix_cols))
    return(i_cols)
  } else {
    return(NULL)
  }
}
