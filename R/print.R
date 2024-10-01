#' Print method for HierPoolPrevOutput objects
#' 
#' @param object An object of class "HierPoolPrevOutput" returned by HierPoolPrev().
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
  icc_inds <- 1:length(icc_names)
  trimmed_object <- object %>% 
    select(grep("ICC", names(object), value = T, invert = T))
  icc_dfs <- lapply(icc_inds, extract_matrix_column_ICC, icc_names, object)
  formatted_object <-  cbind(trimmed_object, icc_dfs) %>%
    mutate(
      across(where(~ is.numeric(.x)) && min(.x) >= 0.01, ~ digits(.x, 4)),
      across(where(~ is.numeric(.x) && min(.x) < 0.01), ~ signif(.x, digits = 3))
    )
  print(formatted_object) # After formatting, use print method for data.frame
}

extract_matrix_column_ICC <- function(i, icc_names, object){
  # This function returns the ICC columns for a single variable in one neat, human-readable data.frame
  # Input: an integer, the names of the matrix columns, and the HierPoolPrevOutput
  if (i <= length(icc_names)){
    icc_cols <- object %>% 
      select(grep("ICC", fonames(object), value = T))
    i_cols <- as.data.frame(lapply(names(icc_cols), function(x){icc_cols[[x]][ , i]}))
    names(i_cols) <- paste0(icc_names[i], ".", names(icc_cols))
    return(i_cols)
  } else {
    return(NULL)
  }
}
