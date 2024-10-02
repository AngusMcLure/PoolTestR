#' Constructor for HierPoolPrevOutput class
#' Allows for nicely-formatted human-readable output using a custom \code{print} method 
#' Internal function
#' 
#' @param x a tibble output by the \code{\link{HierPoolPrev}} function
#' 
#' @noRd
new_HierPoolPrevOutput <- function(x = tbl()) {
  stopifnot(is.tbl(x))
  
  prev_class <- class(x)
  structure(x,
            class = c("HierPoolPrevOutput", prev_class)
  )
}
