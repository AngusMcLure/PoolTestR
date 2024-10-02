## Functions for new class: HierPoolPrevOutput

# Constructor function
new_HierPoolPrevOutput <- function(x = tbl()) {
  stopifnot(is.tbl(x))
  
  prev_class <- class(x)
  structure(x,
            class = c("HierPoolPrevOutput", prev_class)
  )
}
