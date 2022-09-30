#' Rolling mean
#'
#' A function for creating a rolling mean object.
#'
#' @return A function that takes a numeric vector as argument
#' @export
#'
#' @examples
#' a <- c(1, 2, 3, 4, 5)
#' b <- c(4, 2, 5, 1, 3)
#' mu <- rolling_mean()
#' mu(a) # Returns the mean of vector 'a'
#' mu()  # Returns the current mean - the mean of vector 'a'
#' mu(b) # Returns the mean of vector c(a, b)
rolling_mean <- function() {
  m <- 0
  S <- 0

  function(x = NULL) {
    if(!is.null(x)) {
      m <<- m + length(x)
      S <<- S + sum(x)
    }

    if(m > 0) {
      S / m
    } else {
      NaN
    }
  }
}
