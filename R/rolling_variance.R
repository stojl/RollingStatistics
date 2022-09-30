#' Rolling variance
#'
#' A function for creating a rolling variance object.
#'
#' @return A function that takes a numeric vector as argument
#' @param df Degrees of freedom used in the empirical estimate.
#' @export
#'
#' @examples
#' a <- c(1, 2, 3, 4, 5)
#' b <- c(4, 2, 5, 1, 3)
#' sigma <- rolling_variance()
#' sigma(a) # Returns the variance of vector 'a'
#' sigma()  # Returns the current variance - the variance of vector 'a'
#' sigma(b) # Returns the variance of vector c(a, b)
rolling_variance <- function(df = 1) {
  force(df)
  m <- 0
  SSQ <- 0
  S <- 0

  function(x = NULL) {
    if(!is.null(x)) {
      m <<- m + length(x)
      SSQ <<- SSQ + sum(x^2)
      S <<- S + sum(x)
    }

    if(m > df) {
      V1 <- SSQ / (m - df)
      V2 <- S^2 / (m * (m - df))

      V1 - V2
    } else {
      NaN
    }
  }
}
