#' Rolling covariance
#'
#' A function for creating a rolling covariance object. Beware, that the
#' function returned is not symmetric.
#'
#' @return A function that takes a numeric vector as argument
#' @param df Degrees of freedom used in the empirical estimate.
#' @export
#'
#' @examples
#' a <- c(1, 2, 3, 4, 5)
#' b <- c(4, 2, 5, 1, 3)
#' c <- c(6, 4, 10, 4, 6)
#' covariance <- rolling_variance()
#'
#' # Returns the covariance of vector 'a' and vector 'b'
#' covariance(a, b)
#'
#' # Returns the current covariance - the covariance of vector 'a'
#' # and vector 'b'.
#' covariance()
#'
#' # Returns the covariance of vector c(a, a) and vector c(b, c)
#' covariance(a, c)
rolling_covariance <- function(df = 1) {
  force(df)
  m <- 0
  SP <- 0
  S1 <- 0
  S2 <- 0

  function(x = NULL, y = NULL) {
    if(length(x) != length(y)) {
      stop(paste0("Vectors must be of equal length. ",
                  "Vector of length ", length(x),
                  " is not equal to vector of length ", length(y),
                  "."))
    }

    if(!is.null(x) & !is.null(y)) {
      m <<- m + length(x)
      SP <<- SP + sum(x * y)
      S1 <<- S1 + sum(x)
      S2 <<- S2 + sum(y)
    }

    if(m > df) {
      CV1 <- SP / (m - df)
      CV2 <- S1 * S2 / (m * (m - df))

      CV1 - CV2
    } else {
      NaN
    }
  }
}
