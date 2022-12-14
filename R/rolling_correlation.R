#' Rolling correlation
#'
#' A function for creating a rolling correlation object. Beware, that the
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
#' correlation <- rolling_variance()
#'
#' # Returns the correlation of vector 'a' and vector 'b'
#' correlation(a, b)
#'
#' # Returns the current correlation - the correlation of vector 'a'
#' # and vector 'b'.
#' correlation()
#'
#' # Returns the correlation of vector c(a, a) and vector c(b, c)
#' correlation(a, c)
rolling_correlation <- function(df = 1) {
  force(df)

  sd_x <- rolling_variance(df)
  sd_y <- rolling_variance(df)
  corr_xy <- rolling_covariance(df)

  function(x = NULL, y = NULL) {

    corr_xy(x, y) / sqrt(sd_x(x) * sd_y(y))

  }
}
