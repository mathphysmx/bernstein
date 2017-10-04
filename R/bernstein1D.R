#' @title bernstein1D
#'
#' @description Univariate Bernstein-Bezier function.
#'
#' @param x Numeric scalar in the closed interval [0,1]. The value at which the function will be approximated.
#' @param y Numeric vector. The function evaluated at a regular partition of the closed interval [0,1].
#'
#' @return Numeric scalar, the value of the Bernstein-Bezier curve.
#' @export
#'
#' @importFrom "stats" "dbinom"
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#' @examples
#' # Example 1:
#' ex <- seq(0,1, length.out=5)
#' ey <- sin(2*pi*ex); plot(ex, ey)
#' exB <- seq(0,1, by = 0.01)
#' eyB <- sapply(exB, bernstein1D, y = ey)
#' points(exB,eyB, pch = "+")
#'
#' # Example 2 (Bernstein-Kantorovich)
#' eyE <- c(ey[1], (ey[-1] + head(ey, -1))/2, tail(ey, 1))
#' eyEB <- sapply(exB, bernstein1D, y = eyE)
bernstein1D <- function (x,y) {
	n <- length(y) - 1
	Bu <- dbinom(x = 0:n, size = n, prob = x)
	fn <- sum(y * Bu)
	return(fn)
}
