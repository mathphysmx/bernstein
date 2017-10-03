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
#' ex <- seq(0,1, length.out=5)
#' eF <- sin(2*pi*ex); plot(ex, eF)
#' exB <- seq(0,1, by = 0.01)
#' for(i in 1:length(exB)){
	#i <- 1
	#' eFB <- bernstein1D(x=exB[i], y=eF);
	#' points(exB[i], eFB, pch = '+')
#' }
#'
bernstein1D <- function (x,y) {
	n <- length(y) - 1
	Bu <- dbinom(x = 0:n, size = n, prob = x)
	y <- sort(y)
	fn <- sum(y * Bu)
	return(fn)
}
