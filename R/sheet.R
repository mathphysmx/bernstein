# http://stackoverflow.com/questions/26106086/2d-normal-pdf-with-ggplot-and-r
#' @title Computes a 2D function values over a given area.
#'
#' @description Computes a 2D function values over a given area.
#'
#' @param f A list with {name}, other named arguments to \code{f}. See example below.
#' @param lims a 2 x 2 array or data frame. the first row are the limits in the x axis, and the row 2 are the limits of the y axis.
#' @param n a single scalar or a 2-elements array with the number of values in each direction.
#' @param ... further arguments passed to the function \link[graphics]{image.default}.
#' @param plot logical. Should display a plot?
#'
#' @return An array of the values computed.
#' @seealso \code{sheet} is the 2D analog of \link[graphics]{curve}.
#' @export
#'
#' @examples
#' # Example 1
#' ss <- function(x, y, m = 5) {m * x + y}
#' ex0=1; exf=4; ey0=6; eyf= 7
#' elims <- rbind(x = c(ex0, exf), y = c(ey0, eyf)); print(elims)
#' ez <- sheet(f=list(name = ss, m=10),
#'       lims= elims,
#'       n = c(exf-ex0 + 1, eyf-ey0 + 1))
#' print(ez)
#'
#' # Example 2
#' ez <- sheet(f=list(name=ss), lims = elims)
sheet <- function(f, lims, n = 101, plot = TRUE, ...){

	if(length(n) == 1){
		n <- c(n, n)
	}

	x <- seq.int(from = lims[1,1], to = lims[1,2], length.out = n[1])
	y <- seq.int(from = lims[2,1], to = lims[2,2], length.out = n[2])

	z <- f
	if(is.list(f)){
	  z <- evalFunc2D(f = c(list(f = "outer", FUN = f[[1]]), f[-1]),
	                   x, y)
	}

	if(plot){
		image.default(x, y, z, ...)
	}

	invisible(z)
}
