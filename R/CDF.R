#' @title CDF plots the empirical and theoretical cumulative distribution function of a vector of data
#' @description computes and plots the empirical cumulative distribution function (ecdf). Also plots models over the ecdf
#' @param x a vector of values for which the histogram is desired.
#' @param distr a character string of the distribution name. See the examples below and ?distributions. Default to NULL to plot just the empirical DCF.
#' @param para A named list giving the parameters of the named distribution. Default to NULL to plot just the empirical DCF
#' @param xlim the x limits (x1, x2) of the plot. x1 > x2.
#' @param main a main title for the plot, see also title. Default to "CDF".
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to "CDF".
#' @param col a character string of the points color. See ?par
#' @param pch character style. Default to 20. see \link[graphics]{points.default}
#' @param cex Magnification of points. Defatult to 1.8
#' @param lcol a character string of the line color of the model curve.  See "col" in ?par
#' @param lty a character string of the line type  of the model curve. See ?par
#' @param lwd a character string of the line width of the model curve. See ?par
#' @param ... further arguments passed to plot
#' @return a list with:
#'   CDF. A data.frame with the sorted input values in the first column and the CDF values in the second column
#'   xlim. A vector with the limits of the x axis
#' @export
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#' @importFrom "stats" "ecdf","runif","uniroot"
#' @importFrom "graphics" "image.default","lines.default","par","plot"
#' @details This function is based on the function plotdist from the package fitdistrplus
#' @examples
#' # EXAMPLE 1: NORMAL DISTRIBUTION (EMPIRICAL CDF)
#' set.seed(123)
#' MEAN <- 2
#' SD <- 0.5
#' X <- rnorm(n = 300, mean = MEAN, sd = SD)
#' Results <- CDF(x = X, main = "Empirical CDF") # Empirical CDF
#'
#' # EXAMPLE 2: NORMAL DISTRIBUTION (MODEL OVER THE EMPIRICAL CDF)
#' PARA <- list(mean = MEAN, sd = SD)
#' Results <- CDF(x = X, distr = "norm", para = PARA)
#' CDF(x = X, distr = "norm", para = PARA, col = "orange")
#' CDF(x = X, distr = "norm", para = PARA, col = "orange", main = "Test Main")
#' CDF(x = X, distr = "norm", para = PARA, col = "orange", main = "Test Main", pch = "i")
#' CDF(x = X, distr = "norm", para = PARA, col = "orange", main = "Test Main", pch = "i",
#'     lcol = "blue", lwd = 3)
#'
#' # EXAMPLE 3: EXPONENTIAL DISTRIBUTION
#' set.seed(123)
#' RATE <- 0.5
#' X <- rexp(300, RATE)
#' PARA <- list(rate = RATE)
#' Results <- CDF(x = X, distr = "exp", para = PARA)
###### IMPROVEMENTS
# accept the coordinates of the model line
CDF <- function (x, distr = NULL, para = NULL, xlim = NULL,
                 main = "CDF", xlab = NULL, ylab = NULL, col = "lightgray", pch = 20, cex = 1.8,
                 lcol = par("col"), lty = par("lty"), lwd = par("lwd"), ...)
{

  if (is.null(xlim))
    xlim <- c(min(x),max(x))

  xlab <- ifelse(!is.null(xlab),
                 xlab,
                 deparse(substitute(x)))

  ylab <- ifelse(!is.null(ylab),
                 ylab,
                 "CDF")

  s <- x
  ecdfResults <- ecdf(s)
  obsp <- ecdfResults(s) # "obsp" are the CDF values of "data"

  model <- !(is.null(distr) | is.null(para))
  if(model){
    if (!is.character(distr)) {
      distname <- substring(as.character(match.call()$distr), 2) # gets the distribution name from the argument "distr" (by deleting the first letter and converting to character) in case is given as "distr=dnorm", for example
    } else distname <- distr
    if (!is.list(para))
      stop("'para' must be a named list")
    pdistname <- paste("p", distname, sep = "") # builds up the "p" probability distribution function name as a character string
    if (!exists(pdistname, mode = "function"))
      stop(paste("The ", pdistname, " function must be defined"))
  }

  # plots the CDF as points with coordinates (s,obsp). Both "s" and "obsp" are given above
  plot(ecdfResults, xlim = xlim,
               main = main, xlab = xlab, ylab = ylab,
               col = col, pch = pch, cex = cex,...)
  if(model){
    sfin <- seq(xlim[1], xlim[2], by = (xlim[2]-xlim[1])/100)
    theopfin <- do.call(pdistname, c(list(q = sfin), as.list(para)))
    lines.default(sfin, theopfin, col = lcol, lty = lty, lwd = lwd) # plot of the theoretical CDF function over the empirical CDF
  }

  r <- list(CDF = data.frame(s,obsp), xlim = xlim)
  invisible(r)

}
