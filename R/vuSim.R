#' @title Bernstein copula simulation of a bivariate uniform distribution
#'
#' @description Bernstein copula simulation of a bivariate uniform distribution. The bivariate uniform distribution can be a transformed one from non-uniform data.
#'
#' @param n An integer specifying the number of simulations required.
#' @param diffEC Forward difference with respect of $y$ of the empirical copula matrix.
#' Maybe computed with \link[empiricalDistribution]{forwardDifference} of \link[empiricalDistribution]{empiricalCDF2Dcounts}. \link[empiricalDistribution]{empiricalCDF2Dcounts}.
#' @param v Non-exceedance probabilities used for conditional simulation. Default to \code{NULL}, but if specified, then \code{n = length(v)}.
#' @param t Numeric value in \code{[0,1]} for repreducible research.
#' @param delta Numeric scalar. Ideally must be zero, but it is used for numerical computation stability, say, \code{.Machine$double.eps^0.07}.
#' @param ... Further arguments passed to \link[stats]{uniroot}.
#' @return A 2-columns matrix of simulated pseudo-observations \code{u,v}.
#' @references Section 5.3 of the book 'Simulating copulas' computed by the derivative in Eq. 5.23 of the Book of "Goldman, 2002, Pyrimid algorithms..." (the same as eq 7.15 of "phillips, 2003, Interpolation and Approximation...")
#' @importFrom "empiricalDistribution" "empiricalCDF2Dcounts","forwardDifference"
#' @importFrom "inverseFunction" "inverseFunction","evalFunc"
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#' @export
#' @examples
#' # Common to Examples 1, 2 and 3
#' en <- 100
#' library(copBasic)
#' set.seed(123); eu <- runif(en)
#' ev <- simCOPv(u = eu, cop=PLACKETTcop, para=20.1, snv=F) # simulate strong positive Plackett
#' plot.default(eu, ev, asp = 1, xlim = 0:1, ylim = 0:1, main = "data")
#' library(empiricalDistribution)
#' empCopulaCountsmatrix <- empiricalCDF2Dcounts(data.frame(U=eu, V=ev))
#' fDiffEmpCopMatrix <- forwardDifference(empCopulaCountsmatrix, i = 2)
#' library(inverseFunction)
#'
#' # Example 1 (n is given):
#' evMySim1 <- vuSim(n = en, diffEC = fDiffEmpCopMatrix)
#' plot.default(evMySim1, asp = 1, xlim = 0:1, ylim = 0:1, main = "Ex1")
#'
#' # Example 2 (u is given):
#' evMySim2 <- vuSim(n = en, diffEC = fDiffEmpCopMatrix, v = ev)
#' plot.default(evMySim2, asp = 1, xlim = 0:1, ylim = 0:1, main = "Ex2")
#'
#' # Example 3 (Reproducible Research, u and t are given):
#' et <- evMySim2[, 3]
#' evMySim3 <- vuSim(n = 100, diffEC = fDiffEmpCopMatrix, v = ev, t = et)
#' all.equal(evMySim2, evMySim3)
#' plot.default(evMySim3, asp = 1, xlim = 0:1, ylim = 0:1, main = "Ex3")
#'
# TODO:   Bug: for periodic data, simulation lasting infinite because derivative sometimes does not fall in or cover entirely [0,1] (a computational-numerical error?)
# TODO: Make this embarrasingly parallel by splitting the dataset simulated (u)
vuSim <- function(n = 100, diffEC, v = NULL, t = NULL,
                  delta = 0, ...){

  if(is.null(v)){
    vv <- runif(n, min = delta, max = 1 - delta)
  }else{
    n <- length(v)
    vv <- v
  }

  if(is.null(t)){
    tt <- runif(n)
  }else{
    tt <- t
  }

  uu <- vector(mode = "numeric", length = n)

  fun <- list(fun = 'bernstein2DderivativeY', v = vv[1],
              diffEC = diffEC)
  for (i in 1:n) {
    # i <- 2
    fun$v <- vv[i]
    uu[i] <- inverseFunction::inverseFunction(dvar = tt[i], fun = fun)
    # vv[i] <-cv_du_inv3(u = uu[i], a = tt[i], diffEC = diffEC, ...)
  }

  return(cbind(u = uu, v = vv, t = tt))
}
