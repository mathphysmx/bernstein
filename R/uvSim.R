#' @title Bernstein copula simulation of a bivariate uniform distribution
#' @description Bernstein copula simulation of a bivariate uniform distribution. The bivariate uniform distribution can be a transformed one from non-uniform data.
#' @param n An integer specifying the number of simulations required.
#' @param diffEC Forward difference with respect of $x$ of the empirical copula matrix.
#' Maybe computed with \link[empiricalDistribution]{forwardDifference} of \link[empiricalDistribution]{empiricalCDF2Dcounts}. \link[empiricalDistribution]{empiricalCDF2Dcounts}.
#' @param u non-exceedance probabilities used for conditional simulation. Default to \code{NULL}, but if specified, then \code{n = length(u)}
#' @param delta numeric. Ideally must be zero, but it is used for numerical computation stability, say, \code{.Machine$double.eps^0.07}.
#' @param t Numeric value in \code{[0,1]} for repreducible research.
#' @param ... further arguments passed to \link[stats]{uniroot}
#' @return a 2-columns matrix of simulated pseudo-observations \code{u,v}.
#' @references Section 2.9 (Random Variate Generation) Nelsen, 2006
#' @examples
#' # Common to Examples 1, 2 and 3
#' en <- 100
#' library(copBasic)
#' set.seed(123); eu <- runif(en)
#' ev <- simCOPv(u = eu, cop=PLACKETTcop, para=20.1, snv=F) # simulate strong positive Plackett
#' plot.default(eu, ev, asp = 1, xlim = 0:1, ylim = 0:1, main = "data")
#' library(empiricalDistribution)
#' empCopulaCountsmatrix <- empiricalCDF2Dcounts(data.frame(U=eu, V=ev))
#' fDiffEmpCopMatrix <- forwardDifference(empCopulaCountsmatrix)
#' library(inverseFunction)
#'
#' # Example 1 (n is given):
#' evMySim1 <- uvSim(n = en, diffEC = fDiffEmpCopMatrix)
#' plot.default(evMySim1, asp = 1, xlim = 0:1, ylim = 0:1, main = "Ex1")
#'
#' # Example 2 (u is given):
#' evMySim2 <- uvSim(n = en, diffEC = fDiffEmpCopMatrix, u = eu)
#' plot.default(evMySim2, asp = 1, xlim = 0:1, ylim = 0:1, main = "Ex2")
#'
#' # Example 3 (Reproducible Research, u and t are given):
#' et <- evMySim2[, 3]
#' evMySim3 <- uvSim(n = 100, diffEC = fDiffEmpCopMatrix, u = eu, t = et)
#' all.equal(evMySim2, evMySim3)
#' plot.default(evMySim3, asp = 1, xlim = 0:1, ylim = 0:1, main = "Ex3")
#'
#' @export
# TODO:   Bug: for periodic data, simulation lasting infinite because derivative sometimes does not fall in or cover entirely [0,1] (a computational-numerical error?)
# TODO: Make this embarrasingly parallel by splitting the dataset simulated (u)
uvSim <- function(n = 100, diffEC, u = NULL, t = NULL,
                  delta = 0, ...){

  if(is.null(u)){
    uu <- runif(n, min = delta, max = 1 - delta)
  }else{
    n <- length(u)
    uu <- u
  }

  if(is.null(t)){
    tt <- runif(n)
  }else{
    tt <- t
  }

  vv <- vector(mode = "numeric", length = n)

  fun <- list(fun = 'bernstein2DderivativeX', u = uu[1],
              diffEC = diffEC)
  for (i in 1:n) {
    # i <- 2
    fun$u <- uu[i]
    vv[i] <- inverseFunction(dvar = tt[i], fun = fun)
    # vv[i] <-cv_du_inv3(u = uu[i], a = tt[i], diffEC = diffEC, ...)
  }

  return(cbind(u = uu, v = vv, t = tt))
}
