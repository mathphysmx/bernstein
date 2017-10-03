#' @title Partial derivative of the Bernstein-Bezier polynomial with respect to X.
#' @description Computes the partial derivative of the Bernstein-Bezier polynomial with respect to x. For copula theory, it is required for the
#' conditional distribution method simulation algorithm in the book 'Nelsen, 2006. An introduction to copulas'.
#' @param u,v numeric vector of values (pseudo-observartions) in the unit square \eqn{[0,1]x[0,1]}. For copulas, \eqn{u=F(x)}, \eqn{v=G(y)}.
#' @param diffEC Forward difference integer matrix resulting from the empirical copula matrix.
#' Maybe computed with \link[empiricalDistribution]{forwardDifference} of \link[empiricalDistribution]{empiricalCDF2Dcounts}.
#' @return Numeric value, the partial derivative.
#' @export
#' @details Equation 2.9.1 of Nelsen, 2006.
#' Notice that this partial derivative is a function of v, with u being treated as constant or an extra parameter.
#' @examples
#' library(empiricalDistribution)
#' exy <- cbind(1:5, c(2, 4, 3, 6, 7)); print(exy)
#' set.seed(1); exy <- exy[sample(1:5), ]
#' empCopulaCountsmatrix <- empiricalCDF2Dcounts(exy)
#' eu <- 0.5; ev <- 0.7
#' bernstein2DderivativeX(v = ev, u = eu,
#'                       diffEC = forwardDifference(empCopulaCountsmatrix))
#'
#' # Example 2:
#' library(copBasic)
#' n <- 100
#' exy <- PLACKETTsim(n, para=20.3) # simulate strong positive Plackett
#' plotProbs(exy)
#' empCopulaCountsmatrix <- empiricalCDF2Dcounts(exy)
#' eu <- 0.5; ev <- 0.7
#' bernstein2DderivativeX(v = ev, u = eu,
#'                       diffEC = forwardDifference(empCopulaCountsmatrix))
# previously named cv.du
#' @export
#'
bernstein2DderivativeX <- function(v, u, diffEC){

   n <- dim(diffEC)[1] # Degree of Bernstein polynomial
   Bu <- dbinom(x = 0:(n - 1), size = n - 1, prob = u)
   Bv <- dbinom(x = 0:n,       size = n,     prob = v)
   dc <- sum(diffEC * outer(Bu, Bv))

   return(dc)
 }
