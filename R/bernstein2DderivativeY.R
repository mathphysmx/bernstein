#' @title Partial derivative of the Bernstein-Bezier polynomial with respect to Y.
#' @description Computes the partial derivative of the Bernstein-Bezier polynomial with respect to x. For copula theory, it is required for the
#' conditional distribution method simulation algorithm in the book
#' 'Mai et al., 2012. Simulating copulas'.
#' @param u,v numeric vector of values (pseudo-observartions) in the unit square \eqn{[0,1]x[0,1]}. For copulas, \eqn{u=F(x)}, \eqn{v=G(y)}.
#' @param diffEC Forward difference integer matrix resulting from the empirical copula matrix.
#' Maybe computed with \link[empiricalDistribution]{forwardDifference} of \link[empiricalDistribution]{empiricalCDF2Dcounts}.
#' @return Numeric value, the partial derivative.
#' @export
#' @references Section 5.3 of book 'Mai et al., 2012. Simulating copulas' computed by the derivative in Eq. 5.23 of the Book of "Goldman, 2002, Pyrimid algorithms..." (the same as eq 7.15 of "phillips, 2003, Interpolation and Approximation...")
#' Notice that this partial derivative is a function of u, with v being treated as constant or an extra parameter.
#' @examples
#' library(empiricalDistribution)
#' exy <- cbind(1:5, c(2, 4, 3, 6, 7)); print(exy)
#' set.seed(1); exy <- exy[sample(1:5), ]
#' empCopulaCountsmatrix <- empiricalCDF2Dcounts(exy)
#' eu <- 0.5; ev <- 0.7
#' bernstein2DderivativeY(u = eu, v = ev,
#'                       diffEC = forwardDifference(empCopulaCountsmatrix, i =2))
#'
#' # Example 2:
#' library(copBasic)
#' n <- 100
#' exy <- PLACKETTsim(n, para=20.3) # simulate strong positive Plackett
#' plotProbs(exy)
#' empCopulaCountsmatrix <- empiricalCDF2Dcounts(exy)
#' eu <- 0.5; ev <- 0.7
#' bernstein2DderivativeY(u = eu, v = ev,
#'                       diffEC = forwardDifference(empCopulaCountsmatrix, i =2))
# previously named Cdv.R
#' @export
#'
bernstein2DderivativeY <- function(u, v, diffEC){

   n <- dim(diffEC)[2] # Degree of Bernstein polynomial
   Bu <- dbinom(x = 0:n,       size = n,     prob = u)
   Bv <- dbinom(x = 0:(n - 1), size = n - 1, prob = v)
   dc <- sum(diffEC * outer(Bu, Bv))

   return(dc)
 }
