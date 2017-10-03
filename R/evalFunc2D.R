#' @title evaluates a bivariate function in a list with the function name and named parameters.
#'
#' @description evaluates a bivariate function in a list with the function name and named parameters.
#'
#' @inheritParams inverseFunction::evalFunc
#' @param y Vector where \code{f} will be evaluated. If \code{f} is not a list, \code{x, y} is not used.
#' @param f A list with the function name and named parameters. See examples.
#' @param x Independent variable.
#' @param p Non-negative integer. The index where the name (\code{f}) is.
#' @details x and y go to the first and second input parameter of the function \code{f}.
#'
#' @return If is a list, the evaluation of \code{f} at points \code{x,y}. If \code{f} is not a list, the result is the input \code{f}.
#' @export
#'
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#' @examples
#' ss <- function(x, y, m = 5) {m * x + y}
#' evalFunc2D(f = list(f = "outer", FUN = "ss", m = 10), x = 1:4, y = 6:7)
evalFunc2D <- function(f, x, y, p = 1){

  fval <- f
  if(is.list(f)){
    fpar <- f; fpar[p] <- NULL
    fval <- do.call(what = f[[p]],
                    args = c(list(x, y), fpar))
    rownames(fval) = paste0("x", 1:(length(x)))
    colnames(fval) = paste0("y", 1:(length(y)))
  }

  return(fval)
}
