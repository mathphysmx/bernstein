#' @title Fn.Bernshtein
#' @description Fn.Bernshtein
#' @param x values
#' @param xg given x
#' @param tol tolerance
#' @export
Fn.Bernshtein <- function(x, xg, tol) uniroot(Fn.Bernshtein.aux, interval = c(0,1),
                                     xg = x, tol = tol)$root
