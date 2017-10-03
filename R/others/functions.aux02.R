#' @title fn.Bernshtein
#' @description fn.Bernshtein
#' @param x values
#' @param xy real data
#' @export
fn.Bernshtein <- function(x, xy){
  1/dFn.inv.Bernshtein(Fn.Bernshtein(x), xy[,1])
}

#' @title Gn.Bernshtein.aux
#' @description Gn.Bernshtein.aux
#' @param u non-exedance probabilities
#' @param xg given \code{x}
#' @param xy values
#' @export
Gn.Bernshtein.aux <- function(u, xy, xg)
  Fn.inv.Bernshtein(u, xy[,2]) - xg

#' @title Gn.Bernshtein function
#' @description Gn.Bernshtein function
#' @param x values
#' @param tol tolerance
# @inheritParams base::uniroot
#' @export
Gn.Bernshtein <- function(x, tol = .Machine$double.eps^0.25){
  uniroot(Gn.Bernshtein.aux, interval= c(0,1),
          xg = x, tol = tol)$root
}

#' @title gn.Bernshtein
#' @description gn.Bernshtein
#' @param x values
#' @param xy real data
#' @export
gn.Bernshtein <- function(x, xy)
  1/dFn.inv.Bernshtein(Gn.Bernshtein(x), xy[,2])

#' @title densidad.Bernshtein.emp
#' @description densidad.Bernshtein.emp
#' @param x,y values
#' @export
densidad.Bernshtein.emp <- function(x,y)
  dcopula.Bernshtein.emp(Fn.Bernshtein(x),
                         Gn.Bernshtein(y))*fn.Bernshtein(x)*gn.Bernshtein(y)

#' @title densidad.Bernshtein.emp.ydadox
#' @description densidad.Bernshtein.emp.ydadox
#' @param x,y values
#' @export
densidad.Bernshtein.emp.ydadox <- function(y,x)
  dcopula.Bernshtein.emp(Fn.Bernshtein(x),Gn.Bernshtein(y))*gn.Bernshtein(y)

#' @title regresion.copulaB
#' @description regresion.copulaB
#' @param u non-exceedance probability
#' @param q quantil
#' @param tol tolerance
# @inheritParams base::uniroot
#' @export
regresion.copulaB <- function(u, q,
                              tol = .Machine$double.eps^0.25){
  uniroot(cvDuAux,interval = c(0,1),
          ua_vec = c(u,q), tol=tol)$root
}

#' @title regression
#' @description regresion
#' @param x x value
#' @param q quantil
#' @param xy real data
#' @export
regression <- function(x,q, xy){
  Fn.inv.Bernshtein(regresion.copulaB(Fn.Bernshtein(x),q),
                    xy[,2])
}
