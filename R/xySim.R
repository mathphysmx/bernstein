#' @title Generate simulations from bivariate dataset
#' @description Generate simulations from bivariate datasets.
#' @param xy Numeric 2-columns table (data.frame, matrix, cbind) of real dataset. The first column is the independent variable while the second is the dependent variable.
#' @param xcond A numeric vector being the 1st column of xy (xy[,1]). Must be conditional to the independent variable (\code{x})? Default to \code{NULL}.
#' @inheritParams uvSim
#' @return A list with
#'  xy_sim: 2-columns matrix of simulated observations \code{x,y}.
#'  uv_sim: 2-columns matrix of simulated pseudo observations \code{u,v}.
#'
#' @export
#' @examples
#' # Example 1
#' data("PHIV_K"); plot(PHIV_K)
#' eSim <- xySim(n = 200, xy = PHIV_K)
#' plot(eSim$xy_sim)
xySim <- function(n = 100, xy,
                  diffEC = forwardDifference(empiricalCDF2Dcounts(xy)),
                  xcond = NULL){
  #
  # Salida: matriz de n x 2 con las simulaciones
  #         matriz de n x 2 con las simulaciones de la c’opula
  #
  # Dependencias: utiliza los programas <simula.copula.Bernshtein>
  #
  #               <Fn.inv.Bernshtein>
  #

  if(!is.null(xcond)){
    u <- ecdf(xcond)(xcond)
  }else{
    u <- NULL
  }

  uv_sim <- uvSim(n = n, diffEC = diffEC, u = u)

  # get the quasi-inverse by Bernstein-Kantorivich
  if(!is.null(xcond)){
    x <- xcond
  }else{
    x <- sapply(uv_sim[, 1], Fn.inv.Bernshtein, valores.emp = xy[, 1])
  }
  y <- sapply(uv_sim[, 2], Fn.inv.Bernshtein, valores.emp = xy[, 2])

  return(list(xy_sim = cbind(x,y), uv_sim = uv_sim))
}
