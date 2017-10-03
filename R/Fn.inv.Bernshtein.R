#' @title Bersntein polynomial fitting to a quasi-inverse.
#' @description Bersntein polynomial fitting to a quasi-inverse.
#' @param u A point where the fitted Bernstein polynomial is to be evaluated at. \eqn{u \in [0,1]}
#' @param valores.emp Observed values.
#' @references Munoz-Perez and Fernandez-Palacin, 1987. Bernstein-Kantorovich polynomial
#' @author Arturo Erdely (\email{arturo.erdely@comunidad.unam.mx})
#' @export
#' @details See also equation 4 in Hernandez-Maldonado, Diaz-Viera and Erdely, 2012,
#' A joint stochastic simulation method using the Bernstein copula as a flexible...
#' This is the same function as lmomco:::dat2bernqua
Fn.inv.Bernshtein <- function(u, valores.emp){
  #
  # Ajusta polinomio de Bernshtein-Kantorovich a una cuasi-inversa
  # de la funci’on de distribuci’on emp’irica.
  #
  # Entrada: u = valor a evaluar, en [0,1]
  #          valores.emp = valores observados (sin repetici’on)
  #
  # Salida: valor de Fn^(-1)(u)
  #
  x <- sort(valores.emp)
  n <- length(x)
  xm <- rep(0,n+1)
  for (j in 2:n){
    xm[j] <- (x[j-1] + x[j]) / 2
  }
  xm[1] <- x[1]
  xm[n+1] <- x[n]
  return(sum(xm*dbinom(0:n,n,u)))
}
