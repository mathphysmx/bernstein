#' @title Partial derivative of the copula C with respect to v.
#' @description computes the partial derivative of the copula C with respect to v since it is required for the
#' conditional distribution method simulation algorithm in Nelsen (2006) and Erdely-Ruiz and Diaz-Viera (2009)
#' @param u numeric vector of values (pseudo-observaritions) in the unit square \eqn{[0,1]x[0,1]}. \eqn{u=F(x)}, \eqn{v=G(y)}
#' @param xy real data
#' @export
dFn.inv.Bernshtein <- function(u,xy){
  #
  # Derivada del polinomio de Bernshtein ajustado a una cuasi-inversa
  # de la funci’on de distribuci’on emp’irica
  #
  # Entrada: u = valor a evaluar, en [0,1]
  #          xy = valores observados (sin repetici’on)
  #
  # Salida: valor de (d/du)Fn^(-1)(u)
  #
  x <- sort(xy)
  n <- length(x)
  xm <- rep(0,n+1)
  for (j in 2:n){
    xm[j] <- (x[j-1] + x[j]) / 2
  }
  xm[1] <- x[1]
  xm[n+1] <- x[n]
  resultado <- x[n]*(u^(n-1)) - x[1]*((1-u)^(n-1))
  resultado <- resultado + sum(xm[2:n]*(dbinom(0:(n-2),n-1,u) -
                                          dbinom(1:(n-1),n-1,u)))
  resultado <- n*resultado
  return(resultado)
}
