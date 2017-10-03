#' @title density??
#' @description empirical copula density??
#' @param u,v numeric vector of values (pseudo-observaritions) in the unit square \eqn{[0,1]x[0,1]}. \eqn{u=F(x)}, \eqn{v=G(y)}
#' @param emp_cop Empirical copula matrix. Maybe computed with \link{EmpCop}.
#' @export
dcopula.Bernshtein.emp <- function(u,v, emp_cop){
  n <- dim(emp_cop)[1]-1 # Degree of Bernstein polynomial
  ((n)^2)*
    sum(emp_cop*((dbinom(-1:(n-1), n-1,u) - dbinom(0:(n), n-1,u)*c(-1, rep(1,n)))%*%t
                 (dbinom(-1:(n-1), n-1,v) - dbinom(0:(n), n-1,v)*c(-1, rep(1,n)))))
}
