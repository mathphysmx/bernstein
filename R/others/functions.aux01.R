#' @title Gets \eqn{t} from the simulation algorithm.
#' @description Gets \eqn{t} from the simulation algorithm.
#' @param a Is \eqn{t} in the simulation algorithm.
#' @param u A value in \eqn{[0,1]}.
#' @param emp_cop Empirical copula matrix. Maybe computed with \link{EmpCop}.
#' @param silent logical: should the report of error messages be suppressed? See \link[base]{try}
#' @param ... further arguments passed to \link[stats]{uniroot}
#' @examples
#' xye <- cbind(c(1, 1, 2, 3, 4), c(5, 6, 6, 6, 7))
#' uve <- cbind(u = ecdf(xye[,1])(xye[,1]), # Empirical CDF values
#'              v = ecdf(xye[,2])(xye[,2]))
#' Emp_cop_matrix <- EmpCop(uv = uve)
#' cv_du_inv(u = 0.5, a = 0.8,
#'           emp_cop = Emp_cop_matrix)
#' @references conditional distribution method, step 2, page 41, Nelsen, 2006.
#' @export
cv_du_inv <- function(u, a, emp_cop, silent = TRUE, ...){
  vf <- NULL
  del <- 0 # notice that some functions (as Bernstein polynomials) give error for del > 0
  while(!is.numeric(vf)){
    vf <- try(uniroot(f = cvDuAux, interval = c(0 - del, 1 + del),
                     ua_vec = c(u, a), emp_cop = emp_cop, ...)$root,
                  silent = silent)
  }

  return(vf)
}

#' @title Gets \eqn{t} from the simulation algorithm.
#' @description Gets \eqn{t} from the simulation algorithm.
#' @param a Is \eqn{t} in the simulation algorithm.
#' @param u A value in \eqn{[0,1]}.
#' @param emp_cop Empirical copula matrix. Maybe computed with \link{EmpCop}.
#' @examples
#' xye <- cbind(c(1, 1, 2, 3, 4), c(5, 6, 6, 6, 7))
#' uve <- cbind(u = ecdf(xye[,1])(xye[,1]), # Empirical CDF values
#'              v = ecdf(xye[,2])(xye[,2]))
#' Emp_cop_matrix <- EmpCop(uv = uve)
#' cv.du.inv(u = 0.5, a = 0.8, Emp_cop_matrix)
#' @references conditional distribution method, step 2, page 41, Nelsen, 2006.
#' @export
cv.du.inv <- function(u, a, emp_cop)
  uniroot(f = cvDuAux, interval = c(0,1),
          ua_vec = c(u, a), emp_cop = emp_cop)$root

#' @title Auxiliary function to get \eqn{t} from the simulation algorithm.
#' @description Gets \eqn{t} from the simulation algorithm.
#' @param v A value in \eqn{[0,1]}.
#' @param ua_vec u and \eqn{t}.
#' @param emp_cop Empirical copula matrix. Maybe computed with \link{EmpCop}.
#' @examples
#' xye <- cbind(c(1, 1, 2, 3, 4), c(5, 6, 6, 6, 7))
#' uve <- cbind(u = ecdf(xye[,1])(xye[,1]), # Empirical CDF values
#'              v = ecdf(xye[,2])(xye[,2]))
#' Emp_cop_matrix <- EmpCop(uv = uve)
#' cvDuAux(v = 0.7, ua_vec = c(0.5, 0.99), emp_cop = Emp_cop_matrix)
#' @details This function is equation 2.9.1 in Nelsen minus "t". Remember that "t" and eq. 2.9.1 are the same, so their difference must be zero, i. e., to find the root of this "zero" function
#' @export
cvDuAux <- function(v, ua_vec, emp_cop){
  cv.du(u = ua_vec[1], v = v, emp_cop) - ua_vec[2]
}
