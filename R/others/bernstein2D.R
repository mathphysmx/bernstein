#' @title bezier sheet.
#' @description 
#' @param u x axis coordinate where to evaluate the bezier sheet.
#' @param v y axis coordinate where to evaluate the bezier sheet.
#' @param xi matrix of integer values.
#' 
#' @return the value of the bezier sheet.
#' @export 
#' 
#' @examples
 ss <- function(x, y, m = 5) {m * x + y}
 ex0=1; exf=4; ey0=6; eyf= 7
 elims <- rbind(x = c(ex0, exf), y = c(ey0, eyf)); print(elims)
 ez <- sheet(f=list(name=ss), lims = elims)
 bezier2D(data.frame(x=8, y=10), xi=ez)


z <- function(x) x[,1]^2 + x[,2]^2
mesh <- seq(0,1, by = 0.1)
zi <- expand.grid(mesh, mesh)
z(zi)
sheet(f=list(z),lims=rbind(x=c(0,10), y=c(0,10)))
source("/media/paco/myhdd/soft/dev/R/Rfunctions/evalFunc.R")
source("/media/paco/myhdd/soft/dev/R/Rfunctions/evalFunc2D.R")
source("/media/paco/myhdd/soft/dev/R/Rfunctions/sheet.R")
source("/media/paco/myhdd/Doctorado/soft/bernsteincopula/R/empiricalDistribution2Dcounts.R")

#' # Example
 x <- cbind(1:5, c(2, 4, 3, 6, 7))
#' x <- x[sample(1:5), ]
#' xi <- empiricalDistribution2Dcounts(x)
bezier2D(u=c(0.3, 0.6), xi=xi)
ez <- sheet(f=list(name=bezier2D), lims = rbind(x=c(0.5,1), y=c(0.5,1)))

bezier2D <- function(u, xi){

	n <- dim(xi)[1] - 1
	Bu <- dbinom(x = 0:n, size = n, prob = u[1])
	Bv <- dbinom(x = 0:n, size = n, prob = u[2])
	z <- sum(xi * outer(Bu,Bv))

	return(z/n)
}
