#' @title Group duplicated data in order to be suitable for the empirical copula
#' @description Data for empirical distributions must have no duplicated values.
#' This function substitute the duplicated values by the median (default) of the duplicated rows.
#' the input dataset by getting the estatistical median (default) of duplicated values.
#' Notice that an iteration of this function using a second variable can produce again duplicated values in the 1st column.
#' This can continue in a cycle.
#' Another possibility to avoid duplicated datasest is to add an epsilon.
#' @param x A 2-columns dataset (data.frame, cbind, or matrix).
#' @return A 2-columns matrix of depured data ("without duplicated values").
#' @export
#' @examples
#' # Example 1:
#' xe <- cbind(c(1, 1, 2, 2, 4), c(5, 6, 6, 6, 7))
#' groupDuplicated(xe)
#' # Example where this code fails:
#' xe <- as.matrix(airquality[, 1:2])
#' xe <- xe[complete.cases(xe), ]
#' xec <- groupDuplicated(x = xe)
#' any(duplicated(xec[, 1]))
groupDuplicated <- function(x){

  if(any(duplicated(x[,1])) || any(duplicated(x[,2])) || any(duplicated(x[,2]))) print('There were duplicated values')

  x1 = groupDuplicatedUtil(x)
  x2 = groupDuplicatedUtil(x1, col = 2)

  return(x2[order(x2[, 1]),])
}
