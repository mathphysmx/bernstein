% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/groupDuplicated.R
\name{groupDuplicated}
\alias{groupDuplicated}
\title{Group duplicated data in order to be suitable for the empirical copula}
\usage{
groupDuplicated(x)
}
\arguments{
\item{x}{A 2-columns dataset (data.frame, cbind, or matrix).}
}
\value{
A 2-columns matrix of depured data ("without duplicated values").
}
\description{
Data for empirical distributions must have no duplicated values.
This function substitute the duplicated values by the median (default) of the duplicated rows.
the input dataset by getting the estatistical median (default) of duplicated values.
Notice that an iteration of this function using a second variable can produce again duplicated values in the 1st column.
This can continue in a cycle.
Another possibility to avoid duplicated datasest is to add an epsilon.
}
\examples{
# Example 1:
xe <- cbind(c(1, 1, 2, 2, 4), c(5, 6, 6, 6, 7))
groupDuplicated(xe)
# Example where this code fails:
xe <- as.matrix(airquality[, 1:2])
xe <- xe[complete.cases(xe), ]
xec <- groupDuplicated(x = xe)
any(duplicated(xec[, 1]))
# a possible solution could be to add a small error but the dependence structure is modified by this approach.
}
