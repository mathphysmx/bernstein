#' @title Vugular Porosity (PHIV), Permeability (K) in a fractured porous media
#'
#' @description The same as \link{PHIV_K_raw} but after \code{groupDuplicated(PHIV_K_raw)}, i. e., \code{PHIV_K <- groupDuplicated(PHIV_K_raw)}
#'
#' @format A matrix with 380 rows and 2 variables:
#' \describe{
#'   \item{1st column}{Porosity in \code{[0, 1]}}
#'   \item{2nd column}{Permeability in mili Darcys (mD)}
#'   ...
#' }
#' @source Erdely-Ruiz, A., Diaz-Viera, M. A., 2012. Joint Porosity-permeability
#' Stochastic Simulation and Spatial Median Regression by Nonparametric
#' Copula Modeling. In: GI Forum 2012: Geovizualisation, Society
#' and Learning. Herbert Wichmann Verlag, VDE VERLAG GMBH,
#' Berlin/Offenbach, Wichman, Berlin, pp. 346-354.
"PHIV_K"
