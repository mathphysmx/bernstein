#' @title Fn.Bernshtein
#' @description Fn.Bernshtein
#' @param u non-excedance probabilities
#' @param xg given x
#' @param xy real data
#' @export
Fn.Bernshtein.aux <- function(u, xg, xy) Fn.inv.Bernshtein(u, xy[,1]) - xg
