#' @title Manage/remove duplicated values in a table
#'
#' @description Manage/remove duplicated values in a table
#'
#' @param x A numeric table (data.frame or array).
#' @param col The column to be checked for duplicated values.
#' @param fun The function to process duplicated values.
#'
#' @return x with duplicated values processed.
#' @export
#'
#' @examples
#' ex <- data.frame(x=c(1,-2,3,4,-2,4), y = c(10,11,10,12,13,11))
#' ex1 = groupDuplicatedUtil(ex); print(ex1)
#' groupDuplicatedUtil(ex1, col = 2)
#' groupDuplicatedUtil(ex, col = 2)
groupDuplicatedUtil <-function(x, col = 1, fun = "median") {

  dup = duplicated(x[, col])
  dupValues = unique(x[dup, col])
  xunique = x[!dup, ]
  altPos = setdiff(x = 1:ncol(x), col)
  for(i in 1:length(dupValues)){
    # i = 19
    # print(i)
    dupPos = which(x[, col] == dupValues[i])
    # print(10 * i)
    dupUniPos = which(xunique[, col] == dupValues[i])
    xunique[dupUniPos, altPos] = get(fun)(x[dupPos, altPos])
    # print(1000 * i)
  }

  return(xunique)
}
