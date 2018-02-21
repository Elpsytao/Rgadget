#' Conditional lapply to apply only on columns with certain condition
#'
#' @param X A input list
#' @param fun A function to apply
#' @param colfun A function to preapply on columns to specify which column to calculate
#' 
#' @note 
#' Change COLFUN input way more like make_function
#' 
#' @examples
#' conlapply(mtcars, sum, function(x) sum(x > 20) > 5)



condlapply <- function (X, FUN, COLFUN) 
{
  COLFUN <- match.fun(COLFUN)
  index <- which(sapply(X,COLFUN))
  FUN <- match.fun(FUN)
  PART <- X[,index]
  X <- as.list(X)
  X[index] <- lapply(PART, FUN)
  return(X)
}


