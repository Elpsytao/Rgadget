#' Conditional lapply, apply function to run on objects based on criterion.
#' 
#' 
#' condlapply:
#' 
#' 
#' 


condlapply <- function (X, FUN, COLFUN, simplify = TRUE, USE.NAMES = TRUE) 
{
  COLFUN <- match.fun(COLFUN)
  index <- which(sapply(X,COLFUN))
  FUN <- match.fun(FUN)
  PART <- X[,index]
  X <- as.list(X)
  X[index] <- lapply(PART, FUN)
  return(X)
}


