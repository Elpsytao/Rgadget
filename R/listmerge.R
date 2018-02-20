#' Merge two single-layer, named-value list
#'
#' @param x A list to join
#' @param y A list to join
#' @param fun A function to apply on same name values after join
#' @note 
#' Modifiedw with listnames metehod
#' @examples
#' 
#' 


listmerge <- function(x, y, fun, use.names = F, keep = both){
  if(!is.list(x) || !is.list(y)){
    stop('Input must be lists.')
  }
  both <- unique(c(names(x), names(y)))
  names(both) <- both
  out <- mapply(c, x[both], y[both])
  names(out) <- both
  out  <- out[names(eval(keep))]
  out
}

