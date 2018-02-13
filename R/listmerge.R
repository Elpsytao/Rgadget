#' Transform dataframe with two catagorized columns and one value column to a list
#' 
#' Two catagorized columns will be names of the list and names of each elements in list, values will be values in each element
#' 
#' listmerge:
#' 1. Merged list with multiple same name will cause loss of some data
#' 2. Deal with names match - condition apply
#' 3. cpp


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


