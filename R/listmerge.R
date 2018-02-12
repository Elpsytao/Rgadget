#' Transform dataframe with two catagorized columns and one value column to a list
#' 
#' Two catagorized columns will be names of the list and names of each elements in list, values will be values in each element
#' 
#' listmerge:
#' 1. Deal with collapse function
#' 2. Deal with names match
#' 3. Keep left/right/both
#' 


listmerge <- function(x, y, fun, use.names = F, keep){
  if(!is.list(x) || !is.list(y)){
    stop('Input must be lists.')
  }
  all_names <- unique(c(names(x), names(y)))
  out <- mapply(c, x[all_names], y[all_names])
  names(out) <- all_names
  out
}