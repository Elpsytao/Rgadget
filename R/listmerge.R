#' Transform dataframe with two catagorized columns and one value column to a list
#' 
#' Two catagorized columns will be names of the list and names of each elements in list, values will be values in each element
#' 
#' listby2col:
#' 1. Deal with NA: add parameter to control whether NA shown in output
#' 2. Change input type, using a pairlist instead of formula
#' 3. Recursive list tree creat
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