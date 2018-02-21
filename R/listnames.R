#' Extract names for a list.
#'
#' @param x A input list
#' @param collapse A string to separate different levels in list name, default '_'
#' @note 
#' Need to deal with NULL names.
#' 
#' @examples
# test_list <- list(a = list(A1 = c(1,2,3), A2 = c(4,5,6)),
#                   b = c(7,8,9),
#                   c = list(C1 = c(10,11,12), C2 = list(C21 = c(13,14,15))))
# listnames(test_list)



listnames <- function(x, collapse = '_'){
  if(!is.list(x)){
    stop('Input must be a list.')
  }
  names_list <- names(x)
  for(i in seq_along(names_list)){
    if(is.list(x[[i]])){
      names_list <- c(names_list, paste0(names_list[i],collapse,listnames(x[[i]], collapse)))
    }
  }
  names_list
}
