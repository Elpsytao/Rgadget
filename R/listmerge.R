#' Merge two single-layer, named-value list
#'
#' @param x A list to join
#' @param y A list to join
#' @param fun A function to apply on same name values after join
#' @note 
#' Deal with pune and reduce problems
#' @examples
# l1 <- list(a = 1,
#            b = list(x = 3,
#                     y = list(j = 5,
#                              k = c(7,8))))
# l2 <- list(a = 3,
#            d = 7,
#            b = list(x = list(list(m = 6,
#                                   n = 7)),
#                     y = list(j = 10:11,
#                              q = 17)))

listmerge <- function(.x, .y){
  x_n <- listnames(.x)
  y_n <- listnames(.y)
  out <- .x
  for(names in unique(c(x_n, y_n))){
    conponent <- unlist(strsplit(names, '_'))
    if(!is.list(.x[[conponent]]) & !is.list(.y[[conponent]])){
      out[[conponent]] <- c(.x[[conponent]], .y[[conponent]])
    }
  }
  out
}
