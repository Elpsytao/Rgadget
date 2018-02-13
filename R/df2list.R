#' Transform dataframe with two catagorized columns and one value column to a list
#' 
#' Two catagorized columns will be names of the list and names of each elements in list, values will be values in each element
#' 
#' listby2col:
#' 1. Deal with NA: add parameter to control whether NA shown in output
#' 2. Change input type, using a pairlist instead of formula
#' 3. Recursive list tree creat
#' 

listby2col <- function(x, formula = ~., fun){
  outer <- formula[[3]][[2]]
  inner <- formula[[3]][[3]]
  factor <- formula[[2]]
  funcs <- match.fun(fun)
  temp_tbl <- tapply(eval(factor, envir = x), 
         list(eval(outer, envir = x), eval(inner, envir = x)),funcs)
  outer <- unique(as.character(eval(outer, envir = x)))
  out_list <- list()
  for(i in seq_along(outer)){
    sublists <-  setNames(unlist(temp_tbl[outer[i],], use.names = F), 
                          rep(names(temp_tbl[outer[i],]), lengths(temp_tbl[outer[i],])))
    out_list[[outer[i]]] <- sublists[!is.na(sublists)]
  }
  out_list
}









