#' Transform dataframe with two catagorized columns and one value column to a list
#' 
#' Two catagorized columns will be names of the list and names of each elements in list, values will be values in each element
#' 
#' Things to be done:
#' 1. Deal with NA
#' 2. Collapse/Collapse on certain level
#' 3. Recursive list tree creat

listby2col <- function(x, formula = ~., fun){
  outer <- formula[[3]][[2]]
  inner <- formula[[3]][[3]]
  factor <- formula[[2]]
  temp_tbl <- tapply(eval(factor, envir = x), 
         list(eval(outer, envir = x), eval(inner, envir = x)),fun)
  outer <- unique(as.character(eval(outer, envir = x)))
  out_list <- list()
  for(i in seq_along(outer)){
    out_list[[outer[i]]] <- setNames(unlist(out[outer[i],], use.names = F), rep(names(out[outer[i],]), lengths(out[outer[i],])))
  }
  out_list
}




test_df <- data.frame(cola = rep(c('a','b','c'),c(2,3,5)),
                      colb = c('Aron','Adam','Bob','Blunt','Bob','Chris','Curry','Catherine','Curry','Cantebury'),
                      colc = c(1,2,3,4,5,6,7,NA,9,10),
                      cold = letters[1:10])


out <- tapply(test_df$colc, list(test_df$cola, test_df$colb),c)
setNames(unlist(out['c',], use.names = F), rep(names(out['c',]), lengths(out['c',])))




