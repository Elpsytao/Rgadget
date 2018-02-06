#' Transform dataframe with two catagorized columns and one value column to a list
#' 
#' Two catagorized columns will be names of the list and names of each elements in list, values will be values in each element
#' 
#' Things to be done:
#' 1. Deal with factor type input
#' 2. Collapse
#'    




group_list_one <- function(x, formula, ...){
  if(!is.data.frame(x)){
    stop('Input data type error: must be a data.frame')
  }
  out <- xtabs(x, formula = formula, ...)
  names(out) <- colnames(out)
  out
}

group_list_two <- function(x, by, formula){
  uni_val <- unique(x[by])[[1]]
  out_list <- list()
  for(i in seq_along(uni_val)){
    in_data <- x[x[by] == uni_val[i],]
    out_list[[i]] <- group_list_one(in_data, formula = formula)
  }
  names(out_list) <- uni_val
  out_list
}

test_df <- data.frame(cola = rep(c('a','b','c'),c(2,3,5)),
                      colb = c('Aron','Adam','Bob','Blunt','Bob','Chris','Curry','Catherine','Curry','Cantebury'),
                      colc = c(1,2,3,4,5,6,7,NA,9,10),
                      cold = letters[1:10])







