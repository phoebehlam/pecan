#' merge multiple datasets together
#' 
#' @param ... list of dataframes. the matching column must be first variable
#' 
#' @example
#' Reduce(mergethem, list(a, b, c, d, e))
#' 
#' @export
mergethem <- function(..., id){
  df1 = list(...)[[1]]
  df2 = list(...)[[2]]
  col1 = colnames(df1)[1]
  col2 = colnames(df2)[1]
  xxx=merge(..., by.x = col1, by.y = col2, all = TRUE)
  return(xxx)
}

