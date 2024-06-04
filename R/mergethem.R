#' mergethem
#'
#' merge multiple dfs together, identifier must be first column, doesn't need to be same name
#' @param ... dataframes separated by comma
#' @export
mergethem <- function (...){
  
  func <- function(...){
    df1 = list(...)[[1]]
    df2 = list(...)[[2]]
    col1 = colnames(df1)[1]
    col2 = colnames(df2)[1]
    xxx=merge(..., by.x = col1, by.y = col2, all = TRUE)
    return(xxx)
  }
  
  Reduce(func, list(...)) 
  
}