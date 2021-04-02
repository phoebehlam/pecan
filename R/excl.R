#' exclude
#'
#' exclude based on user input sd from the mean
#' @param x vector of data
#' @param sd the number of standard deviations from mean as the criterion for excluding
#' @export
excl <- function (x, sd) {
  z = scale(x)
  
  posout<-which(z >= sd)
  negout<-which(z <= -sd)
  
  xna=x
  xna[posout] <- "pos"
  xna[negout] <- "neg"
  
  xex1 <- replace (xna, xna == "pos", NA_real_)
  xex2 <- replace (xex1, xex1 == "neg", NA_real_)
  
  xex2<-as.numeric(as.character(xex2))
  
  return(xex2)
  
}


