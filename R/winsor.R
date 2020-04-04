#' winsorize
#'
#' winsorize to next lowest/highest value based on user input sd from the mean
#' @param x vector of data
#' @param sd the number of standard deviations from mean as the criterion for winsorizing
#' @export
winsor <- function (x, sd) {
  z = scale(x)
  
  posout<-which(z >= sd)
  negout<-which(z <= -sd)
  
  xna=x
  xna[posout] <- "pos"
  xna[negout] <- "neg"
  
  high <- max( as.numeric(xna), na.rm=T)
  low <- min( as.numeric(xna), na.rm=T)
  
  xwin1 <- replace (xna, xna == "pos", high)
  xwin2 <- replace (xwin1, xwin1 == "neg", low)
  
  return(xwin2)
  
}