#' pomp using actual min and max
#'
#' pomp score adjusted to 0 to 10
#' @param x vector of data
#' @export
pomp <- function (x) {
  min = min(x, na.rm=T)
  max = max(x, na.rm=T)
  pomp = 10*(x - min)/(max-min)
  return(pomp)
}
