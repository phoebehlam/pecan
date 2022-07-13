#' serial histogram
#'
#' winsorize to next lowest/highest value based on user input sd from the mean
#' @param dat df name
#' @param var vector of variables
#' @export
histthem <- function (dat, var) {
  
  hist <- function (dat, x) {
    ggplot2::ggplot(dat=dat, ggplot2::aes_string(x=x)) + ggplot2::geom_histogram()
  }
  
  plots=purrr::map(var, ~hist(dat, .x))
  do.call(gridExtra::grid.arrange, plots)

}



