#sunrise. at every start-up.
.onAttach <- function(libname,pkgname) {
  packageStartupMessage('\np  e  c  a  n\n')
}

#set the kitchen as directory
#'set the kitchen as directory
#'
#'@examples
#'kitch()
#'
#' @export
kitch <- function(){
  setwd("/Volumes/GoogleDrive/My Drive/the kitchen")
}
