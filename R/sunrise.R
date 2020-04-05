#sunrise. at every start-up.
.onAttach <- function(libname,pkgname) {
  packageStartupMessage('\np  e  c  a  n\nsomeone once roasted pecans \ntho oblivious before\nthey became essential\njust like someone\njust like these functions\n')
}

#set the kitchen as directory
#'set the kitchen as directory
#'
#'@examples
#'kitch()
#'
#' @export
kitch <- function(){
  setwd("/Users/phoebelam/Google Drive/chialam/stats resources/the kitchen")
}
