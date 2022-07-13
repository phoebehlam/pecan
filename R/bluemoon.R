#bluemoon. one in a while functions.

#' gimmepac: install all previous packages after an r update
#'
#' fix the annoying thing where all your packages disappear after an r update
#'
#' @param path path to which the packages were saved for your previous version\cr
#' do .Library to find current and then back track shold be something like this\cr
#' /Library/Frameworks/R.framework/Versions/
#'
#' @examples
#' gimmepac ("/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
#'
#' @export
gimmepac <- function (path) {
  lib_loc<- path

  to_install <- unname(installed.packages(lib.loc = lib_loc)[, "Package"])
  to_install
  install.packages(pkgs = to_install)

}
