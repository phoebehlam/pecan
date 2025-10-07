#' view colnames
#'
#' @param x dataframe
#' @export
rowsum <- function(x) {
  rs <- rowSums(x, na.rm = TRUE)
  rs[rowSums(!is.na(x)) == 0] <- NA
  rs
}

