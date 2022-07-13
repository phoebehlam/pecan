#' paste from clipboard
#' @export
cb <- function(...) {
  utils::read.table(pipe("pbpaste"),
                    sep="\t",
                    header=T, ...)
}

