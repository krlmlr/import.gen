globalVariables(c("."))

#' Generate @@importFrom directives
#'
#' This function generates \code{@@importFrom}
#' roxygen directives that import all symbols in a package.
#'
#' @param ... \code{[character(1)]}
#'
#'   Packages to generate \code{@@importFrom}
#'   directives for.
#'
#' @param .pkgs \code{[character()} or \code{list()]}
#'
#'   Packages to generate \code{@@importFrom}
#'   directives for, as list.
#'
#' @param .output \code{[character(1)]}
#'
#'   What should happen with the generated directives? If \code{"clipboard"} (default),
#'   copy to clipboard. If \code{"cat"}, print to the console. If \code{"return"},
#'   simply return as a character vector.
#'
#' @param .comment \code{[logical(1)]}
#'
#'   Should comments be prepended to the generated code? Default: \code{TRUE}.
#'
#' @return \code{[character()]}
#'
#'   The generated output, invisibly unless \code{output} is set to \code{"return"}.
#'
#' @examples
#' importFrom("rpart", .output = "cat")
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @export
importFrom <- function(..., .pkgs = NULL, .output = c("clipboard", "cat", "return"),
                 .comment = TRUE) {
  .output <- match.arg(.output)
  .dots <- list(...)
  symbols <- find_symbols(c(unname(.dots), .pkgs))
  ret <- importFrom_symbols(filter(symbols, keep))
  if (.comment) {
    my_call <- get_call("importFrom", .dots, .pkgs)

    ignore <- importFrom_symbols(filter(symbols, !keep), at = FALSE)

    ret <- c(
      "# The imports below were generated using the following call:",
      paste0("# ", format(my_call)),
      if (length(ignore) > 0L) {
        paste("#", c("The following symbols could not be imported:", ignore))
      },
      ret
    )
  }

  send_output(ret, .output)
}

#' @importFrom magrittr %>% extract2
importFrom_symbols <- function(pkg, at = TRUE) {
  pkg %>%
    group_by(name) %>%
    do(data_frame(format = {
      directive <- paste0("#", if (at) "'", " @importFrom ", .$name[1L], " ")
      paste0(directive, strwrap(paste(.$symbol, collapse = " "), 80L - nchar(directive)))
    })) %>%
    extract2("format")
}
