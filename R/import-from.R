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

  symbols <-
    find_symbols(c(unname(.dots), .pkgs)) %>%
    mutate(has_spaces = grepl(" ", symbol))

  ret <- importFrom_symbols(filter(symbols, keep & !has_spaces))

  if (.comment) {
    my_call <- get_call("importFrom", .dots, .pkgs)

    ignore <- importFrom_symbols(filter(symbols, !keep), directive = "# @importFrom %s")

    with_spaces <- importFrom_symbols(filter(symbols, keep & has_spaces), directive = "# %s:")

    ret <- c(
      "# The imports below were generated using the following call:",
      paste0("# ", format(my_call)),
      if (length(ignore) > 0L) {
        c("# The following symbols are duplicates and therefore not imported:", ignore)
      },
      if (length(with_spaces) > 0) {
        c(
          "# The following symbols contain spaces and cannot be used in @importFrom:",
          with_spaces
        )
      },
      ret
    )
  }

  send_output(ret, .output)
}

#' @importFrom magrittr %>% extract2
importFrom_symbols <- function(pkg, directive = "#' @importFrom %s") {
  pkg %>%
    group_by(name) %>%
    do(data_frame(format = {
      named_directive <- paste0(sprintf(directive, .$name[1L]), " ")
      paste0(named_directive, strwrap(paste(.$symbol, collapse = " "), 80L - nchar(named_directive)))
    })) %>%
    extract2("format")
}
