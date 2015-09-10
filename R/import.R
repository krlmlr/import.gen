globalVariables(c("."))

#' Generate import::from calls
#'
#' This function generates calls to import::\link[import]{from} that imports
#' all symbols in a package.
#'
#' @param ... \code{[character(1)]}
#'
#'   Packages to generate import calls for.
#'
#' @param .pkgs \code{[character()} or \code{list()]}
#'
#'   Packages to generate import calls for, as list.
#'
#' @param .output \code{[character(1)]}
#'
#'   What should happen with the generated calls? If \code{"clipboard"} (default),
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
#' from("rpart", .output = "cat")
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter_
#' @export
from <- function(..., .pkgs = NULL, .output = c("clipboard", "cat", "return"),
                 .comment = TRUE) {
  .output <- match.arg(.output)
  .dots <- list(...)
  symbols <- find_symbols(c(unname(.dots), .pkgs))
  ret <- from_symbols(filter_(symbols, ~keep))
  if (.comment) {
    my_call <- get_call("from", .dots, .pkgs)

    ignore <- from_symbols(filter_(symbols, ~!keep))

    ret <- c(
      "# The imports below were generated using the following call:",
      paste0("# ", format(my_call)),
      if (length(ignore) > 0L) {
        paste("#", c("The following symbols are duplicates and therefore not imported:", ignore))
      },
      ret
    )
  }

  send_output(ret, .output)
}

#' @importFrom magrittr %>% extract2
from_symbols <- function(pkg) {
  pkg %>%
    group_by_(~name) %>%
    do(data_frame(format = {
      c(call("::", as.name("import"), as.name("from")), .$name[1L], .$symbol) %>%
        as.call %>%
        format
    })) %>%
    extract2("format")
}
