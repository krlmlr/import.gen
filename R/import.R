globalVariables(c("."))

#' Generate import::from calls
#'
#' This function generates calls to import::\link[import]{from} that imports
#' all symbols in a package.
#'
#' @param ... \code{[character(1)}]}
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
#' @export
from <- function(..., .pkgs = NULL, .output = c("clipboard", "cat", "return"),
                 .comment = TRUE) {
  .output <- match.arg(.output)
  .dots <- list(...)
  ret <- lapply(c(unname(.dots), .pkgs), from_one) %>%
    unlist
  if (.comment) {
    my_call <- c(list("from"), .dots, if (!is.null(.pkgs)) list(.pkgs = .pkgs)) %>%
      do.call(call, .) %>%
      call("::", as.name("import.gen"), .)

    ret <- c(
      "# The imports below were generated using the following call:",
      paste0("# ", format(my_call)),
      ret
    )
  }
  switch(
    .output,
    clipboard = message_after(clipr::write_clip(c(ret, "")), "Import declaration copied to clipboard.") %>% invisible,
    cat = cat(ret, sep = "\n"),
    `return` = ret
  )
}

#' @importFrom magrittr %>%
from_one <- function(pkg) {
  pkg %>%
    {c(., getNamespaceExports(.) %>% sort)} %>%
    as.list %>%
    c(call("::", as.name("import"), as.name("from")), .) %>%
    as.call %>%
    format
}

message_after <- function(code, msg) {
  ret <- force(code)
  message(msg)
  ret
}
