globalVariables(c("."))

#' Generate import::from calls
#'
#' This function generates calls to import::\link[import]{from} that imports
#' all symbols in a package.
#'
#' @param pkgs \code{[character()} or \code{list()]}\cr
#'   Packages to generate import calls for.
#' @param output \code{[character(1)]}\cr
#'   What should happen with the generated calls? If \code{"clipboard"} (default),
#'   copy to clipboard. If \code{"cat"}, print to the console. If \code{"return"},
#'   simply return as a character vector.
#' @param comment \code{[logical(1)]}\cr
#'   Should comments be prepended to the generated code? Default: \code{TRUE}.
#'
#' @return \code{[character()]}\cr
#'   The generated output, invisibly unless \code{output} is set to \code{"return"}.
#'
#' @examples
#' from("rpart", output = "cat")
#'
#' @importFrom magrittr %>%
#' @export
from <- function(pkgs, output = c("clipboard", "cat", "return"), comment = TRUE) {
  output <- match.arg(output)
  ret <- lapply(pkgs, from_one) %>%
    unlist
  if (comment) {
    ret <- c(
      "# The imports below were generated using the following call:",
      paste0("# import.gen::", list("from", pkgs = pkgs) %>% do.call(call, .) %>% format),
      ret
    )
  }
  switch(
    output,
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
