#' Generate import::from calls
#'
#' This function generates calls to import::\link[import]{from} that imports
#' all symbols in a package.
#'
#' @importFrom magrittr %>%
#' @export
from <- function(pkgs, output = c("clipboard", "cat", "character"), comment = TRUE) {
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
    clipboard = invisible(clipr::write_clip(c(ret, ""))),
    cat = cat(ret, sep = "\n"),
    character = ret
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
