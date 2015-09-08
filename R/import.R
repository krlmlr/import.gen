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
#' @importFrom dplyr filter
#' @export
from <- function(..., .pkgs = NULL, .output = c("clipboard", "cat", "return"),
                 .comment = TRUE) {
  .output <- match.arg(.output)
  .dots <- list(...)
  symbols <- find_symbols(c(unname(.dots), .pkgs))
  ret <- from_symbols(filter(symbols, keep))
  if (.comment) {
    my_call <- c(list("from"), .dots, if (!is.null(.pkgs)) list(.pkgs = .pkgs)) %>%
      do.call(call, .) %>%
      call("::", as.name("import.gen"), .)

    ignore <- paste("#", from_symbols(filter(symbols, !keep)))

    ret <- c(
      "# The imports below were generated using the following call:",
      paste0("# ", format(my_call)),
      if (length(ignore) > 0L) "# The following symbols could not be imported:",
      ignore,
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
#' @importFrom dplyr tbl_df group_by ungroup do mutate data_frame
#' @importFrom kimisc list_to_df
find_symbols <- function(pkgs) {
  exports <-
    pkgs %>%
    setNames(nm = .) %>%
    lapply(getNamespaceExports)

  exports_df <-
    exports %>%
    list_to_df %>%
    group_by(name) %>%
    do(data_frame(symbol = unlist(.$value))) %>%
    ungroup %>%
    mutate(keep = !duplicated(symbol, fromLast = TRUE))

  exports_df
}

#' @importFrom magrittr %>% extract2
from_symbols <- function(pkg) {
  pkg %>%
    group_by(name) %>%
    do(data_frame(format = {
      c(call("::", as.name("import"), as.name("from")), .$name[[1L]], .$symbol) %>%
        as.call %>%
        format
    })) %>%
    extract2("format")
}

message_after <- function(code, msg) {
  ret <- force(code)
  message(msg)
  ret
}
