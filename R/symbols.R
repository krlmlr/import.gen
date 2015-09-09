globalVariables(c("."))

#' @importFrom magrittr %>%
#' @importFrom dplyr tbl_df group_by ungroup do mutate data_frame arrange
#' @importFrom kimisc list_to_df
find_symbols <- function(pkgs) {
  exports <-
    pkgs %>%
    setNames(nm = .) %>%
    lapply(getNamespaceExports)

  exports_df <-
    exports %>%
    list_to_df %>%
    mutate(name = factor(name, levels = pkgs)) %>%
    group_by(name) %>%
    do(data_frame(symbol = sort(unlist(.$value)))) %>%
    ungroup %>%
    mutate(keep = !duplicated(symbol, fromLast = TRUE))

  exports_df
}

message_after <- function(code, msg) {
  ret <- force(code)
  message(msg)
  ret
}

send_output <- function(ret, .output) {
  switch(
    .output,
    clipboard = message_after(clipr::write_clip(c(ret, "")), "Import declaration copied to clipboard.") %>% invisible,
    cat = cat(ret, sep = "\n"),
    `return` = ret
  )
}

get_call <- function(func_name, .dots, .pkgs) {
  c(list(func_name), .dots, if (!is.null(.pkgs)) list(.pkgs = .pkgs)) %>%
    do.call(call, .) %>%
    call("::", as.name("import.gen"), .)
}
