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
    group_by(name) %>%
    do(data_frame(symbol = unlist(.$value))) %>%
    ungroup %>%
    mutate(keep = !duplicated(symbol, fromLast = TRUE)) %>%
    arrange(name, symbol)

  exports_df
}

message_after <- function(code, msg) {
  ret <- force(code)
  message(msg)
  ret
}
