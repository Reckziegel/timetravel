#' @keywords internal
make_tidy_names <- function(x) paste0("...", 1:NCOL(x))

#' @keywords internal
get_dbl_name <- function(x) {
  names(get_dbl_col(x))
}

#' @keywords internal
get_dbl_col <- function(x) {
  bool <- purrr::map_lgl(.x = x, .f = is.numeric)
  x[bool]
}

#' @keywords internal
tbl_to_mtx <- function(x) {
  if (any(purrr::map_lgl(x, lubridate::is.Date))) {
    x <- x |>
      dplyr::select(where(is.numeric)) |>
      as.matrix()
  } else {
    x <- as.matrix(x)
  }
  x
}
