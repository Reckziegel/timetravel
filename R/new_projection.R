#' COnstructor for the `projection` class.
#'
#' @keywords internal
new_projection <- function(.name, .projection, .horizon) {

  tibble::new_tibble(
    x       = rlang::list2(!!.name := .projection[[1L]]),
    nrow    = vctrs::vec_size(.projection[[1L]]),
    class   = "projection",
    attr    = .projection[[2L]],
    horizon = .horizon
  )

}
