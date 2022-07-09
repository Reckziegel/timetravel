# project_mean_model_mv ---------------------------------------------------


#' @keywords internal
project_mean_model_mv <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  UseMethod("project_mean_model_mv", .invariant)
}

#' @keywords internal
project_mean_model_mv.default <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @keywords internal
project_mean_model_mv.tbl <- function(.invariant, .horizon, .n = 10000, .model, ...) {

  .invariant <- tbl_to_mtx(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)

  for (i in 1:.n_col) {
    .prjct[[i]] <- project_mean_model_uv(
      .invariant = .invariant[ , i, drop = FALSE],
      .horizon   = .horizon,
      .n         = .n,
      .model     = .model,
      ...)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_mean_model_mv.xts <- function(.invariant, .horizon, .n = 10000, .model, ...) {

  .invariant <- as.matrix(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)

  for (i in 1:.n_col) {
    .prjct[[i]] <- project_mean_model_uv(
      .invariant = .invariant[ , i, drop = FALSE],
      .horizon   = .horizon,
      .n         = .n,
      .model     = .model,
      ...)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_mean_model_mv.matrix <- function(.invariant, .horizon, .n = 10000, .model, ...) {

  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)

  for (i in 1:.n_col) {
    .prjct[[i]] <- project_mean_model_uv(
      .invariant = .invariant[ , i, drop = FALSE],
      .horizon   = .horizon,
      .n         = .n,
      .model     = .model,
      ...)
  }

  dplyr::bind_cols(.prjct)

}


# project_garch_model_mv --------------------------------------------------


#' @keywords internal
project_garch_model_mv <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  UseMethod("project_garch_model_mv", .invariant)
}

#' @keywords internal
project_garch_model_mv.default <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @keywords internal
project_garch_model_mv.tbl <- function(.invariant, .horizon, .n = 10000, .model, ...) {

  .invariant <- tbl_to_mtx(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)

  for (i in 1:.n_col) {
    .prjct[[i]] <- project_garch_model_uv(
      .invariant = .invariant[ , i, drop = FALSE],
      .horizon   = .horizon,
      .n         = .n,
      .model     = .model,
      ...)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_garch_model_mv.xts <- function(.invariant, .horizon, .n = 10000, .model, ...) {

  .invariant <- as.matrix(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)

  for (i in 1:.n_col) {
    .prjct[[i]] <- project_garch_model_uv(
      .invariant = .invariant[ , i, drop = FALSE],
      .horizon   = .horizon,
      .n         = .n,
      .model     = .model,
      ...)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_garch_model_mv.matrix <- function(.invariant, .horizon, .n = 10000, .model, ...) {

  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)

  for (i in 1:.n_col) {
    .prjct[[i]] <- project_garch_model_uv(
      .invariant = .invariant[ , i, drop = FALSE],
      .horizon   = .horizon,
      .n         = .n,
      .model     = .model,
      ...)
  }

  dplyr::bind_cols(.prjct)

}
