# Mean Models -------------------------------------------------------------

#' Projection for Conditional Mean Models
#'
#' Fits and simulates conditional paths for mean models up to a given future horizon.
#'
#' @param .invariant An univariate time-series.
#' @param .horizon A \code{double}. The horizon in which the distribution should be projected.
#' @param .n A \code{double}. The number of simulations that should be performed
#' in order to compute the pdf distribution at the horizon.
#' @param .model A \code{character}. One of: \code{\link[forecast]{ets}},
#' \code{\link[stats]{ar}}, \code{\link[stats]{arima}}, \code{\link[forecast]{auto.arima}} or
#' \code{\link[forecast]{nnetar}}.
#' @param ... Additional parameters to be passed thought `.model`.
#'
#' @return A tidy \code{tibble} of the \code{next_step} class.
#'
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets[ , c(1, 2)])), ncol = 2)
#' colnames(x) <- c("DAX", "SMI")
#' project_mean_model(.invariant    = x,
#'                    .horizon      = 21,
#'                    .n            = 10,
#'                    .model        = "arima") # random-walk
#'
#' project_mean_model(.invariant    = x,
#'                    .horizon      = 21,
#'                    .n            = 10,
#'                    .model        = "nnetar") # neural-networks
project_mean_model <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  UseMethod("project_mean_model", .invariant)
}

#' @rdname project_mean_model
#' @export
project_mean_model.default <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @rdname project_mean_model
#' @export
project_mean_model.tbl <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  .invariant <- tbl_to_mtx(.invariant)
  if (NCOL(.invariant) == 1) {
    project_mean_model_uv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  } else {
    project_mean_model_mv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  }
}

#' @rdname project_mean_model
#' @export
project_mean_model.xts <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  if (NCOL(.invariant) == 1) {
    project_mean_model_uv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  } else {
    project_mean_model_mv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  }
}

#' @rdname project_mean_model
#' @export
project_mean_model.matrix <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  if (NCOL(.invariant) == 1) {
    project_mean_model_uv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  } else {
    project_mean_model_mv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  }
}

#' @rdname project_mean_model
#' @export
project_mean_model.numeric <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  if (NCOL(.invariant) == 1) {
    project_mean_model_uv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  } else {
    project_mean_model_mv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  }
}

# GARCH Models ------------------------------------------------------------

#' Projection for Conditional GARCH Models
#'
#' Fits and simulates conditional paths for GARCH models up to a given future horizon.
#'
#' @param .invariant An univariate time-series.
#' @param .horizon A \code{double}. The horizon in which the distribution should be projected.
#' @param .n A \code{double}. The number of simulations that should be performed
#' in order to compute the pdf distribution at the horizon.
#' @param .model An object from the `uGARCHspec` class. See \code{\link[rugarch]{ugarchspec}}
#' for details.
#' @param ... Additional parameters to be passed thought \code{\link[rugarch]{ugarchspec}}.
#'
#' @return A tidy \code{tibble} of the \code{next_step} class.
#'
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets[ , c(1, 3)])), ncol = 2)
#' colnames(x) <- c("DAX", "CAC")
#' project_garch_model(.invariant    = x,
#'                     .horizon      = 21,
#'                     .n            = 5,
#'                     .model        = rugarch::ugarchspec())
project_garch_model <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  UseMethod("project_garch_model", .invariant)
}

#' @rdname project_garch_model
#' @export
project_garch_model.default <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @rdname project_garch_model
#' @export
project_garch_model.tbl <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  .invariant <- tbl_to_mtx(.invariant)
  if (NCOL(.invariant) == 1) {
    project_garch_model_uv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  } else {
    project_garch_model_mv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  }
}

#' @rdname project_garch_model
#' @export
project_garch_model.xts <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  if (NCOL(.invariant) == 1) {
    project_garch_model_uv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  } else {
    project_garch_model_mv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  }
}

#' @rdname project_garch_model
#' @export
project_garch_model.matrix <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  if (NCOL(.invariant) == 1) {
    project_garch_model_uv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  } else {
    project_garch_model_mv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  }
}

#' @rdname project_garch_model
#' @export
project_garch_model.numeric <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  if (NCOL(.invariant) == 1) {
    project_garch_model_uv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  } else {
    project_garch_model_mv(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)
  }
}
