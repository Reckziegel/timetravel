# project_mean_model_uv ---------------------------------------------------

#' @keywords internal
project_mean_model_uv <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  UseMethod("project_mean_model_uv", .invariant)
}

#' @keywords internal
project_mean_model_uv.default <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_mean_model_uv.tbl <- function(.invariant, .horizon, .n = 10000, .model, ...) {

  prjct <- project_mean_model_(.invariant = tbl_to_mtx(.invariant), .horizon = .horizon, .n = .n, .model = .model, ...)

  .dbl_name <- get_dbl_name(.invariant)

  tibble::new_tibble(
    x       = rlang::list2(!!.dbl_name := prjct[[1L]]),
    nrow    = vctrs::vec_size(prjct[[1L]]),
    class   = "projection",
    attr    = NULL,
    horizon = .horizon
  )

}

#' @keywords internal
project_mean_model_uv.xts <- function(.invariant, .horizon, .n = 10000, .model, ...) {

  prjct <- project_mean_model_(.invariant = as.matrix(.invariant), .horizon = .horizon, .n = .n, .model = .model, ...)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  tibble::new_tibble(
    x       = rlang::list2(!!.dbl_name := prjct[[1L]]),
    nrow    = vctrs::vec_size(prjct[[1L]]),
    class   = "projection",
    attr    = NULL,
    horizon = .horizon
  )

}

#' @keywords internal
project_mean_model_uv.matrix <- function(.invariant, .horizon, .n = 10000, .model, ...) {

  prjct <- project_mean_model_(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  tibble::new_tibble(
    x       = rlang::list2(!!.dbl_name := prjct[[1L]]),
    nrow    = vctrs::vec_size(prjct[[1L]]),
    class   = "projection",
    attr    = NULL,
    horizon = .horizon
  )

}

#' @keywords internal
project_mean_model_ <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  if (NCOL(.invariant) > 1) {
    rlang::abort("`.invariant` must be an univariate time series.")
  }
  assertthat::assert_that(assertthat::is.number(.horizon))
  assertthat::assert_that(assertthat::is.number(.n))
  .model <- match.arg(.model, c("ets", "arima", "auto.arima", "nnetar"))

  .model_fit <- switch(.model,
                       "ets"        = forecast::ets(y = as.vector(.invariant), ...),
                       "ar"         = stats::ar(x = as.vector(.invariant), ...),
                       "arima"      = stats::arima(x = as.vector(.invariant), ...),
                       "auto.arima" = forecast::auto.arima(y = as.vector(.invariant),...),
                       "nnetar"     = forecast::nnetar(y = as.vector(.invariant), ...))

  if (.model == "ets") {
    .model_paths <- purrr::rerun(
      .n = .n,
      forecast:::simulate.ets(object = .model_fit, nsim = .horizon, bootstrap = TRUE))
  } else if (.model == "ar") {
    .model_paths <- purrr::rerun(
      .n = .n,
      forecast:::simulate.ar(object = .model_fit, nsim = .horizon, bootstrap = TRUE))
  } else if (.model == "arima" || .model == "auto.arima") {
    .model_paths <- purrr::rerun(
      .n = .n,
      forecast:::simulate.Arima(object = .model_fit, nsim = .horizon, bootstrap = TRUE))
  } else {
    .model_paths <- purrr::rerun(
      .n = .n,
      forecast:::simulate.nnetar(object = .model_fit, nsim = .horizon, bootstrap = TRUE))
  }

  .model_paths <- .model_paths |>
    purrr::map(~mean(.invariant) + (exp(cumsum(.x)) - 1))

  simul_slice_tail <- .model_paths |>
    purrr::map_dbl(.f = utils::tail, 1)

  emp_den <- stats::density(simul_slice_tail, n = .n)
  hor_x   <- emp_den$x
  hor_cdf <- emp_den$y
  hor_cdf <- cumsum(hor_cdf) / sum(hor_cdf)

  .min <- min(simul_slice_tail)
  .max <- max(simul_slice_tail)

  out_vctr <- stats::approx(
    x      = hor_cdf,
    y      = hor_x,
    xout   = simul_slice_tail,
    method = "linear",
    rule   = 2)$x

  list(projection = out_vctr)

}



# project_garch_model_uv --------------------------------------------------

#' @keywords internal
project_garch_model_uv <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  UseMethod("project_garch_model_uv", .invariant)
}

#' @keywords internal
project_garch_model_uv.default <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_garch_model_uv.tbl <- function(.invariant, .horizon, .n = 10000, .model, ...) {

  prjct <- project_garch_model_(.invariant = tbl_to_mtx(.invariant), .horizon = .horizon, .n = .n, .model = .model, ...)

  .dbl_name <- get_dbl_name(.invariant)

  tibble::new_tibble(
    x       = rlang::list2(!!.dbl_name := prjct[[1L]]),
    nrow    = vctrs::vec_size(prjct[[1L]]),
    class   = "projection",
    attr    = NULL,
    horizon = .horizon
  )

}

#' @keywords internal
project_garch_model_uv.xts <- function(.invariant, .horizon, .n = 10000, .model, ...) {

  prjct <- project_garch_model_(.invariant = as.matrix(.invariant), .horizon = .horizon, .n = .n, .model = .model, ...)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  tibble::new_tibble(
    x       = rlang::list2(!!.dbl_name := prjct[[1L]]),
    nrow    = vctrs::vec_size(prjct[[1L]]),
    class   = "projection",
    attr    = NULL,
    horizon = .horizon
  )

}

#' @keywords internal
project_garch_model_uv.matrix <- function(.invariant, .horizon, .n = 10000, .model, ...) {

  prjct <- project_garch_model_(.invariant = .invariant, .horizon = .horizon, .n = .n, .model = .model, ...)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  tibble::new_tibble(
    x       = rlang::list2(!!.dbl_name := prjct[[1L]]),
    nrow    = vctrs::vec_size(prjct[[1L]]),
    class   = "projection",
    attr    = NULL,
    horizon = .horizon
  )

}


#' @keywords internal
project_garch_model_ <- function(.invariant, .horizon, .n = 10000, .model, ...) {
  if (NCOL(.invariant) > 1) {
    stop("`.invariant` must be an univariate time series.", call. = FALSE)
  }
  if (!inherits(.model, "uGARCHspec")) {
    stop("`.model` must be an object from `uGARCHspec` class. See `?rugarch::ugarchspec`.", call. = FALSE)
  }
  assertthat::assert_that(assertthat::is.number(.horizon))
  assertthat::assert_that(assertthat::is.number(.n))


  .fit <- rugarch::ugarchfit(spec = .model, data = .invariant, ...)

  .model_paths <- purrr::rerun(
    .n = .n,
    rugarch::ugarchsim(fit = .fit, n.sim = .horizon, startMethod = "sample")
  ) |>
    purrr::map("simulation") |>
    purrr::map("seriesSim") |>
    purrr::map(~mean(.invariant) + (exp(cumsum(.x)) - 1))

  simul_slice_tail <- .model_paths |>
    purrr::map_dbl(.f = utils::tail, 1)

  emp_den <- stats::density(simul_slice_tail, n = .n)
  hor_x <- emp_den$x
  hor_cdf <- emp_den$y
  hor_cdf <- cumsum(hor_cdf) / sum(hor_cdf)

  .min <- min(simul_slice_tail)
  .max <- max(simul_slice_tail)

  out_vctr <- stats::approx(
    x      = hor_cdf,
    y      = hor_x,
    xout   = simul_slice_tail,
    method = "linear",
    rule   = 2)$x

  list(projection = out_vctr)

}
