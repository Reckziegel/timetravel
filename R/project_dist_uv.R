# project_np_uv -----------------------------------------------------------

#' @keywords internal
project_np_uv <- function(.invariant, .horizon = 12, .n) {
  UseMethod("project_np_uv", .invariant)
}

#' @keywords internal
project_np_uv.default <- function(.invariant, .horizon = 12, .n) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_np_uv.tbl <- function(.invariant, .horizon = 12, .n) {

  prjct <- project_np_(.invariant = tbl_to_mtx(.invariant), .horizon = .horizon, .n = .n)

  new_projection(.name = get_dbl_name(.invariant), .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_np_uv.xts <- function(.invariant, .horizon = 12, .n) {

  prjct <- project_np_(.invariant = as.matrix(.invariant), .horizon = .horizon, .n = .n)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_np_uv.matrix <- function(.invariant, .horizon = 12, .n) {

  prjct <- project_np_(.invariant = .invariant, .horizon = .horizon, .n = .n)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_np_ <- function(.invariant, .horizon = 12, .n) {

  assertthat::assert_that(assertthat::is.number(.horizon))
  assertthat::assert_that(assertthat::is.number(.n))
  if (NCOL(.invariant) > 1) {
    rlang::abort("`.invariant` must be an univariate time series.")
  }

  N <- 2 ^ 14

  # standardized distribution
  m <- mean(.invariant, na.rm = TRUE)
  s <- stats::sd(.invariant, na.rm = TRUE)
  Y <- (.invariant - m) / s

  # discretize initial pdf
  a  <- -stats::qnorm(10 ^ (-14), 0, sqrt(.horizon))
  h  <- 2 * a / N                            # formula 3.237
  Xi <- seq(from = -a + h, to = a, by = h)   # formula 3.238
  x  <- Xi + h / 2                           # formula 3.239

  NumPerBin     <- graphics::hist(Y, x, plot = FALSE)$counts
  f             <- 1 / h * NumPerBin / nrow(.invariant)
  f[N] <- 1 / h * (nrow(.invariant) - sum(NumPerBin[1:length(NumPerBin) - 1])) / nrow(.invariant)

  # discretized characteristic function
  Phi <- stats::fft(f)

  # projection of discretized characteristic function
  Signs <- (-1) ^ (0:(N - 1) * (.horizon - 1))
  Phi_T <- h ^ (.horizon - 1) * Signs * (Phi ^ .horizon)

  # horizon discretized pdf (standardized)
  f_T <- Re(stats::fft(Phi_T, inverse = TRUE))

  # non-standardized initial pdf
  x_start <- m + s * Xi
  f_start <- f / s
  F_start <- h * cumsum(f_start * s)

  # non-standardized horizon pdf and cdf
  x_hor <- m * .horizon + s * Xi
  f_hor <- f_T / s
  F_hor <- h * cumsum(f_hor * s)

  out_attr <- tibble::tibble(now_x   = x_start,
                             now_pdf = f_start / 100,
                             now_cdf = F_start,
                             hor_x   = x_hor,
                             hor_pdf = f_hor / N / 100,
                             hor_cdf = F_hor / N)

  .min <- min(out_attr$hor_cdf)
  .max <- max(out_attr$hor_cdf)
  .u <- stats::runif(n = .n, min = .min, max = .max)

  out_interp <- stats::approx(
    x      = out_attr$hor_cdf,
    y      = out_attr$hor_x,
    xout   = .u,
    method = "linear",
    rule   = 2,
    ties   = stats::median)$y

  list(projection = out_interp, attr = out_attr)

}




# project_t_uv ------------------------------------------------------------

#' Projection of univariate t distributions
#'
#' @keywords internal
project_t_uv <- function(.invariant, .horizon = 12, .n) {
  UseMethod("project_t_uv", .invariant)
}

#' @keywords internal
project_t_uv.default <- function(.invariant, .horizon = 12, .n) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_t_uv.tbl <- function(.invariant, .horizon = 12, .n) {
  .invariant <- tbl_to_mtx(.invariant)
  if (NCOL(.invariant) > 1) {
    rlang::abort("`.invariant` must be an univariate time series.")
  }

  if (!requireNamespace("uncover", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
      rlang::abort("The `devtools` package must be installed to conduct the projection step.")
    }
    rlang::warn("The `uncover` package is being downloaded from `Reckziegel/uncover`")
    devtools::install_github("Reckziegel/uncover")
    rlang::inform("Package Downloaded.")
  }

  fit <- attributes(uncover::fit_t(.invariant = .invariant, .symmetric = TRUE))$ghyp
  .nu <- -2 * fit@lambda
  .mu <- fit@mu
  .sd <- as.vector(fit@sigma)

  prjct <- project_t_(.nu = .nu, .mu = .mu, .sd = .sd, .horizon = .horizon, .n = .n)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_t_uv.xts <- function(.invariant, .horizon = 12, .n) {
  if (NCOL(.invariant) > 1) {
    stop("`.invariant` must be an univariate time series.", call. = FALSE)
  }

  if (!requireNamespace("uncover", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
      rlang::abort("The `devtools` package must be installed to conduct the projection step.")
    }
    rlang::warn("The `uncover` package is being downloaded from `Reckziegel/uncover`")
    devtools::install_github("Reckziegel/uncover")
    rlang::inform("Package Downloaded.")
  }

  fit <- attributes(uncover::fit_t(.invariant = .invariant, .symmetric = TRUE))$ghyp
  .nu <- -2 * fit@lambda
  .mu <- fit@mu
  .sd <- c(fit@sigma)

  prjct <- project_t_(.nu = .nu, .mu = .mu, .sd = .sd, .horizon = .horizon, .n = .n)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  tibble::new_tibble(
    x       = rlang::list2(!!.dbl_name := prjct[[1L]]),
    nrow    = vctrs::vec_size(prjct[[1L]]),
    class   = "projection",
    attr    = prjct[[2L]],
    horizon = .horizon
  )

}

#' @keywords internal
project_t_uv.matrix <- function(.invariant, .horizon = 12, .n) {
  if (NCOL(.invariant) > 1) {
    stop("`.invariant` must be an univariate time series.", call. = FALSE)
  }

  if (!requireNamespace("uncover", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
      rlang::abort("The `devtools` package must be installed to conduct the projection step.")
    }
    rlang::warn("The `uncover` package is being downloaded from `Reckziegel/uncover`")
    devtools::install_github("Reckziegel/uncover")
    rlang::inform("Package Downloaded.")
  }

  fit <- attributes(uncover::fit_t(.invariant = .invariant, .symmetric = TRUE))$ghyp
  .nu <- -2 * fit@lambda
  .mu <- fit@mu
  .sd <- as.vector(fit@sigma)

  prjct <- project_t_(.nu = .nu, .mu = .mu, .sd = .sd, .horizon = .horizon, .n = .n)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_t_ <- function(.nu, .mu, .sd, .horizon, .n) {

  assertthat::assert_that(assertthat::is.number(.nu))
  assertthat::assert_that(assertthat::is.number(.mu))
  assertthat::assert_that(assertthat::is.number(.sd))
  assertthat::assert_that(assertthat::is.number(.horizon))
  assertthat::assert_that(assertthat::is.number(.n))

  # set up grid
  N  <- 2 ^ 14  # coarseness level
  a  <- -stats::qnorm(10 ^ (-15), 0, sqrt(.horizon))
  h  <- 2 * a / N
  Xi <- seq(-a + h, a, h)

  # discretized initial pdf (standardized)
  f <- 1 / h * (stats::pt(Xi + h / 2, .nu) - stats::pt(Xi - h / 2, .nu))
  f[N] <- 1 / h * (stats::pt(-a + h / 2, .nu) - stats::pt(-a, .nu) + stats::pt(a, .nu) - stats::pt(a - h / 2, .nu))

  # discretized characteristic function
  Phi <- stats::fft(f)

  # projection of discretized characteristic function
  Signs <- (-1) ^ ((0:(N - 1)) * (.horizon - 1))
  Phi_T <- h ^ (.horizon - 1) * Signs * (Phi ^ .horizon)

  # horizon discretized pdf (standardized)
  f_T <- Re(stats::fft(Phi_T, inverse = TRUE))

  # non-standardized initial pdf
  x_start <- .mu + .sd * Xi
  f_start <- f / .sd
  F_start <- h * cumsum(f_start * .sd)

  # horizon discretized pdf and cdf (non-standardized)
  x_Hor <- .mu * .horizon + .sd * Xi
  f_Hor <- f_T / .sd
  F_Hor <- h * cumsum(f_Hor * .sd)

  out_attr <- tibble::tibble(now_x   = x_start,
                             now_pdf = f_start / 100,
                             now_cdf = F_start,
                             hor_x   = x_Hor,
                             hor_pdf = f_Hor / N / 100,
                             hor_cdf = F_Hor / N)

  .min <- min(out_attr$hor_cdf)
  .max <- max(out_attr$hor_cdf)
  .u <- stats::runif(n = .n, min = .min, max = .max)

  out_interp <- stats::approx(
    x      = out_attr$hor_cdf,
    y      = out_attr$hor_x,
    xout   = .u,
    method = "linear",
    rule   = 2,
    ties   = stats::median)$y

  list(projection = out_interp, attr = out_attr)

}



# project_ghyp_uv ---------------------------------------------------------


#' @keywords internal
project_ghyp_uv <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_ghyp_uv", .invariant)
}

#' @keywords internal
project_ghyp_uv.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_ghyp_uv.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_ghyp_(.invariant = tbl_to_mtx(.invariant), .horizon = .horizon, .n, .symmetric = .symmetric)

  .dbl_name <- get_dbl_name(.invariant)

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_ghyp_uv.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_ghyp_(.invariant = as.matrix(.invariant), .horizon = .horizon, .n, .symmetric = .symmetric)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_ghyp_uv.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_ghyp_(.invariant = .invariant, .horizon = .horizon, .n, .symmetric = .symmetric)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_ghyp_ <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  assertthat::assert_that(assertthat::is.number(.horizon))
  assertthat::assert_that(assertthat::is.number(.n))
  assertthat::assert_that(assertthat::is.flag(.symmetric))
  if (NCOL(.invariant) > 1) {
    rlang::abort("`.invariant` must be an univariate time series.")
  }

  # set up grid
  N  <- 2 ^ 14  # coarseness level
  a  <- -stats::qnorm(10 ^ (-14), 0, sqrt(.horizon))
  h  <- 2 * a / N
  Xi <- seq(-a + h, a, h)

  # fit
  fit_ghyp <- ghyp::fit.ghypuv(data      = .invariant,
                               symmetric = .symmetric,
                               silent    = TRUE,
                               control   = list(maxit = 2000))

  # check convergence
  if (!fit_ghyp@converged) {
    warning("The function `fit.ghypuv` failed to converge... switching to `fit.hypuv`.", immediate. = TRUE)
    fit_ghyp <- ghyp::fit.hypuv(.invariant, symmetric = .symmetric, silent = TRUE, control = list(maxit = 2000))
    if (!fit_ghyp@converged) {
      warning("The function `fit.hypuv` failed to converge... switching to `fit.tuv`.", immediate. = TRUE)
      fit_ghyp <- ghyp::fit.tuv(.invariant, symmetric = .symmetric, silent = TRUE, control = list(maxit = 2000))
      if (!fit_ghyp@converged) {
        warning("The function `fit.tuv` failed to converge... switching to `fit.gaussuv`", immediate. = TRUE)
        fit_ghyp <- ghyp::fit.gaussuv(.invariant)
        if (!fit_ghyp@converged) {
          rlang::abort("`.invariant` fail to converge for `fit.ghypuv`, `fit.hypuv`, `fit.tuv` and `fit.gaussuv`.
                       Use another method to project the distribution.")
        }
      }
    }
  }

  fit_ghyp_scaled <- ghyp::scale(fit_ghyp)

  # discretized initial pdf (standardized)
  f <- 1 / h * (ghyp::pghyp(Xi + h / 2, fit_ghyp_scaled) - ghyp::pghyp(Xi - h / 2, fit_ghyp_scaled))
  f[N] <- 1 / h * (ghyp::pghyp(-a + h / 2, fit_ghyp_scaled) - ghyp::pghyp(-a, fit_ghyp_scaled) + ghyp::pghyp(a, fit_ghyp_scaled) - ghyp::pghyp(a - h / 2, fit_ghyp_scaled))

  # TODO fix this latter to substitute the 'incorrect' values by the previous one.
  if (any(f > 1)) f[which(f > 1)] <- 1e-32
  if (any(f < 0)) f[which(f < 0)] <- 1e-32

  # discretized characteristic function
  Phi <- stats::fft(f)

  # projection of discretized characteristic function
  Signs <- (-1) ^ ((0:(N - 1)) * (.horizon - 1))
  Phi_T <- h ^ (.horizon - 1) * Signs * (Phi ^ .horizon)

  # horizon discretized pdf (standardized)
  f_T <- Re(stats::fft(Phi_T, inverse = TRUE))

  # shift the distribution
  mu <- fit_ghyp@mu
  sd <- c(fit_ghyp@sigma)

  # non-standardized initial pdf
  x_start <- mu + sd * Xi
  f_start <- f / sd
  F_start <- h * cumsum(f_start * sd)

  # horizon discretized pdf and cdf (non-standardized)
  x_Hor <- mu * .horizon + sd * Xi
  f_Hor <- f_T / sd
  F_Hor <- h * cumsum(f_Hor * sd)

  out_attr <- tibble::tibble(now_x   = x_start,
                             now_pdf = f_start / 100,
                             now_cdf = F_start,
                             hor_x   = x_Hor,
                             hor_pdf = f_Hor / N / 100,
                             hor_cdf = F_Hor / N)

  .min <- min(out_attr$hor_cdf)
  .max <- max(out_attr$hor_cdf)
  .u <- stats::runif(n = .n, min = .min, max = .max)

  out_interp <- stats::approx(
    x      = out_attr$hor_cdf,
    y      = out_attr$hor_x,
    xout   = .u,
    method = "linear",
    rule   = 2,
    ties   = stats::median)$y

  list(projection = out_interp, attr = out_attr)

}



# project_hyp_uv ----------------------------------------------------------

#' @keywords internal
project_hyp_uv <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_hyp_uv", .invariant)
}

#' @keywords internal
project_hyp_uv.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_hyp_uv.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_hyp_(.invariant = tbl_to_mtx(.invariant), .horizon = .horizon, .n = .n, .symmetric = .symmetric)

  .dbl_name <- get_dbl_name(.invariant)

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_hyp_uv.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_hyp_(.invariant = as.matrix(.invariant), .horizon = .horizon, .n = .n, .symmetric = .symmetric)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_hyp_uv.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_hyp_(.invariant = .invariant, .horizon = .horizon, .n = .n, .symmetric = .symmetric)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_hyp_ <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  assertthat::assert_that(assertthat::is.number(.horizon))
  assertthat::assert_that(assertthat::is.number(.n))
  assertthat::assert_that(assertthat::is.flag(.symmetric))
  if (NCOL(.invariant) > 1) {
    stop("`.invariant` must be an univariate time series.", call. = FALSE)
  }

  # set up grid
  N  <- 2 ^ 14  # coarseness level
  a  <- -stats::qnorm(10 ^ (-14), 0, sqrt(.horizon))
  h  <- 2 * a / N
  Xi <- seq(-a + h, a, h)

  # fit
  fit_ghyp <- ghyp::fit.hypuv(.invariant, symmetric = .symmetric, silent = TRUE, control = list(maxit = 2000))

  # check convergence
  if (!fit_ghyp@converged) {
    warning("The function `fit.hypuv` failed to converge... switching to `fit.tuv`.", immediate. = TRUE)
    fit_ghyp <- ghyp::fit.tuv(.invariant, symmetric = .symmetric, silent = TRUE, control = list(maxit = 2000))
    if (!fit_ghyp@converged) {
      warning("The function `fit.tuv` failed to converge... switching to `fit.gaussuv`.", immediate. = TRUE)
      fit_ghyp <- ghyp::fit.gaussuv(.invariant)
      if (!fit_ghyp@converged) {
        rlang::abort("`.invariant` fail to converge for `fit.hypuv`, `fit.tuv` and `fit.gaussuv`.
                     Use another method to project the distribution.")
      }
    }
  }

  fit_ghyp_scaled <- ghyp::scale(fit_ghyp)

  # discretized initial pdf (standardized)
  f <- 1 / h * (ghyp::pghyp(Xi + h / 2, fit_ghyp_scaled) - ghyp::pghyp(Xi - h / 2, fit_ghyp_scaled))
  f[N] <- 1 / h * (ghyp::pghyp(-a + h / 2, fit_ghyp_scaled) - ghyp::pghyp(-a, fit_ghyp_scaled) + ghyp::pghyp(a, fit_ghyp_scaled) - ghyp::pghyp(a - h / 2, fit_ghyp_scaled))

  # TODO fix this latter to substitute the 'incorrect' values by the previous one.
  if (any(f > 1)) f[which(f > 1)] <- 1e-32
  if (any(f < 0)) f[which(f < 0)] <- 1e-32

  # discretized characteristic function
  Phi <- stats::fft(f)

  # projection of discretized characteristic function
  Signs <- (-1) ^ ((0:(N - 1)) * (.horizon - 1))
  Phi_T <- h ^ (.horizon - 1) * Signs * (Phi ^ .horizon)

  # horizon discretized pdf (standardized)
  f_T <- Re(stats::fft(Phi_T, inverse = TRUE))

  # shift the distribution
  mu <- fit_ghyp@mu
  sd <- c(fit_ghyp@sigma)

  # non-standardized initial pdf
  x_start <- mu + sd * Xi
  f_start <- f / sd
  F_start <- h * cumsum(f_start * sd)

  # horizon discretized pdf and cdf (non-standardized)
  x_Hor <- mu * .horizon + sd * Xi
  f_Hor <- f_T / sd
  F_Hor <- h * cumsum(f_Hor * sd)

  out_attr <- tibble::tibble(now_x   = x_start,
                             now_pdf = f_start / 100,
                             now_cdf = F_start,
                             hor_x   = x_Hor,
                             hor_pdf = f_Hor / N / 100,
                             hor_cdf = F_Hor / N)

  .min <- min(out_attr$hor_cdf)
  .max <- max(out_attr$hor_cdf)
  .u <- stats::runif(n = .n, min = .min, max = .max)

  out_interp <- stats::approx(
    x      = out_attr$hor_cdf,
    y      = out_attr$hor_x,
    xout   = .u,
    method = "linear",
    rule   = 2,
    ties   = stats::median)$y

  list(projection = out_interp, attr = out_attr)

}



# project_nig_uv ----------------------------------------------------------

#' @keywords internal
project_nig_uv <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_nig_uv", .invariant)
}

#' @keywords internal
project_nig_uv.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_nig_uv.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_nig_(.invariant = tbl_to_mtx(.invariant), .horizon = .horizon, .n, .symmetric = .symmetric)

  .dbl_name <- get_dbl_name(.invariant)

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_nig_uv.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_nig_(.invariant = .invariant, .horizon = .horizon, .n, .symmetric = .symmetric)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_nig_uv.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_nig_(.invariant = .invariant, .horizon = .horizon, .n, .symmetric = .symmetric)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_nig_ <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  assertthat::assert_that(assertthat::is.number(.horizon))
  assertthat::assert_that(assertthat::is.number(.n))
  assertthat::assert_that(assertthat::is.flag(.symmetric))
  if (NCOL(.invariant) > 1) {
    stop("`.invariant` must be an univariate time series.", call. = FALSE)
  }

  # set up grid
  N  <- 2 ^ 14  # coarseness level
  a  <- -stats::qnorm(10 ^ (-14), 0, sqrt(.horizon))
  h  <- 2 * a / N
  Xi <- seq(-a + h, a, h)

  # fit
  fit_ghyp <- ghyp::fit.NIGuv(.invariant, symmetric = .symmetric, silent = TRUE)

  # check convergence
  if (!fit_ghyp@converged) {
    warning("The function `fit.NIGuv` failed to converge... switching to `fit.tuv`.", immediate. = TRUE)
    fit_ghyp <- ghyp::fit.tuv(.invariant, symmetric = .symmetric, silent = TRUE, control = list(maxit = 2000))
    if (!fit_ghyp@converged) {
      warning("The function `fit.tuv` failed to converge... switching to `fit.gaussuv`.", immediate. = TRUE)
      fit_ghyp <- ghyp::fit.gaussuv(.invariant)
      if (!fit_ghyp@converged) {
        rlang::abort("`.invariant` fail to converge for `fit.NIGuv`, `fit.tuv` and `fit.gaussuv`.
                     Use another method to project the distribution.")
      }
    }
  }

  fit_ghyp_scaled <- ghyp::scale(fit_ghyp)

  # discretized initial pdf (standardized)
  f <- 1 / h * (ghyp::pghyp(Xi + h / 2, fit_ghyp_scaled) - ghyp::pghyp(Xi - h / 2, fit_ghyp_scaled))
  f[N] <- 1 / h * (ghyp::pghyp(-a + h / 2, fit_ghyp_scaled) - ghyp::pghyp(-a, fit_ghyp_scaled) + ghyp::pghyp(a, fit_ghyp_scaled) - ghyp::pghyp(a - h / 2, fit_ghyp_scaled))

  # TODO fix this latter to substitute the 'incorrect' values by the previous one.
  if (any(f > 1)) f[which(f > 1)] <- 1e-32
  if (any(f < 0)) f[which(f < 0)] <- 1e-32

  # discretized characteristic function
  Phi <- stats::fft(f)

  # projection of discretized characteristic function
  Signs <- (-1) ^ ((0:(N - 1)) * (.horizon - 1))
  Phi_T <- h ^ (.horizon - 1) * Signs * (Phi ^ .horizon)

  # horizon discretized pdf (standardized)
  f_T <- Re(stats::fft(Phi_T, inverse = TRUE))

  # shift the distribution
  mu <- fit_ghyp@mu
  sd <- c(fit_ghyp@sigma)

  # non-standardized initial pdf
  x_start <- mu + sd * Xi
  f_start <- f / sd
  F_start <- h * cumsum(f_start * sd)

  # horizon discretized pdf and cdf (non-standardized)
  x_Hor <- mu * .horizon + sd * Xi
  f_Hor <- f_T / sd
  F_Hor <- h * cumsum(f_Hor * sd)

  out_attr <- tibble::tibble(now_x   = x_start,
                             now_pdf = f_start / 100,
                             now_cdf = F_start,
                             hor_x   = x_Hor,
                             hor_pdf = f_Hor / N / 100,
                             hor_cdf = F_Hor / N)

  .min <- min(out_attr$hor_cdf)
  .max <- max(out_attr$hor_cdf)
  .u <- stats::runif(n = .n, min = .min, max = .max)

  out_interp <- stats::approx(
    x      = out_attr$hor_cdf,
    y      = out_attr$hor_x,
    xout   = .u,
    method = "linear",
    rule   = 2,
    ties   = stats::median)$y

  list(projection = out_interp, attr = out_attr)

}



# project_vg_uv -----------------------------------------------------------

#' @keywords internal
project_vg_uv <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_vg_uv", .invariant)
}

#' @keywords internal
project_vg_uv.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_vg_uv.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_vg_(.invariant = tbl_to_mtx(.invariant), .horizon = .horizon, .n = .n, .symmetric = .symmetric)

  .dbl_name <- get_dbl_name(.invariant)

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_vg_uv.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_vg_(.invariant = .invariant, .horizon = .horizon, .n = .n, .symmetric = .symmetric)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_vg_uv.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_vg_(.invariant = .invariant, .horizon = .horizon, .n = .n, .symmetric = .symmetric)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_vg_ <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  assertthat::assert_that(assertthat::is.number(.horizon))
  assertthat::assert_that(assertthat::is.number(.n))
  assertthat::assert_that(assertthat::is.flag(.symmetric))
  if (NCOL(.invariant) > 1) {
    stop("`.invariant` must be an univariate time series.", call. = FALSE)
  }

  # set up grid
  N  <- 2 ^ 14  # coarseness level
  a  <- -stats::qnorm(10 ^ (-14), 0, sqrt(.horizon))
  h  <- 2 * a / N
  Xi <- seq(-a + h, a, h)

  # fit
  fit_ghyp <- ghyp::fit.VGuv(.invariant, symmetric = .symmetric, silent = TRUE)

  # check convergence
  if (!fit_ghyp@converged) {
    warning("The function `fit.VGuv` failed to converge... switching to `fit.tuv`.", immediate. = TRUE)
    fit_ghyp <- ghyp::fit.tuv(.invariant, symmetric = .symmetric, silent = TRUE, control = list(maxit = 2000))
    if (!fit_ghyp@converged) {
      warning("The function `fit.tuv` failed to converge... switching to `fit.gaussuv`.", immediate. = TRUE)
      fit_ghyp <- ghyp::fit.gaussuv(.invariant)
      if (!fit_ghyp@converged) {
        rlang::abort("`.invariant` fail to converge for `fit.NIGuv`, `fit.tuv` and `fit.gaussuv`.
                     Use another method to project the distribution.")
      }
    }
  }

  fit_ghyp_scaled <- ghyp::scale(fit_ghyp)

  # discretized initial pdf (standardized)
  f <- 1 / h * (ghyp::pghyp(Xi + h / 2, fit_ghyp_scaled) - ghyp::pghyp(Xi - h / 2, fit_ghyp_scaled))
  f[N] <- 1 / h * (ghyp::pghyp(-a + h / 2, fit_ghyp_scaled) - ghyp::pghyp(-a, fit_ghyp_scaled) + ghyp::pghyp(a, fit_ghyp_scaled) - ghyp::pghyp(a - h / 2, fit_ghyp_scaled))

  # TODO fix this latter to substitute the 'incorrect' values by the previous one.
  if (any(f > 1)) f[which(f > 1)] <- 1e-32
  if (any(f < 0)) f[which(f < 0)] <- 1e-32

  # discretized characteristic function
  Phi <- stats::fft(f)

  # projection of discretized characteristic function
  Signs <- (-1) ^ ((0:(N - 1)) * (.horizon - 1))
  Phi_T <- h ^ (.horizon - 1) * Signs * (Phi ^ .horizon)

  # horizon discretized pdf (standardized)
  f_T <- Re(stats::fft(Phi_T, inverse = TRUE))

  # shift the distribution
  mu <- fit_ghyp@mu
  sd <- c(fit_ghyp@sigma)

  # non-standardized initial pdf
  x_start <- mu + sd * Xi
  f_start <- f / sd
  F_start <- h * cumsum(f_start * sd)

  # horizon discretized pdf and cdf (non-standardized)
  x_Hor <- mu * .horizon + sd * Xi
  f_Hor <- f_T / sd
  F_Hor <- h * cumsum(f_Hor * sd)

  out_attr <- tibble::tibble(now_x   = x_start,
                             now_pdf = f_start / 100,
                             now_cdf = F_start,
                             hor_x   = x_Hor,
                             hor_pdf = f_Hor / N / 100,
                             hor_cdf = F_Hor / N)

  #tibble::new_tibble(x = out, nrow = nrow(out), class = "projection", type = "fourier_vg", horizon = .horizon)

  .min <- min(out_attr$hor_cdf)
  .max <- max(out_attr$hor_cdf)
  .u <- stats::runif(n = .n, min = .min, max = .max)

  out_interp <- stats::approx(
    x      = out_attr$hor_cdf,
    y      = out_attr$hor_x,
    xout   = .u,
    method = "linear",
    rule   = 2,
    ties   = stats::median)$y

  list(projection = out_interp, attr = out_attr)

}



# project_t_ghyp_uv -------------------------------------------------------

#' @keywords internal
project_t_ghyp_uv <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_t_ghyp_uv", .invariant)
}

#' @keywords internal
project_t_ghyp_uv.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_t_ghyp_uv.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_t_ghyp_(.invariant = tbl_to_mtx(.invariant), .horizon = .horizon, .n = .n, .symmetric = .symmetric)

  .dbl_name <- get_dbl_name(.invariant)

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_t_ghyp_uv.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_t_ghyp_(.invariant = as.matrix(.invariant), .horizon = .horizon, .n = .n, .symmetric = .symmetric)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}


#' @keywords internal
project_t_ghyp_uv.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  prjct <- project_t_ghyp_(.invariant = .invariant, .horizon = .horizon, .n = .n, .symmetric = .symmetric)

  .dbl_name <- colnames(.invariant)
  if (is.null(.dbl_name)) {
    .dbl_name <- make_tidy_names(.dbl_name)
  }

  new_projection(.name = .dbl_name, .projection = prjct, .horizon = .horizon)

}

#' @keywords internal
project_t_ghyp_ <- function(.invariant, .horizon, .n, .symmetric) {

  assertthat::assert_that(assertthat::is.number(.horizon))
  assertthat::assert_that(assertthat::is.number(.n))
  assertthat::assert_that(assertthat::is.flag(.symmetric))
  if (NCOL(.invariant) > 1) {
    rlang::abort("`.invariant` must be an univariate time series.")
  }

  # set up grid
  N  <- 2 ^ 14  # coarseness level
  a  <- -stats::qnorm(10 ^ (-14), 0, sqrt(.horizon))
  h  <- 2 * a / N
  Xi <- seq(-a + h, a, h)

  # fit
  fit_ghyp <- ghyp::fit.tuv(.invariant, symmetric = .symmetric, silent = TRUE)

  # check for convergence
  if (!fit_ghyp@converged) {
    warning("The function `fit.tuv` failed to converge switching to `fit.gaussuv`.", immediate. = TRUE)
    fit_ghyp <- ghyp::fit.gaussuv(.invariant)
    if (!fit_ghyp@converged) {
      rlang::abort("`.invariant` fail to converge for `fit.tuv` and `fit.gaussuv`.
                   Use another method to project the distribution.")
    }
  }

  fit_ghyp_scaled <- ghyp::scale(fit_ghyp)

  # discretized initial pdf (standardized)
  f <- 1 / h * (ghyp::pghyp(Xi + h / 2, fit_ghyp_scaled) - ghyp::pghyp(Xi - h / 2, fit_ghyp_scaled))
  f[N] <- 1 / h * (ghyp::pghyp(-a + h / 2, fit_ghyp_scaled) - ghyp::pghyp(-a, fit_ghyp_scaled) + ghyp::pghyp(a, fit_ghyp_scaled) - ghyp::pghyp(a - h / 2, fit_ghyp_scaled))

  # TODO fix this latter to substitute the 'incorrect' values by the previous one.
  if (any(f > 1)) f[which(f > 1)] <- 1e-32
  if (any(f < 0)) f[which(f < 0)] <- 1e-32

  # discretized characteristic function
  Phi <- stats::fft(f)

  # projection of discretized characteristic function
  Signs <- (-1) ^ ((0:(N - 1)) * (.horizon - 1))
  Phi_T <- h ^ (.horizon - 1) * Signs * (Phi ^ .horizon)

  # horizon discretized pdf (standardized)
  f_T <- Re(stats::fft(Phi_T, inverse = TRUE))

  # shift the distribution
  mu <- fit_ghyp@mu
  sd <- c(fit_ghyp@sigma)

  # non-standardized initial pdf
  x_start <- mu + sd * Xi
  f_start <- f / sd
  F_start <- h * cumsum(f_start * sd)

  # horizon discretized pdf and cdf (non-standardized)
  x_Hor <- mu * .horizon + sd * Xi
  f_Hor <- f_T / sd
  F_Hor <- h * cumsum(f_Hor * sd)

  out_attr <- tibble::tibble(now_x   = x_start,
                             now_pdf = f_start / 100,
                             now_cdf = F_start,
                             hor_x   = x_Hor,
                             hor_pdf = f_Hor / N / 100,
                             hor_cdf = F_Hor / N)

  .min <- min(out_attr$hor_cdf)
  .max <- max(out_attr$hor_cdf)
  .u <- stats::runif(n = .n, min = .min, max = .max)

  out_interp <- stats::approx(
    x      = out_attr$hor_cdf,
    y      = out_attr$hor_x,
    xout   = .u,
    method = "linear",
    rule   = 2,
    ties   = stats::median)$y

  list(projection = out_interp, attr = out_attr)

}
