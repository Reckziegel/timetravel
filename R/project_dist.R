# Non-Parametric Projection -----------------------------------------------

#' Non-Parametric Projection
#'
#' Applies the Discrete Fourier Transform to project distributions non-parametrically
#' into the future.
#'
#' @param .invariant A tabular (non-tidy) time-series.
#' @param .horizon A number. The horizon in which the distribution should be projected.
#' @param .n An \code{integer} with the number of interpolations (Monte-Carlo simulations)
#' to be conducted.
#'
#' @return A \code{tibble} with \code{.n} rows.
#'
#' @export
#'
#' @seealso \code{\link{project_t_uv}}
#'
#' @references
#' - Meucci, A., 2005. Risk and Asset Allocation. Springer.
#'
#' - Attilio Meucci (2021). Exercises in Advanced Risk and Portfolio Management
#' (https://www.mathworks.com/matlabcentral/fileexchange/25010-exercises-in-advanced-risk-and-portfolio-management),
#' MATLAB Central File Exchange. Retrieved August 4, 2021.
#'
#' @examples
#' \donttest{
#' # log returns
#' x <- matrix(diff(log(EuStockMarkets[ , "FTSE", drop = FALSE])))
#' colnames(x) <- "FTSE"
#'
#' project_np(x, .horizon = 12, .n = 10)
#' }
project_np <- function(.invariant, .horizon = 5, .n) {
  UseMethod("project_np", .invariant)
}

#' @rdname project_np
#' @export
project_np.default <- function(.invariant, .horizon = 5, .n) {
  stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @rdname project_np
#' @export
project_np.tbl <- function(.invariant, .horizon = 5, .n) {
  .invariant <- tbl_to_mtx(.invariant)
  if (NCOL(.invariant) == 1) {
    project_np_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_np_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_np
#' @export
project_np.xts <- function(.invariant, .horizon = 5, .n) {
  if (NCOL(.invariant) == 1) {
    project_np_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_np_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_np
#' @export
project_np.matrix <- function(.invariant, .horizon = 5, .n) {
  if (NCOL(.invariant) == 1) {
    project_np_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_np_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_np
#' @export
project_np.numeric <- function(.invariant, .horizon = 5, .n) {
  if (NCOL(.invariant) == 1) {
    project_np_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_np_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}



# projection_student_t ----------------------------------------------------

#' Student-t Projection
#'
#' Applies the Discrete Fourier Transform to project the t-distribution into the future.
#'
#' @inheritParams project_np
#'
#' @return A \code{tibble} with \code{.n} rows.
#'
#' @export
#'
#' @seealso \code{\link{project_np}}
#'
#' @references
#' - Meucci, A., 2005. Risk and Asset Allocation. Springer.
#'
#' - Attilio Meucci (2021). Exercises in Advanced Risk and Portfolio Management
#' (https://www.mathworks.com/matlabcentral/fileexchange/25010-exercises-in-advanced-risk-and-portfolio-management),
#' MATLAB Central File Exchange. Retrieved August 4, 2021.
#'
#' @examples
#' \donttest{
#' # log returns
#' x <- matrix(diff(log(EuStockMarkets[ , "FTSE", drop = FALSE])))
#' colnames(x) <- "FTSE"
#'
#' project_t(x, .horizon = 12, .n = 10)
#' }
project_t <- function(.invariant, .horizon = 12, .n) {
  UseMethod("project_t", .invariant)
}

#' @rdname project_t
#' @export
project_t.default <- function(.invariant, .horizon = 12, .n) {
  stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @rdname project_t
#' @export
project_t.tbl <- function(.invariant, .horizon = 12, .n) {
  .invariant <- tbl_to_mtx(.invariant)
  if (NCOL(.invariant) == 1) {
    project_t_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_t_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_t
#' @export
project_t.xts <- function(.invariant, .horizon = 12, .n) {
  if (NCOL(.invariant) == 1) {
    project_t_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_t_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_t
#' @export
project_t.matrix <- function(.invariant, .horizon = 12, .n) {
  if (NCOL(.invariant) == 1) {
    project_t_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_t_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_t
#' @export
project_t.numeric <- function(.invariant, .horizon = 12, .n) {
  if (NCOL(.invariant) == 1) {
    project_t_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_t_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}


# projection generalized hyperbolic ---------------------------------------

#' Project of the Generalized Hyperbolic Distribution
#'
#' This function uses the fourier transform operation to project the GHD into the
#' future.
#'
#' @inheritParams project_np
#' @param .symmetric A flag. Should the estimated distribution be symmetric?
#' Defaults to \code{FALSE}.
#'
#' @return A \code{tibble} with \code{.n} rows.
#'
#' @export
#'
#' @references Meucci, A., 2005. Risk and Asset Allocation. Springer.
#'
#' @examples
#' \donttest{
#' x <- matrix(diff(log(EuStockMarkets[ , 1])))
#' colnames(x) <- "DAX"
#'
#' project_ghyp(x, .horizon = 12, .n = 10)
#' }
project_ghyp <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_ghyp", .invariant)
}

#' @rdname project_ghyp
#' @export
project_ghyp.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @rdname project_ghyp
#' @export
project_ghyp.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  .invariant <- tbl_to_mtx(.invariant)
  if (NCOL(.invariant) == 1) {
    project_ghyp_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_ghyp_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_ghyp
#' @export
project_ghyp.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_ghyp_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_ghyp_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_ghyp
#' @export
project_ghyp.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_ghyp_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_ghyp_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_ghyp
#' @export
project_ghyp.numeric <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_ghyp_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_ghyp_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

# projection hyperbolic ---------------------------------------------------


#' Project the Hyperbolic Distribution
#'
#' This function uses the fourier transform operation to project the HD into the
#' future.
#'
#' @inheritParams project_np
#' @param .symmetric Should fitted distribution be symmetric? The default is FALSE.
#'
#' @return A \code{tibble} with \code{.n} rows.
#'
#' @export
#'
#' @references Meucci, A., 2005. Risk and Asset Allocation. Springer.
#'
#' @examples
#' \donttest{
#' x <- matrix(diff(log(EuStockMarkets[ , 1])))
#' colnames(x) <- "DAX"
#'
#' project_hyp(x, .horizon = 12, .n = 10)
#' }
project_hyp <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_hyp", .invariant)
}

#' @rdname project_hyp
#' @export
project_hyp.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @rdname project_hyp
#' @export
project_hyp.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  .invariant <- tbl_to_mtx(.invariant)
  if (NCOL(.invariant) == 1) {
    project_hyp_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_hyp_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_hyp
#' @export
project_hyp.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_hyp_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_hyp_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_hyp
#' @export
project_hyp.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_hyp_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_hyp_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_hyp
#' @export
project_hyp.numeric <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_hyp_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_hyp_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}



# projection nig ----------------------------------------------------------

#' Project the Normal Inverse Gaussian Distribution
#'
#' This function uses the fourier transform operation to project the NIG into the
#' future.
#'
#' @inheritParams project_np
#' @param .symmetric Should fitted distribution be symmetric? The default is FALSE.
#'
#' @return A \code{tibble} with \code{.n} rows.
#'
#' @export
#'
#' @references Meucci, A., 2005. Risk and Asset Allocation. Springer.
#'
#' @examples
#' \donttest{
#' x <- matrix(diff(log(EuStockMarkets[ , 1])))
#' colnames(x) <- "DAX"
#'
#' project_nig(x, .horizon = 12, .n = 10)
#' }
project_nig <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_nig", .invariant)
}

#' @rdname project_nig
#' @export
project_nig.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @rdname project_nig
#' @export
project_nig.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  .invariant <- tbl_to_mtx(.invariant)
  if (NCOL(.invariant) == 1) {
    project_nig_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_nig_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_nig
#' @export
project_nig.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_nig_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_nig_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_nig
#' @export
project_nig.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_nig_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_nig_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_nig
#' @export
project_nig.numeric <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_nig_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_nig_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}



# projection variance gamma -----------------------------------------------

#' Project the Variance Gamma Distribution
#'
#' This function uses the fourier transform operation to project the VGD into the
#' future.
#'
#' @inheritParams project_np
#' @param .symmetric Should fitted distribution be symmetric? The default is FALSE.
#'
#' @return A \code{tibble} with \code{.n} rows.
#'
#' @export
#'
#' @references Meucci, A., 2005. Risk and Asset Allocation. Springer.
#'
#' @examples
#' \donttest{
#' x <- matrix(diff(log(EuStockMarkets[ , 1])))
#' colnames(x) <- "DAX"
#'
#' project_vg(x, .horizon = 12, .n = 10)
#' }
project_vg <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_vg", .invariant)
}

#' @rdname project_vg
#' @export
project_vg.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @rdname project_vg
#' @export
project_vg.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  .invariant <- tbl_to_mtx(.invariant)
  if (NCOL(.invariant) == 1) {
    project_vg_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_vg_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_vg
#' @export
project_vg.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_vg_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_vg_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_vg
#' @export
project_vg.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_vg_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_vg_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_vg
#' @export
project_vg.numeric <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_vg_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_vg_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}


# projection_t_numeric ----------------------------------------------------

#' Project the t-Distribution from the GH Optimization
#'
#' This function uses the fourier transform operation to project the t-student distribution
#' into the future.
#'
#' @inheritParams project_np
#' @param .symmetric Should fitted distribution be symmetric? The default is FALSE.
#'
#' @return A \code{tibble} with \code{.n} rows.
#'
#' @export
#'
#' @references Meucci, A., 2005. Risk and Asset Allocation. Springer.
#'
#' @examples
#' \donttest{
#' x <- matrix(diff(log(EuStockMarkets[ , 1])))
#' colnames(x) <- "DAX"
#'
#' project_t_ghyp(x, .horizon = 12, .n = 10)
#' }
project_t_ghyp <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_t_ghyp", .invariant)
}

#' @rdname project_t_ghyp
#' @export
project_t_ghyp.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @rdname project_t_ghyp
#' @export
project_t_ghyp.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  .invariant <- tbl_to_mtx(.invariant)
  if (NCOL(.invariant) == 1) {
    project_t_ghyp_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_t_ghyp_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_t_ghyp
#' @export
project_t_ghyp.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_t_ghyp_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_t_ghyp_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}


#' @rdname project_t_ghyp
#' @export
project_t_ghyp.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_t_ghyp_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_t_ghyp_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}

#' @rdname project_t_ghyp
#' @export
project_t_ghyp.numeric <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  if (NCOL(.invariant) == 1) {
    project_t_ghyp_uv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  } else {
    project_t_ghyp_mv(.invariant = .invariant, .horizon = .horizon, .n = .n)
  }
}
