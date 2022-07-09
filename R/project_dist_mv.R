# project_np_mv -----------------------------------------------------------

#' @keywords internal
project_np_mv <- function(.invariant, .horizon = 12, .n) {
  UseMethod("project_np_mv", .invariant)
}

#' @keywords internal
project_np_mv.default <- function(.invariant, .horizon = 12, .n) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_np_mv.tbl <- function(.invariant, .horizon = 12, .n) {

  .invariant <- tbl_to_mtx(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_np_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_np_mv.xts <- function(.invariant, .horizon = 12, .n) {

  .invariant <- as.matrix(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_np_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_np_mv.matrix <- function(.invariant, .horizon = 12, .n) {

  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_np_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}




# project_t_mv ------------------------------------------------------------

#' @keywords internal
project_t_mv <- function(.invariant, .horizon = 12, .n) {
  UseMethod("project_t_mv", .invariant)
}

#' @keywords internal
project_t_mv.default <- function(.invariant, .horizon = 12, .n) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_t_mv.tbl <- function(.invariant, .horizon = 12, .n) {

  .invariant <- tbl_to_mtx(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_t_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_t_mv.xts <- function(.invariant, .horizon = 12, .n) {

  .invariant <- as.matrix(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_t_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_t_mv.matrix <- function(.invariant, .horizon = 12, .n) {

  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_t_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}




# project_ghyp_mv ---------------------------------------------------------

#' @keywords internal
project_ghyp_mv <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_ghyp_mv", .invariant)
}

#' @keywords internal
project_ghyp_mv.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_ghyp_mv.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .invariant <- tbl_to_mtx(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_ghyp_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_ghyp_mv.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .invariant <- as.matrix(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_ghyp_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_ghyp_mv.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_ghyp_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}



# project_hyp_mv ----------------------------------------------------------

#' @keywords internal
project_hyp_mv <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_hyp_mv", .invariant)
}

#' @keywords internal
project_hyp_mv.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_hyp_mv.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .invariant <- tbl_to_mtx(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_hyp_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_hyp_mv.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .invariant <- as.matrix(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_hyp_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_hyp_mv.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_hyp_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}



# project_nig_mv ----------------------------------------------------------

#' @keywords internal
project_nig_mv <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_nig_mv", .invariant)
}

#' @keywords internal
project_nig_mv.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_nig_mv.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .invariant <- tbl_to_mtx(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_nig_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_nig_mv.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .invariant <- as.matrix(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_nig_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_nig_mv.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_nig_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}



# project_vg --------------------------------------------------------------


#' @keywords internal
project_vg_mv <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_vg_mv", .invariant)
}

#' @keywords internal
project_vg_mv.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_vg_mv.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .invariant <- tbl_to_mtx(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_vg_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_vg_mv.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .invariant <- as.matrix(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_vg_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_vg_mv.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_vg_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}



# project_t_ghyp_mv -------------------------------------------------------


#' @keywords internal
project_t_ghyp_mv <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  UseMethod("project_t_ghyp_mv", .invariant)
}

#' @keywords internal
project_t_ghyp_mv.default <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @keywords internal
project_t_ghyp_mv.tbl <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .invariant <- tbl_to_mtx(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_t_ghyp_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}

#' @keywords internal
project_t_ghyp_mv.xts <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .invariant <- as.matrix(.invariant)
  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_t_ghyp_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}


#' @keywords internal
project_t_ghyp_mv.matrix <- function(.invariant, .horizon = 12, .n, .symmetric = FALSE) {

  .n_col     <- NCOL(.invariant)
  .prjct     <- vector("list", .n_col)
  for (i in 1:.n_col) {
    .prjct[[i]] <- project_t_ghyp_uv(.invariant = .invariant[ , i, drop = FALSE], .horizon = .horizon, .n = .n)
  }

  dplyr::bind_cols(.prjct)

}
