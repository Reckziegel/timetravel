#' Visualize the projected PDF distribution
#'
#' @param object An object of the \code{projection} class.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @importFrom ggplot2 autoplot
#' @rdname autoplot
#'
#' @examples
#' library(ggplot2)
#'
#' x <- as.matrix(diff(log(EuStockMarkets[ , 1])))
#'
#' proj3  <- project_t(x, .horizon = 3, .n = 2000)
#' proj5  <- project_t(x, .horizon = 5, .n = 2000)
#' proj10 <- project_t(x, .horizon = 10, .n = 2000)
#'
#' autoplot(proj3)
#' autoplot(proj5)
#' autoplot(proj10)
autoplot.projection <- function(object) {

  if (inherits(object, "projection")) {

    .proj <- attributes(object)$attr

    .horizon <- attributes(object)$horizon

    .proj |>
      tidyr::pivot_longer(cols = -.data$hor_x) |>
      dplyr::filter(.data$name %in% c("now_pdf", "hor_pdf")) |>
      dplyr::mutate(name = dplyr::case_when(
        name == "now_pdf" ~ "Today",
        name == "hor_pdf" ~ paste("In", .horizon, "Days"),
        TRUE              ~ name)) |>
      ggplot2::ggplot(ggplot2::aes(x = .data$hor_x, y = .data$value, color = .data$name)) +
      ggplot2::geom_line() +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::scale_x_continuous(labels = scales::percent_format()) +
      ggplot2::labs(x = NULL, y = NULL, color = "Projection")

  } else {

    rlang::abort("`object` must be an object of the projection class.")

  }

}


#' @rdname autoplot
#' @importFrom graphics plot
plot.projection <- function(object) {
  print(ggplot2::autoplot(object))
}
