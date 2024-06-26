% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_dist.R
\name{project_t_ghyp}
\alias{project_t_ghyp}
\alias{project_t_ghyp.default}
\alias{project_t_ghyp.tbl}
\alias{project_t_ghyp.xts}
\alias{project_t_ghyp.matrix}
\alias{project_t_ghyp.numeric}
\title{Project the t-Distribution from the GH Optimization}
\usage{
project_t_ghyp(.invariant, .horizon = 12, .n, .symmetric = FALSE)

\method{project_t_ghyp}{default}(.invariant, .horizon = 12, .n, .symmetric = FALSE)

\method{project_t_ghyp}{tbl}(.invariant, .horizon = 12, .n, .symmetric = FALSE)

\method{project_t_ghyp}{xts}(.invariant, .horizon = 12, .n, .symmetric = FALSE)

\method{project_t_ghyp}{matrix}(.invariant, .horizon = 12, .n, .symmetric = FALSE)

\method{project_t_ghyp}{numeric}(.invariant, .horizon = 12, .n, .symmetric = FALSE)
}
\arguments{
\item{.invariant}{A tabular (non-tidy) time-series.}

\item{.horizon}{A number. The horizon in which the distribution should be projected.}

\item{.n}{An \code{integer} with the number of interpolations (Monte-Carlo simulations)
to be conducted.}

\item{.symmetric}{Should fitted distribution be symmetric? The default is FALSE.}
}
\value{
A \code{tibble} with \code{.n} rows.
}
\description{
This function uses the fourier transform operation to project the t-student distribution
into the future.
}
\examples{
\donttest{
x <- matrix(diff(log(EuStockMarkets[ , 1])))
colnames(x) <- "DAX"

project_t_ghyp(x, .horizon = 12, .n = 10)
}
}
\references{
Meucci, A., 2005. Risk and Asset Allocation. Springer.
}
