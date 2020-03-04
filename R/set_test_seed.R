
#' @title Set random seed for unit tests
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  In order for tests to be compatible with R versions \code{< 3.6.0},
#'  we set the \code{sample.kind} argument in \code{\link[base:set.seed]{set.seed()}}
#'  to \code{"Rounding"} when using R versions \code{>= 3.6.0}.
#' @param seed Random seed.
#' @param ... Named arguments to \code{\link[base:set.seed]{set.seed()}}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @author R. Mark Sharp
#' @return \code{NULL}.
#' @export
#' @details
#'  Initially contributed by R. Mark Sharp (github: @@rmsharp).
set_test_seed <- function(seed = 42, ...) {
  if ((getRversion()$major == 3 &&
       getRversion()$minor >= 6) ||
      getRversion()$major > 3) {
    args <- list(seed, sample.kind = "Rounding", ...)
  } else {
    args <- list(seed, ...)
  }
  suppressWarnings(do.call(set.seed, args))
}
