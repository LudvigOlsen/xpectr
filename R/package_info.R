#' xpectr: A package for generating tests for \code{testthat} unit testing
#'
#' A set of utilities and RStudio addins for generating tests.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @docType package
#' @name xpectr
NULL


# R CMD check NOTE handling
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))

# Never used, but removes R CMD check NOTEs
rcmd_import_handler <- function(){
  lifecycle::deprecate_soft()
}
