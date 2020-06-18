

#   __________________ #< c1a5d30f3e6f0045ba8f573f35aea87d ># __________________
#   Suppress messages and warnings                                          ####


#' @title Suppress warnings and messages
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Run expression wrapped in both
#'  \code{\link[base:message]{suppressMessages()}} and
#'  \code{\link[base:warning]{suppressWarnings()}}.
#' @param expr Any expression to run within \code{\link[base:message]{suppressMessages()}} and
#'  \code{\link[base:warning]{suppressWarnings()}}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return The output of \code{expr}.
#' @details
#' \code{suppressWarnings(suppressMessages(expr))}
#' @export
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' fn <- function(a, b){
#'   warning("a warning")
#'   message("a message")
#'   a + b
#' }
#'
#' suppress_mw(fn(1, 5))
suppress_mw <- function(expr){
  suppressMessages(
    suppressWarnings(
      expr
    )
  )
}
