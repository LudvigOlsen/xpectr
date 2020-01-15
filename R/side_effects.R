

#   __________________ #< 0d3e1cc7677b500a32d83bc726793163 ># __________________
#   Capture side effects                                                    ####

#' @title Capture side effects
#' @description
#'  Captures errors, warnings, and messages from running a given function.
#'
#'  When the function throws an error, no other side effects are captured.
#'
#'  Simple wrapper for testthat's
#'  \code{\link[testthat:capture_error]{capture_error()}},
#'  \code{\link[testthat:capture_warnings]{capture_warnings()}} and
#'  \code{\link[testthat:capture_messages]{capture_messages()}}.
#' @param fn Function to evaluate.
#' @param ... Arguments for \code{fn}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return Named list with the side effects.
#' @export
capture_side_effects <- function(fn, ...) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_function(x = fn, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Capture error
  error <- testthat::capture_error(suppressMessages(suppressWarnings(fn(...))))
  # If no error, capture messages and warnings
  if (is.null(error)) {
    messages <- testthat::capture_messages(suppressWarnings(fn(...)))
    warnings <- testthat::capture_warnings(suppressMessages(fn(...)))
  } else {
    error <- error$message
    messages <- NULL
    warnings <- NULL
  }

  list(
    "error" = error,
    "warnings" = warnings,
    "messages" = messages,
    "has_side_effects" = any_side_effects(error, warnings, messages)
  )
}


#   __________________ #< 48a4eeb063cff1ae132cfc1682fa0d14 ># __________________
#   Capture side effects from parse eval                                    ####


#' @title Capture side effects from parse eval
#' @description
#'  Wraps string in \code{\link[xpectr:capture_side_effects]{capture_side_effects()}}
#'  before parsing and evaluating it.
#'  The side effects (error, warnings, messages) are returned in a list.
#'
#'  When capturing an error, no other side effects are captured.
#' @param string String of code that can be parsed and evaluated in \code{envir}.
#' @param envir Environment to evaluate in. Defaults to
#'  \code{\link[base:parent.frame]{parent.frame()}}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return Named list with the side effects.
#' @export
#' @examples
#' # Attach package
#' library(xpectr)
#'
#' \donttest{
#' capture_parse_eval_side_effects("stop('hi!')")
#' capture_parse_eval_side_effects("warning('hi!')")
#' capture_parse_eval_side_effects("message('hi!')")
#' }
capture_parse_eval_side_effects <- function(string, envir = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = string, add = assert_collection)
  checkmate::assert_environment(x = envir, null.ok = TRUE,
                                add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Set default envir
  if (is.null(envir)){
    envir <- parent.frame()
  }

  # Get side effects
  # Note: Without xpectr:: it complains
  # that it cannot find capture_side_effects
  catcher_string <- paste0(
    "xpectr::capture_side_effects(",
    "function(){", string, "}",
    ")"
  )

  # Parse and evaluate to get the side effects
  side_effects <- eval_string(catcher_string, envir = envir)

  side_effects
}


#   __________________ #< 8df9505f95f75a94490f398ae5c74641 ># __________________
#   Utils                                                                   ####


any_side_effects <- function(error, messages, warnings) {
  !(is.null(error) || length(error) == 0) ||
    !(is.null(messages) || length(messages) == 0) ||
    !(is.null(warnings) || length(warnings) == 0)
}
