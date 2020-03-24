

#   __________________ #< 0d3e1cc7677b500a32d83bc726793163 ># __________________
#   Capture side effects                                                    ####

#' @title Capture side effects
#' @description
#'  Captures errors, warnings, and messages from an expression.
#'
#'  In case of an error, no other side effects are captured.
#'
#'  Simple wrapper for \code{testthat}'s
#'  \code{\link[testthat:capture_error]{capture_error()}},
#'  \code{\link[testthat:capture_warnings]{capture_warnings()}} and
#'  \code{\link[testthat:capture_messages]{capture_messages()}}.
#'
#'  Note: Evaluates \code{expr} up to three times.
#' @param expr Expression.
#' @param envir Environment to evaluate expression in.
#' @param reset_seed Whether to reset the random state on exit. (Logical)
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return Named list with the side effects.
#' @export
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' fn <- function(raise = FALSE){
#'   message("Hi! I'm Kevin, your favorite message!")
#'   warning("G'Day Mam! I'm a warning to the world!")
#'   message("Kevin is ma name! Yesss!")
#'   warning("Hopefully the whole world will see me :o")
#'   if (isTRUE(raise)){
#'     stop("Lord Evil Error has arrived! Yeehaaa")
#'   }
#'   "the output"
#' }
#' \donttest{
#' capture_side_effects(fn())
#' capture_side_effects(fn(raise = TRUE))
#' }
capture_side_effects <- function(expr, envir = NULL, reset_seed = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_environment(x = envir, null.ok = TRUE, add = assert_collection)
  checkmate::assert_flag(x = reset_seed, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # We cannot rely on lazy evaluation
  # as it would only be called in the first call (capture_error)
  sexpr <- substitute(expr)

  if (is.null(envir))
    envir <- parent.frame()

  # We need a random state to be able to make the
  # same computation for the three evaluations
  if (!exists(".Random.seed")) {
    xpectr::set_test_seed(floor(runif(1, 0, 100000)))
    rm_seed <- TRUE
  } else {
    rm_seed <- FALSE
  }

  # Get current random state
  initial_random_state <- .Random.seed

  # Capture error
  error <- testthat::capture_error(suppress_mw(
    eval(sexpr, envir = envir)))
  error_class = class(error)

  # If no error, capture messages and warnings
  if (is.null(error)) {

    # Capture messages
    assign_random_state(initial_random_state, check_existence = FALSE)
    messages <- testthat::capture_messages(suppressWarnings(
      eval(sexpr, envir = envir)))

    # Capture warnings
    assign_random_state(initial_random_state, check_existence = FALSE)
    warnings <- testthat::capture_warnings(suppressMessages(
      eval(sexpr, envir = envir)))
  } else {
    error <- cnd_message(error)
    messages <- NULL
    warnings <- NULL
  }

  # Remove or reset random state
  if (isTRUE(rm_seed)) {
    rm(".Random.seed", envir = globalenv())
  } else if (isTRUE(reset_seed)) {
    assign_random_state(initial_random_state, check_existence = FALSE)
  }

  list(
    "error" = error,
    "error_class" = error_class,
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
    string,
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
