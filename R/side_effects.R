

#   __________________ #< 0d3e1cc7677b500a32d83bc726793163 ># __________________
#   Capture side effects                                                    ####

#' @title Capture side effects
#' @description
#'  Captures \code{error}s, \code{warning}s, and \code{message}s from an expression.
#'
#'  In case of an \code{error}, no other side effects are captured.
#'
#'  Simple wrapper for \code{testthat}'s
#'  \code{\link[testthat:capture_condition]{capture_error()}},
#'  \code{\link[testthat:capture_condition]{capture_warnings()}} and
#'  \code{\link[testthat:capture_condition]{capture_messages()}}.
#'
#'  Note: Evaluates \code{expr} up to three times.
#' @param expr Expression.
#' @param envir Environment to evaluate in. Defaults to
#'  \code{\link[base:sys.parent]{parent.frame()}}.
#' @param copy_env Whether to use deep copies of the environment when capturing side effects. (Logical)
#'
#'  Disabled by default to save memory but is often preferable to enable, e.g. when the function
#'  alters non-local variables before throwing its \code{error}/\code{warning}/\code{message}.
#' @param reset_seed Whether to reset the random state on exit. (Logical)
#' @param disable_crayon Whether to disable \code{crayon} formatting.
#'  This can remove ANSI characters from the messages. (Logical)
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return \code{named list} with the side effects.
#' @family capturers
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
#' capture_side_effects(fn(raise = TRUE), copy_env = TRUE)
#' }
capture_side_effects <- function(expr, envir = NULL, copy_env = FALSE, reset_seed = FALSE, disable_crayon = TRUE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_environment(x = envir, null.ok = TRUE, add = assert_collection)
  checkmate::assert_flag(x = reset_seed, add = assert_collection)
  checkmate::assert_flag(x = copy_env, add = assert_collection)
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

  # Disable crayon (ANSI codes)
  withr::local_options(c(crayon.enabled = !disable_crayon))

  # Capture error
  error <- testthat::capture_error(suppress_mw(
    eval(sexpr, envir = clone_env_if(envir, cond = copy_env, deep = TRUE))))
  error_class = class(error)

  # If no error, capture messages and warnings
  if (is.null(error)) {

    # Capture messages
    assign_random_state(initial_random_state, check_existence = FALSE)
    messages <- testthat::capture_messages(suppressWarnings(
      eval(sexpr, envir = clone_env_if(envir, cond = copy_env, deep = TRUE))))

    # Capture warnings
    assign_random_state(initial_random_state, check_existence = FALSE)
    warnings <- testthat::capture_warnings(suppressMessages(
      eval(sexpr, envir = clone_env_if(envir, cond = copy_env, deep = TRUE))))
  } else {
    error <- cnd_message(error, disable_crayon = disable_crayon)
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
#'  The side effects (\code{error}, \code{warning}s, \code{message}s) are returned in a \code{list}.
#'
#'  When capturing an \code{error}, no other side effects are captured.
#' @param string String of code that can be parsed and evaluated in \code{envir}.
#' @inheritParams capture_side_effects
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return \code{named list} with the side effects.
#' @family capturers
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
capture_parse_eval_side_effects <- function(string, envir = NULL, copy_env = FALSE,
                                            reset_seed = FALSE, disable_crayon = TRUE) {

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
    ", copy_env = ", deparse(copy_env),
    ", reset_seed = ", deparse(reset_seed),
    ", disable_crayon = ", deparse(disable_crayon),
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
