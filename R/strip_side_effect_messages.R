

#   __________________ #< 29fde22ad2b6cc22736749f39cd5f41a ># __________________
#   Strip message                                                           ####

#' @title Strip side-effect messages of non-alphanumeric characters and rethrow them
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Catches side effects (\code{error}, \code{warning}s, \code{message}s), strips the message strings of
#'  non-alphanumeric characters with \code{\link[xpectr:strip]{strip()}} and regenerates them.
#'
#'  When numbers in error messages vary slightly between systems
#'  (and this variation isn't important to catch), we can strip the numbers as well.
#'
#'  Use case: Sometimes \code{testthat} tests have differences in punctuation and newlines on different
#'  systems. By stripping both the error message and the expected message
#'  (with \code{\link[xpectr:strip]{strip()}}), we can avoid such failed tests.
#' @param x Code that potentially throws \code{warning}s, \code{message}s, or an \code{error}.
#' @inheritParams strip
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @family strippers
#' @export
#' @return Returns \code{NULL} invisibly.
#' @examples
#' # Attach packages
#' library(xpectr)
#' library(testthat)
#'
#' \dontrun{
#' strip_msg(stop("this 'dot' .\n is removed! 123"))
#' strip_msg(warning("this 'dot' .\n is removed! 123"))
#' strip_msg(message("this 'dot' .\n is removed! 123"))
#' strip_msg(message("this 'dot' .\n is removed! 123"), remove_numbers = TRUE)
#' error_fn <- function(){stop("this 'dot' .\n is removed! 123")}
#' strip_msg(error_fn())
#'
#' # With testthat tests
#' expect_error(strip_msg(error_fn()),
#'              strip("this 'dot' .\n is removed! 123"))
#' expect_error(strip_msg(error_fn(), remove_numbers = TRUE),
#'              strip("this 'dot' .\n is removed! 123", remove_numbers = TRUE))
#' }
strip_msg <- function(x, remove_spaces = FALSE, remove_numbers = FALSE,
                      remove_ansi = TRUE, lowercase = FALSE){

  # Catch x lexically
  # Needed with direct message() calls
  x <- substitute(x)
  # Create function that evaluates x in the parent to this function
  side_effects <- capture_side_effects(eval(x, envir = parent.frame(4)))
  stripper <- function(msg) {
    strip(msg, remove_spaces = remove_spaces,
          remove_numbers = remove_numbers,
          remove_ansi = remove_ansi,
          lowercase = lowercase)
  }

  # Regenerate error
  stop_if(checkmate::test_character(side_effects$error, min.len = 1),
          stripper(side_effects$error),
          sys.parent.n = 1)

  # Regenerate messages
  if (checkmate::test_character(side_effects$messages, min.len = 1)){
    plyr::l_ply(side_effects$messages, function(m){
      message_if(TRUE, stripper(m),
                 sys.parent.n = 4)
    })
  }

  # Regenerate warnings
  if (checkmate::test_character(side_effects$warnings, min.len = 1)){
    plyr::l_ply(side_effects$warnings, function(w){
      warn_if(TRUE, stripper(w),
      sys.parent.n = 4)
    })
  }

  invisible()
}


