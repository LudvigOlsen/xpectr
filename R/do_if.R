

#   __________________ #< a4e6c4b8d57e42c270d1c97b24c965f8 ># __________________
#   Do if family                                                            ####


#' @title Simple side effect functions
#' @name stop_if
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  If the \code{`condition`} is \code{TRUE},
#'  generate \code{error}/\code{warning}/\code{message} with the supplied message.
#' @param condition The condition to check. (Logical)
#' @param message Message. (Character)
#'
#'  Note: If \code{NULL}, the \code{`condition`} will be used as message.
#' @param sys.parent.n The number of generations to go back when calling the message function.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return Returns \code{NULL} invisibly.
#' @details
#'  When \code{`condition`} is \code{FALSE}, they return \code{NULL} invisibly.
#'
#'  When \code{`condition`} is \code{TRUE}:
#'
#'  \subsection{stop_if()}{
#'  Throws error with the supplied message.
#'  }
#'  \subsection{warn_if()}{
#'  Throws warning with the supplied message.
#'  }
#'  \subsection{message_if()}{
#'  Generates message with the supplied message.
#'  }
#' @examples
#' # Attach packages
#' library(xpectr)
#' \dontrun{
#' a <- 0
#' stop_if(a == 0, "'a' cannot be 0.")
#' warn_if(a == 0, "'a' was 0.")
#' message_if(a == 0, "'a' was so kind to be 0.")
#' }
#' @importFrom rlang :=
NULL

# NOTE Aliases only work when building the package
# so use do_if to see docs

add_condition_prefix <- function(m) {
  paste0("This was TRUE: ", m)
}

#   __________________ #< 137557485c74acc6406cd780ee5b6edb ># __________________
#   stop_if                                                                 ####

#' @rdname stop_if
#' @export
stop_if <- function(condition, message = NULL, sys.parent.n = 0L) {

  if (condition) {

    # If message is NULL, get condition
    if (is.null(message)) {
      message <- tryCatch( # Doesn't really work to do this in subfunction
        deparse(
          substitute(
            expr = condition,
            env = sys.frame(which = sys.nframe())
          )
        ),
        error = function(e) {
          stop("Cannot use 'condition' as message. Please provide a message.")
        },
        warning = function(w) {
          stop("Cannot use 'condition' as message. Please provide a message.")
        }
      )
      # Add "This was TRUE: "
      message <- add_condition_prefix(message)
    }

    stop(
      simpleError(
        message,
        call = if (p <- sys.parent(sys.parent.n+1)) sys.call(p)))
  }

  invisible()
}


#   __________________ #< 80cb7ed785f3edf07e65abebbe27d157 ># __________________
#   warn_if                                                                 ####


#' @rdname stop_if
#' @export
warn_if <- function(condition, message = NULL, sys.parent.n = 0L) {

  if (condition) {

    # If message is NULL, get condition
    if (is.null(message)) {
      message <- tryCatch( # Doesn't really work to do this in subfunction
        deparse(
          substitute(
            expr = condition,
            env = sys.frame(which = sys.nframe())
          )
        ),
        error = function(e) {
          stop("Cannot use 'condition' as message. Please provide a message.")
        },
        warning = function(w) {
          stop("Cannot use 'condition' as message. Please provide a message.")
        }
      )
      # Add "This was TRUE: "
      message <- add_condition_prefix(message)
    }

    warning(
      simpleWarning(
        message,
        call = if (p <- sys.parent(sys.parent.n+1)) sys.call(p)))
  }

  invisible()
}


#   __________________ #< 0b979d2011c6ffa7f887ce415b9612b1 ># __________________
#   message_if                                                              ####


#' @rdname stop_if
#' @export
message_if <- function(condition, message = NULL, sys.parent.n = 0L) {

  if (condition) {

    # If message is NULL, get condition
    if (is.null(message)) {
      message <- tryCatch( # Doesn't really work to do this in subfunction
        deparse(
          substitute(
            expr = condition,
            env = sys.frame(which = sys.nframe())
          )
        ),
        error = function(e) {
          stop("Cannot use 'condition' as message. Please provide a message.")
        },
        warning = function(w) {
          stop("Cannot use 'condition' as message. Please provide a message.")
        }
      )
      # Add "This was TRUE: "
      message <- add_condition_prefix(message)
    }

    message(
      simpleMessage(
        paste0(message,"\n"),
        call = if (p <- sys.parent(sys.parent.n + 1)) sys.call(p)))
  }

  invisible()
}


#   __________________ #< 67ff2c242b343b96414a9f3df8ab7067 ># __________________
#   identity_if                                                             ####


# Not sure this is useful
# It seems to be no different than ifelse
# Except that it doesn't check types and lengths, etc.
# So perhaps look into whether it should be a "free" ifelse?
# Keep internally for now
identity_if <- function(condition, x, otherwise = invisible()) {
  if (condition) {
    return(x)
  }

  otherwise
}


#   __________________ #< 53196784009271631dda5f0ea3f9e5da ># __________________
#   do_if                                                                   ####


# Not sure this is useful
# R already has lazy evaluation
# And it's not easier to read than a simple for loop
# Keep internally for now
do_if <- function(condition, fn, ..., otherwise = invisible()) {
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_function(x = fn, add = assert_collection)
  checkmate::reportAssertions(assert_collection)

  if (condition) {
    return(fn(...))
  }

  otherwise
}
