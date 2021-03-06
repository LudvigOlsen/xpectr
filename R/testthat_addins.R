

#   __________________ #< c402467242308fa93e897c7ecda52e75 ># __________________
#   Generate testthat tests                                                 ####


#' @title Creates testthat tests for selected code
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Inserts relevant \code{expect_*} tests based
#'  on the evaluation of the selected code.
#'
#'  Example: If the selected code is the name of a \code{data.frame} object,
#'  it will create an \code{\link[testthat:equality-expectations]{expect_equal}}
#'  test for each column,
#'  along with a test of the column names.
#'
#'  Currently supports side effects (\code{error}, \code{warning}s, \code{message}s),
#'  \code{data.frame}s, and \code{vector}s.
#'
#'  List columns in \code{data.frame}s (like nested \code{tibble}s) are currently skipped.
#'
#'  See \code{`Details`} for how to set a key command.
#' @param selection String of code. (Character)
#'
#'  E.g. \code{"stop('This gives an expect_error test')"}.
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @param indentation Indentation of the selection. (Numeric)
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @param insert Whether to insert the expectations via
#'  \code{\link[rstudioapi:rstudio-documents]{rstudioapi::insertText()}}
#'  or return them. (Logical)
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @inheritParams gxs_selection
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @family expectation generators
#' @family addins
#' @export
#' @return Inserts \code{\link[testthat:equality-expectations]{testthat::expect_*}}
#'  unit tests for the selected code.
#'
#'  Returns \code{NULL} invisibly.
#' @details
#'  \subsection{How}{
#'  Parses and evaluates the selected code string
#'  within the parent environment (or a deep copy thereof).
#'  Depending on the output, it creates a set of unit tests
#'  (like \code{expect_equal(data[["column"]], c(1,2,3))}),
#'  and inserts them instead of the selection.
#'  }
#'  \subsection{How to set up a key command in RStudio}{
#'
#'  After installing the package.
#'  Go to:
#'
#'  \code{Tools >> Addins >> Browse Addins >> Keyboard Shortcuts}.
#'
#'  Find \code{"Insert Expectations"} and press its field under \code{Shortcut}.
#'
#'  Press desired key command, e.g. \code{Alt+E}.
#'
#'  Press \code{Apply}.
#'
#'  Press \code{Execute}.
#'  }
#' @importFrom utils capture.output head tail
#' @importFrom rlang := .data
#' @importFrom dplyr %>%
#' @importFrom stats runif
insertExpectationsAddin <- function(selection = NULL, insert = TRUE, indentation = 0, copy_env = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = selection, null.ok = TRUE,
                           add = assert_collection)
  checkmate::assert_flag(x = insert, add = assert_collection)
  checkmate::assert_flag(x = copy_env, add = assert_collection)
  checkmate::assert_integerish(x = indentation, lower = 0,
                               any.missing = FALSE,
                               null.ok = TRUE,
                               add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Get the selection and indentation
  if (is.null(selection)){
    selection <- tryCatch(
      get_selection(),
      error = function(e) {
        return("")
      }
    )
    indentation <- tryCatch(
      get_indentation(),
      error = function(e) {
        return(0)
      }
    )
  }

  # Get parent environment
  parent_envir <- parent.frame()

  #  Create expectations

  if (selection != "") {

    generator <- function(out){
      gxs_selection(selection, indentation = indentation,
                    envir = parent_envir, out = out,
                    copy_env = copy_env)
    }

    if (!isTRUE(insert)) {
      # Return the expectations instead of inserting them
      return(generator(out = "return"))
    } else {
      # Insert the expectations
      generator(out = "insert")
    }

  }

  invisible()
}

#' @rdname insertExpectationsAddin
#' @export
insertExpectationsCopyEnvAddin <- function(selection = NULL, insert = TRUE, indentation = 0, copy_env = TRUE){
  insertExpectationsAddin(
    selection = selection,
    insert = insert,
    indentation = indentation,
    copy_env = copy_env
  )
}
