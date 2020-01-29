

#   __________________ #< c402467242308fa93e897c7ecda52e75 ># __________________
#   Generate testthat tests                                                 ####


#' @title Creates testthat tests for selected code
#' @description Inserts relevant \code{expect_*} tests based
#'  on the evaluation of the selected code.
#'
#'  Example: If the selected code is the name of a data frame object,
#'  it will create an \code{\link[testthat:expect_equal]{expect_equal}}
#'  test for each column,
#'  along with a test of the column names.
#'
#'  Currently supports side effects (error, warnings, messages),
#'  data frames, and vectors.
#'
#'  List columns in data frames (like nested tibbles) are currently skipped.
#'
#'  See \code{Details} for how to set a key command.
#' @param selection String of code. (Character)
#'
#'  E.g. \code{"stop('This gives an expect_error test')"}.
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @param indentation Indentation of the selection. (Numeric)
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @param insert Whether to insert the expectations via
#'  \code{\link[rstudioapi:insertText]{rstudioapi::insertText()}}
#'  or return them. (Logical)
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @family expectation generators
#' @family addins
#' @export
#' @return Inserts \code{\link[testthat:expect_equal]{testthat::expect_*}}
#'  unit tests for the selected code.
#'
#'  Returns \code{NULL} invisibly.
#' @details
#'  \subsection{How}{
#'  Parses and evaluates the selected code string
#'  within the parent environment.
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
insertExpectationsAddin <- function(selection = NULL, insert = TRUE, indentation = 0) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = selection, null.ok = TRUE,
                           add = assert_collection)
  checkmate::assert_flag(x = insert, add = assert_collection)
  checkmate::assert_number(x = indentation, lower = 0,
                           add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Get the selection and indentation
  if (is.null(selection)){
    selection <- get_selection()
    indentation <- get_indentation()
  }

  # Get parent environment
  parent_envir <- parent.frame()

  #  Create expectations

  if (selection != "") {

    generator <- function(out){
      gxs_selection(selection, indentation = indentation,
                    envir = parent_envir, out = out)
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
