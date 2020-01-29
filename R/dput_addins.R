

#   __________________ #< 90a81bcce8455070cdfb04265d716dd6 ># __________________
#   dput() selection addin                                                  ####


#' @title Replaces selected code with its dput() output
#' @description RStudio Addin:
#'  Runs \code{\link[base:dput]{dput()}} on the selected code and inserts
#'  it instead of the selection.
#'
#'  See \code{Details} for how to set a key command.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
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
#' @export
#' @family addins
#' @return Inserts the output of running
#'  \code{\link[base:dput]{dput()}} on the selected code.
#'
#'  Does not return anything.
#' @details
#'  \subsection{How}{
#'  Parses and evaluates the selected code string,
#'  applies \code{\link[base:dput]{dput()}} and
#'  inserts the output instead of the selection.
#'  }
#'  \subsection{How to set up a key command in RStudio}{
#'
#'  After installing the package.
#'  Go to:
#'
#'  \code{Tools >> Addins >> Browse Addins >> Keyboard Shortcuts}.
#'
#'  Find \code{"dput() Selected"} and press its field under \code{Shortcut}.
#'
#'  Press desired key command, e.g. \code{Alt+D}.
#'
#'  Press \code{Apply}.
#'
#'  Press \code{Execute}.
#'  }
dputSelectedAddin <- function(selection = NULL, insert = TRUE, indentation = 0) {

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

  # dput() and insert/return
  if (selection != "") {
    dput_out <- apply_capture(selection, dput, envir = parent_envir)

    if (!isTRUE(insert)) {
      # Return the expectations instead of inserting them
      return(dput_out)
    } else {
      # Insert the expectations
      insert_code(dput_out, indentation = indentation)
    }

  }

  invisible()
}
