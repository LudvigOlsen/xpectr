
#' @title Inserts code for a checkmate assert collection
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  RStudio Addin:
#'  Inserts code for initializing and reporting a
#'  \code{\link[checkmate:AssertCollection]{checkmate assert collection}}.
#'
#'  See \code{Details} for how to set a key command.
#' @param add_comments Whether to add comments around. (Logical)
#'
#'  This makes it easy for a user to create their own addin without the comments.
#' @param indentation Indentation of the code. (Numeric)
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @param insert Whether to insert the code via
#'  \code{\link[rstudioapi:insertText]{rstudioapi::insertText()}}
#'  or return it. (Logical)
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family addins
#' @return Inserts the following (excluding the \code{----}):
#'
#'  \code{----}
#'
#'  \code{# Check arguments ####}
#'
#'  \code{assert_collection <- checkmate::makeAssertCollection()}
#'
#'  \code{# checkmate::assert_ , add = assert_collection)}
#'
#'  \code{checkmate::reportAssertions(assert_collection)}
#'
#'  \code{# End of argument checks ####}
#'
#'   \code{----}
#'
#'  Returns \code{NULL} invisibly.
#' @details
#'  \subsection{How to set up a key command in RStudio}{
#'
#'  After installing the package.
#'  Go to:
#'
#'  \code{Tools >> Addins >> Browse Addins >> Keyboard Shortcuts}.
#'
#'  Find \code{"Insert checkmate AssertCollection Code"} and press its field under \code{Shortcut}.
#'
#'  Press desired key command, e.g. \code{Alt+C}.
#'
#'  Press \code{Apply}.
#'
#'  Press \code{Execute}.
#'  }
assertCollectionAddin <- function(add_comments = TRUE, insert = TRUE, indentation = NULL){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_flag(x = add_comments, add = assert_collection)
  checkmate::assert_flag(x = insert, add = assert_collection)
  checkmate::assert_integerish(x = indentation, lower = 0,
                               any.missing = FALSE,
                               null.ok = TRUE,
                               add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Get the indentation
  if (is.null(indentation)){
    indentation <- tryCatch(
      get_indentation(),
      error = function(e) {
        return(0)
      }
    )
  }

  to_insert <- c(
    "assert_collection <- checkmate::makeAssertCollection()",
    "# checkmate::assert_ , add = assert_collection)",
    "checkmate::reportAssertions(assert_collection)"
  )

  # Add comments around
  if (isTRUE(add_comments)){
    to_insert <- c(
      "# Check arguments ####",
      to_insert,
      "# End of argument checks ####"
    )
  }

  if (isTRUE(insert)){
    insert_code(to_insert, prepare = TRUE, indentation = indentation)
  } else {
    return(to_insert)
  }

  invisible()

}
