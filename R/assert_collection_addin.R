
#' @title Inserts code for a checkmate assert collection
#' @description RStudio Addin:
#'   Inserts code for initializing and reporting a
#'   \code{\link[checkmate:AssertCollection]{checkmate assert collection}}.
#'
#'  See \code{Details} for how to set a key command.
#' @param add_comments Whether to add comments around. (Logical)
#'
#'  This makes it easy for a user to create their own addin without the comments.
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
#'  Find \code{"dput() Selected"} and press its field under \code{Shortcut}.
#'
#'  Press desired key command, e.g. \code{Alt+C}.
#'
#'  Press \code{Apply}.
#'
#'  Press \code{Execute}.
#'  }
assertCollectionAddin <- function(add_comments = TRUE){

  indentation <- get_indentation()

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

  # Insert the code
  insert_code(to_insert,
              prepare = TRUE,
              indentation = indentation)

  invisible()

}
