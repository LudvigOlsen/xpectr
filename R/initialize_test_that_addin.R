

#   __________________ #< ae21df5a647472abc01a4d8a26621404 ># __________________
#   Initialize test_that call                                               ####


#' @title Initializes \code{test_that()} call
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Inserts code for calling \code{\link[testthat:test_that]{testthat::test_that()}}.
#'
#'  See \code{`Details`} for how to set a key command.
#' @param indentation Indentation of the code. (Numeric)
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @param insert Whether to insert the code via
#'  \code{\link[rstudioapi:rstudio-documents]{rstudioapi::insertText()}}
#'  or return it. (Logical)
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @family addins
#' @export
#' @return Inserts code for calling
#'  \code{\link[testthat:test_that]{testthat::test_that()}}.
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
#'  Find \code{"Initialize test_that()"} and press its field under \code{Shortcut}.
#'
#'  Press desired key command, e.g. \code{Alt+T}.
#'
#'  Press \code{Apply}.
#'
#'  Press \code{Execute}.
#'  }
initializeTestthatAddin <- function(insert = TRUE, indentation = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_flag(x = insert, add = assert_collection)
  checkmate::assert_integerish(x = indentation, lower = 0,
                               any.missing = FALSE,
                               null.ok = TRUE,
                               add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Get the indentation
  if (is.null(indentation)){
    indentation <- tryCatch(get_indentation(),
                            error = function(e){return(0)})
  }

  #  Create test_that initialization strings
  test_that_strings <- c(
    " ",
    "test_that(\"testing ...()\", {",
    "  xpectr::set_test_seed(42)",
    " ",
    "  # ...",
    " ",
    "})",
    " "
  )

  if (isTRUE(insert)){
    insert_code(test_that_strings, prepare = TRUE, indentation = indentation)
  } else {
    return(test_that_strings)
  }

  invisible()
}
