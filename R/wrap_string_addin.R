

#   __________________ #< 2708eaade73cef523d55b70d4eefdc80 ># __________________
#   Wrap string addin                                                       ####


#' @title Wraps the selection with paste0
#' @description Splits the selection every n characters
#'  and inserts it in a \code{\link[base:paste0]{paste0()}} call.
#'
#'  See \code{Details} for how to set a key command.
#' @param selection String of code. (Character)
#'
#'  N.B. Mainly intended for testing the addin programmatically.
#' @param indentation Indentation of the selection. (Numeric)
#'
#'  N.B. Mainly intended for testing the addin programmatically.
#' @param insert Whether to insert the wrapped text via
#'  \code{\link[rstudioapi:insertText]{rstudioapi::insertText()}}
#'  or return it. (Logical)
#'
#'  N.B. Mainly intended for testing the addin programmatically.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @return Inserts \code{paste0("first n chars", "next n chars")} etc.
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
#'  Find \code{"Insert Expectations"} and press its
#'  field under \code{Shortcut}.
#'
#'  Press desired key command, e.g. \code{Alt+P}.
#'
#'  Press \code{Apply}.
#'
#'  Press \code{Execute}.
#'  }
wrapStringAddin <- function(selection = NULL, insert = TRUE, indentation = 0) {


##  .................. #< ba972bb3fde6730407b73845cacc3613 ># ..................
##  Assert arguments                                                        ####


  # Add asserts
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = selection, null.ok = TRUE,
                           add = assert_collection)
  checkmate::assert_flag(x = insert, add = assert_collection)
  checkmate::assert_number(x = indentation, lower = 0,
                           add = assert_collection)
  checkmate::reportAssertions(assert_collection)


##  .................. #< 2b7b1d70203486ee3e9de35889785403 ># ..................
##  Get selection and context                                               ####


  # Get the selection and indentation
    if (is.null(selection)){
    selection <- get_selection()
    indentation <- get_indentation()
  }

  # Get parent environment
  parent_envir <- parent.frame()


##  .................. #< 4a81977d929b0be2b75c983f858a866a ># ..................
##  Wrap selection                                                          ####


  if (selection != "") {

    wrapped <- split_to_paste0(selection, spaces = indentation)

    if (!isTRUE(insert)) {
      # Return the wrapped string instead of inserting it
      return(wrapped)
    } else {
      # Insert the wrapped
      insert_code(list(wrapped))
    }
  }

  invisible()
}
