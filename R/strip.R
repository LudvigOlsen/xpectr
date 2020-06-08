

#   __________________ #< 646904aaf54daa74ea9d6d8c40429d96 ># __________________
#   strip                                                                   ####


#' @title Strip strings of non-alphanumeric characters
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  1) Removes any character that is not alphanumeric or a space.
#'
#'  2) (Disabled by default): Remove numbers.
#'
#'  3) Reduces multiple consecutive whitespaces to a single whitespace and trims ends.
#'
#'  Can for instance be used to simplify error messages before checking them.
#' @param strings \code{vector} of strings. (Character)
#' @param allow_na Whether to allow \code{strings}
#'  to contain \code{NA}s. (Logical)
#' @param replacement What to replace blocks of punctuation with. (Character)
#' @param remove_spaces Whether to remove all whitespaces. (Logical)
#' @param remove_numbers Whether to remove all numbers. (Logical)
#' @param remove_ansi Whether to remove ANSI control sequences. (Logical)
#' @param lowercase Whether to make the strings lowercase. (Logical)
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @family strippers
#' @return The stripped strings.
#' @export
#' @details
#' 1) ANSI control sequences are removed with \code{\link[fansi:strip_ctl]{fansi::strip_ctl()}}.
#'
#' 2) \code{gsub("[^[:alnum:][:blank:]]", replacement, strings))}
#'
#' 3) \code{gsub('[0-9]+', '', strings)} (Note: only if specified!)
#'
#' 4) \code{trimws( gsub("[[:blank:]]+", " ", strings) )}
#' (Or \code{""} if \code{remove_spaces} is \code{TRUE})
#'
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' strings <- c(
#'   "Hello! I am George.  \n\rDon't call me Frank! 123",
#'   "    \tAs that, is, not, my,     name!"
#' )
#'
#' strip(strings)
#' strip(strings, remove_spaces = TRUE)
#' strip(strings, remove_numbers = TRUE)
strip <- function(strings,
                  replacement = "",
                  remove_spaces = FALSE,
                  remove_numbers = FALSE,
                  remove_ansi = TRUE,
                  lowercase = FALSE,
                  allow_na = TRUE) {


##  .................. #< 2adf3d749542993ca3074b95c0f31e8e ># ..................
##  Argument Asserts                                                        ####


  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_flag(allow_na, add = assert_collection)
  checkmate::reportAssertions(assert_collection) # Must check allow_na first!
  checkmate::assert_character(x = strings, any.missing = allow_na,
                              add = assert_collection)
  checkmate::assert_string(replacement, add = assert_collection)
  checkmate::assert_flag(remove_spaces, add = assert_collection)
  checkmate::assert_flag(remove_numbers, add = assert_collection)
  checkmate::assert_flag(remove_ansi, add = assert_collection)
  checkmate::assert_flag(lowercase, add = assert_collection)
  checkmate::reportAssertions(assert_collection)


##  .................. #< 43d21464a1012b2a6ace37d658f5a3b6 ># ..................
##  Code                                                                    ####


  # Remove ansi escape sequence
  # Borrowed from crayon::strip_style
  if (isTRUE(remove_ansi)){
    strings <- fansi::strip_ctl(strings, ctl = "all")
  }

  # Replace all non-alphanumeric and non-space
  strings <- gsub("[^[:alnum:][:blank:]]", replacement, strings)

  # Remove numbers if specified
  if (isTRUE(remove_numbers)){
    strings <- gsub('[0-9]+', '', strings)
  }

  # Reduce multiple consecutive whitespaces
  # to a single whitespace (or non if specified)
  strings <- gsub("[[:blank:]]+", ifelse(isTRUE(remove_spaces), "", " "), strings)

  # Trim both ends for whitespaces
  strings <- trimws(strings)

  # Make lowercase
  if (isTRUE(lowercase)){
    strings <- tolower(strings)
  }

  strings
}

