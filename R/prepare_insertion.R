

#   __________________ #< 814bcffd276aad37f42a762e054cf5cc ># __________________
#   Prepare expectations for insertion                                      ####


#' @title Prepare expectations for insertion
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Collapses a list/vector of expectation strings and adds the specified indentation.
#' @param strings Expectation strings. (List or Character)
#'
#'  As returned with \code{gxs_*} functions with \code{out = "return"}.
#' @param indentation Indentation to add. (Numeric)
#' @param trim_left Whether to trim whitespaces from the beginning of the collapsed string. (Logical)
#' @param trim_right Whether to trim whitespaces from the end of the collapsed string. (Logical)
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @family inserters
#' @return A string for insertion with \code{\link[rstudioapi:insertText]{rstudioapi::insertText()}}.
#' @export
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' \donttest{
#' df <- data.frame('a' = c(1, 2, 3), 'b' = c('t', 'y', 'u'),
#'                  stringsAsFactors = FALSE)
#'
#' tests <- gxs_selection("df", out = "return")
#' for_insertion <- prepare_insertion(tests)
#' for_insertion
#' rstudioapi::insertText(for_insertion)
#' }
prepare_insertion <- function(strings,
                              indentation = 0,
                              trim_left = FALSE,
                              trim_right = FALSE) {

  # Get as character vector
  strings <- unlist(strings, recursive = TRUE, use.names = FALSE)

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_character(x = strings, add = assert_collection)
  checkmate::assert_count(x = indentation, add = assert_collection)
  checkmate::assert_flag(x = trim_left, add = assert_collection)
  checkmate::assert_flag(x = trim_right, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Create string of spaces
  spaces_string <- create_space_string(n = indentation)

  # Split by newline
  strings <- unlist(strsplit(strings, split = "\n"), recursive = TRUE, use.names = FALSE)

  # Empty single space strings
  strings <- gsub("^ $", "", strings)

  # Collapse strings
  string <- paste(strings, collapse = paste0("\n", spaces_string))

  # Trimming
  to_trim <- dplyr::case_when(
    isTRUE(trim_left) && isTRUE(trim_right) ~ "both",
    isTRUE(trim_left) ~ "left",
    isTRUE(trim_right) ~ "right",
    TRUE ~ "none"
  )

  if (to_trim != "none"){
    string <- trimws(string, which = to_trim)
  }

  string
}
