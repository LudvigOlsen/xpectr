

#   __________________ #< ad4fd4c3bf0175135055204d9012d9ac ># __________________
#   Navigate to test file                                                   ####


#' @title Navigates to test file
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  \code{RStudio} Addin:
#'  Extracts file name and (possibly) line number of a test file
#'  from a selection or from clipboard content.
#'  Navigates to the file and places the cursor at the line number.
#'
#'  Supported types of strings: \code{"test_x.R:3"}, \code{"test_x.R#3"}, \code{"test_x.R"}.
#'
#'  The string must start with \code{"test_"} and contain \code{".R"}.
#'  It is split at either \code{":"} or \code{"#"}, with the second element (here \code{"3"}) being
#'  interpreted as the line number.
#'
#'  See \code{Details} for how to set a key command.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param selection String with file name and line number. (Character)
#'
#'  E.g. \code{"test_x.R:3:"}, which navigates to the third line of \code{"/tests/testthat/test_x.R"}.
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @param navigate Whether to navigate to the file or return the extracted file name and line number. (Logical)
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @param abs_path Whether to return the full path or only the file name when \code{navigate} is \code{FALSE}.
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @export
#' @family addins
#' @return Navigates to file and line number.
#'
#'  Does not return anything.
#' @details
#'  \subsection{How to set up a key command in RStudio}{
#'
#'  After installing the package.
#'  Go to:
#'
#'  \code{Tools >> Addins >> Browse Addins >> Keyboard Shortcuts}.
#'
#'  Find \code{"Go To Test File"} and press its field under \code{Shortcut}.
#'
#'  Press desired key command, e.g. \code{Alt+N}.
#'
#'  Press \code{Apply}.
#'
#'  Press \code{Execute}.
#'  }
navigateTestFileAddin <- function(selection = NULL, navigate = TRUE, abs_path = TRUE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = selection, null.ok = TRUE,
                           add = assert_collection)
  checkmate::assert_flag(x = navigate, add = assert_collection)
  checkmate::assert_flag(x = abs_path, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Get the selection and indentation
  if (is.null(selection)){
    selection <- tryCatch(get_selection(),
                          error = function(e){return("")},
                          message = function(m){return("")})
  }

  if (selection == ""){ # TODO Make sure not to run on CRAN
    selection <- read_clipboard()
  }

  if (is.null(selection) || selection == ""){
    message("selection/clipboard content was empty/unavailable.")
    return(invisible())
  }

  if (!grepl(pattern = ".R", x = selection, fixed = TRUE)){
    stop("selection/clipboard content did not include '.R'.")
  }

  if (grepl(pattern = ".R#", x = selection, fixed = TRUE)){
    sep <- "#"
  } else {
    sep <- ":"
  }

  # Extract file name and (possibly) line number
  selection <- trimws(selection, which = "both")
  split_sel <- unlist(strsplit(selection, sep))
  if (length(split_sel) > 1){
    file_name <- split_sel[[1]]
    line_num <- split_sel[[2]]
    if (!grepl("^[0-9]", line_num)){
      stop("line number was not a number.")
    }
    line_num <- gsub("^([0-9]+).*$", "\\1", line_num)
  } else {
    file_name <- split_sel
    line_num <- -1
  }

  # File name
  if (substring(file_name, 1, 5) != "test_"){
    stop("extracted file name must start with 'test_'.")
  }
  if (substring(file_name, nchar(file_name)-1, nchar(file_name)) != ".R"){
    file_name <- paste0(file_name, ".R")
  }
  if (grepl(pattern = "[[:space:]]", x = file_name)){
    stop("found white space in extracted file named.")
  }
  # Line number
  line_num <- tryCatch(as.integer(line_num),
                       error = function(e){return(NA)},
                       warning = function(w){return(NA)})
  if (is.null(line_num) || is.na(line_num)){
    stop("extracted line number could not be converted to 'integer'.")
  }

  # Create absolute path
  full_file_path <- paste0(getwd(), "/tests/testthat/", file_name)

  # Navigate or return extractions

  if (isTRUE(navigate)){
    # Open file and set cursor at line number
    rstudioapi::navigateToFile(file = full_file_path,
                               line = line_num)
  } else {
    output <- list()
    if (isTRUE(abs_path)) output[["file"]] <- full_file_path
    else output[["file"]] <- file_name
    output[["line_number"]] <- line_num
    output
  }

}


