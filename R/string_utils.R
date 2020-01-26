

#   __________________ #< 581c92a7122e86b9d99cebb6dbb6b596 ># __________________
#   String utils                                                            ####


# Collapse list of strings once
collapse_strings <- function(strings) {
  if (length(strings) > 1) {
    strings <- paste0(strings, collapse = "")
  }
  strings
}

# Add extra \ before the
# special metacharacters: \n \r \t \v \f
escape_metacharacters <- function(string) {
  # TODO must be a way to do it in one gsub with groups?
  string <- gsub("\n", "\\n", string, fixed = TRUE)
  string <- gsub("\r", "\\r", string, fixed = TRUE)
  string <- gsub("\t", "\\t", string, fixed = TRUE)
  string <- gsub("\v", "\\v", string, fixed = TRUE)
  string <- gsub("\f", "\\f", string, fixed = TRUE)
  string <- gsub('"', '\\"', string, fixed = FALSE)
  string
}


add_quotation_marks <- function(string){
  first <- substr(string, 1, 1)
  last <- substr(string, nc <- nchar(string), nc)
  if (first != "\"") string <- paste0("\"", string)
  if (last != "\"") string <- paste0(string, "\"")
  string
}

# Create string with n spaces
create_space_string <- function(n = 2) {
  paste0(rep(" ", n), collapse = "")
}


#   __________________ #< d29a5eb0e16cdf89ef5dcf00b1b2ed60 ># __________________
#   Split/wrap string                                                       ####


# Wrap text every n characters.
# Rude but useful for long error messages.
split_string_every <- function(string, per = 60) {
  # https://stackoverflow.com/a/26497700/11832955
  n <- seq(1, nc <- nchar(string), by = per)
  splits <- substring(string, n, c(n[-1] - 1, nc))

  # Extract tail backslashes
  tail_backslashes <- gsub(".*[^\\\\*$]", "", splits, fixed = F)

  # Remove tail backslashes
  splits <- gsub("\\\\*$", "", splits)

  if (sum(nchar(tail_backslashes)) == 0) return(splits)

  # We don't want to remove the tail slashes
  # from the last string, so we create a list to easily
  # join with the others
  final_backslashes <- c(rep("", n = length(tail_backslashes) - 1),
                         tail(tail_backslashes, 1))
  # Move backslashes to next string
  tail_backslashes <- c(
    "", head(tail_backslashes,
             length(tail_backslashes) - 1))
  # Add tail backslashes at beginning of next string
  splits <- paste0(tail_backslashes, splits, final_backslashes)

  splits
}

# Split long string into elements in paste0
split_to_paste0 <- function(string, per = 60, tolerance = 10, spaces = 2) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = string, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # We only want to split it if it is too long
  if (nchar(string) > per + tolerance) {
    splits <- split_string_every(paste0(string))
  } else {
    string <- add_quotation_marks(string)
    return(string)
  }

  # Create string of spaces (7 is the length of "paste0(" )
  spaces_string <- create_space_string(n = spaces + 7)

  # Format string
  string <- paste0(
    paste0(splits, collapse = paste0("\",\n", spaces_string, "\"")))
  string <- add_quotation_marks(string)
  paste0(
    "paste0(",
    string,
    ")"
  )
}

