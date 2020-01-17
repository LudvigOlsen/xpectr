# Get all lists in a list with a certain name
# Use: list_of_lists %c% 'list_name'
`%c%` <- function(x, n) lapply(x, `[[`, n)
# From http://stackoverflow.com/questions/5935673/
# accessing-same-named-list-elements-of-the-list-of-lists-in-r/5936077#5936077

has_na <- function(v) {
  sum(is.na(v)) > 0
}

# Skips testthat test, if the R version is below 3.6.0
# WHY? Due to the change in the random sampling generator
# tests fail on R versions below 3.6.0.
# It is possible to fix this by using the old generator for
# unit tests, but that would take a long time to convert,
# and most likely the code works the same on v3.5
skip_test_if_old_R_version <- function(min_R_version = "3.6") {
  if (getRversion()$minor < strsplit(
      min_R_version, ".", fixed = TRUE)[[1]][[2]]) {
    testthat::skip(message = paste0(
      "Skipping test as R version is < ", min_R_version, "."))
  }
}

# Wrapper for setting seed with the sample generator for R versions <3.6
# Used for unittests
# Partly contributed by R. Mark Sharp
set_seed_for_R_compatibility <- function(seed = 1) {
  if ((getRversion()$major == 3 &&
       getRversion()$minor >= 6) ||
       getRversion()$major > 3) {
    args <- list(seed, sample.kind = "Rounding")
  } else {
    args <- list(seed)
  }
  suppressWarnings(do.call(set.seed, args))
}


is_logical_scalar_not_na <- function(arg) {
  rlang::is_scalar_logical(arg) && !is.na(arg)
}


# Wraps tibble::add_column
reposition_column <- function(data, col,
                              .before = NULL,
                              .after = NULL) {
  col_values <- data[[col]]
  data[[col]] <- NULL
  data %>%
    tibble::add_column(!!(col) := col_values,
                       .before = .before,
                       .after = .after)
}

# Remove NAs and empty "" names
non_empty_names <- function(x) {
  ns <- names(x)
  ns <- ns[!is.na(ns)]
  ns[nzchar(ns, keepNA = TRUE)]
}

# Never used, but removes R CMD check NOTEs
rcmd_import_handler <- function(){
  lifecycle::deprecate_soft()
}
