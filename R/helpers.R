

#   __________________ #< 86dfaf6a7ff4d9198c3b406886f4d005 ># __________________
#   Utilities                                                               ####

# Get all lists in a list with a certain name
# Use: list_of_lists %c% 'list_name'
`%c%` <- function(x, n) lapply(x, `[[`, n)
# From http://stackoverflow.com/questions/5935673/
# accessing-same-named-list-elements-of-the-list-of-lists-in-r/5936077#5936077


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

# Programmatic subset function
# Inspired by https://stackoverflow.com/questions/11880906/
# pass-subset-argument-through-a-function-to-subset
# use: p = "x>3"
# prog_subset <- function(df, subset = NULL, select = NULL, deselect = NULL, envir = NULL) {
#
#   # TODO Doesn't work when subset refers to am external variable
#   # if (is.null(envir))
#   #   envir <- parent.frame()
#   #
#   # if (!is.null(subset)) {
#   #   df <- subset(df, eval(parse(text = subset), envir = envir))
#   # }
#   if (!is.null(select)) {
#     df <- subset(df, select = select)
#   }
#   if (!is.null(deselect)) {
#     df <- subset(df, select = setdiff(names(data), deselect))
#   }
#   rownames(df) <- NULL
#   df
# }

# https://stackoverflow.com/a/55923178/11832955
# TODO Test
ndigits <- function(x){

  trunc_x <- floor(abs(x))

  if(trunc_x != 0){
    floor(log10(trunc_x)) + 1
  } else {
    1
  }

}

# assign .Random.seed in global environment
assign_random_state <- function(state, envir = globalenv(), check_existence = TRUE){
  if (!isTRUE(check_existence) ||
      exists(x = ".Random.seed")){
      assign(x = ".Random.seed",
             value = state,
             envir = envir)
    }
}


