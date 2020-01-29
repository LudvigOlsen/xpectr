

#   __________________ #< bff1ce9ea40e521479e8d8cf57f1bc31 ># __________________
#   Expectation creator wrappers                                            ####


##  .................. #< 3a9d08f950c35f0d601ec991f55cb5d6 ># ..................
##  Create equality expectation                                             ####


# Creates expect_equal
create_equality_expectation <- function(data,
                                        name,
                                        prefix,
                                        suffix,
                                        add_strip = FALSE,
                                        add_tolerance = FALSE,
                                        add_fixed = FALSE,
                                        indentation = 0) {

  # Create x
  x <- paste0(prefix, name, suffix)
  # Create y as string
  y_string <- paste0(prefix, "data", suffix)

  # Get current environment
  current_envir <- sys.frame(which = sys.nframe())

  # Evaluate y and capture its dput
  y <- apply_capture(
    string = y_string,
    fn = dput,
    envir = current_envir)
  y <- collapse_strings(y)

  # Add strips
  y <- add_strip(y, strip = add_strip)
  x <- add_strip(x, strip = add_strip)

  # Create test
  create_expect_equal(
    x = x,
    y = y,
    add_tolerance = add_tolerance,
    add_fixed = add_fixed,
    spaces = 2
  )
}




##  .................. #< fe7d19065058acccdc4ac9d5067ca9ae ># ..................
##  Create type and class expectations                                      ####


create_type_expectation <- function(name,
                                    type,
                                    prefix = "",
                                    suffix = "",
                                    indentation = 0) {

  # Create x
  x <- paste0(prefix, name, suffix)

  # Create test
  create_expect_type(
    x = x,
    type = add_quotation_marks(type),
    spaces = 2
  )
}

create_class_expectation <- function(name,
                                     data,
                                     prefix = "class(",
                                     suffix = ")",
                                     indentation = 0) {

  # Create x
  x <- paste0(prefix, name, suffix)
  y <- collapse_strings(capture.output(dput(class(data))))

  # Create test
  create_expect_equal(
    x = x,
    y = y,
    add_fixed = TRUE,
    spaces = 2
  )
}


##  .................. #< bffc8add6da673fdf2613e0e958327cf ># ..................
##  Create condition expectation                                            ####

create_condition_expectation <- function(name,
                                         prefix = "",
                                         suffix = "",
                                         indentation = 0) {

  # Create x
  x <- paste0(prefix, name, suffix)

  # Create test
  create_expect_true(
    x = x,
    spaces = 2
  )
}

#   __________________ #< 440b147b963f8a7fd202661bfc3b068e ># __________________
#   Creators                                                                ####
##  .................. #< abd5e8f22decefad01cca729a155076c ># ..................
##  Create names expectation                                                ####


# returns: expect_equal(names(name), c("a","b"))
create_names_expectation <- function(data, name, sample_n = NULL, indentation = 0) {

  pref_suff <- add_smpl_string(
    condition = !is.null(sample_n) && length(data) > sample_n,
    sample_n = sample_n)

  create_equality_expectation(
    data = data,
    name = name,
    prefix = paste0("names(", pref_suff[["prefix"]]),
    suffix = paste0(pref_suff[["suffix"]],")"),
    add_fixed = TRUE,
    indentation = indentation
  )
}

create_colnames_expectation <- function(data, name, sample_n = NULL, indentation = 0) {

  pref_suff <- add_smpl_string(
    condition = !is.null(sample_n) && ncol(data) > sample_n,
    sample_n = sample_n)

  create_equality_expectation(
    data = data,
    name = name,
    prefix = paste0("colnames(", pref_suff[["prefix"]]),
    suffix = paste0(pref_suff[["suffix"]],")"),
    add_fixed = TRUE,
    indentation = indentation
  )
}

create_rownames_expectation <- function(data, name, sample_n = NULL, indentation = 0) {

  pref_suff <- add_smpl_string(
    condition = !is.null(sample_n) && nrow(data) > sample_n,
    sample_n = sample_n)

  create_equality_expectation(
    data = data,
    name = name,
    prefix = paste0("rownames(", pref_suff[["prefix"]]),
    suffix = paste0(pref_suff[["suffix"]],")"),
    add_fixed = TRUE,
    indentation = indentation
  )
}


##  .................. #< cad874f32d32a8eae09090d2894d7ad5 ># ..................
##  Create dimensions expectation                                           ####


# Does not work for all types of data!
# Use for data frames only for now!
create_dim_expectation <- function(data, name, indentation = 0) {
  create_equality_expectation(
    data = data,
    name = name,
    prefix = "dim(",
    suffix = ")",
    indentation = indentation
  )
}


##  .................. #< 79a1a50b2f6fd2bc4a62237a6399b27c ># ..................
##  Create length expectation                                               ####


create_length_expectation <- function(data, name, indentation = 0) {
  create_equality_expectation(
    data = data,
    name = name,
    prefix = "length(",
    suffix = ")",
    indentation = indentation
  )
}


##  .................. #< eb074f5b5a560bb19ff907907f958da2 ># ..................
##  Create group key names expectation                                      ####


create_group_key_names_expectation <- function(data, name, indentation = 0) {
  create_equality_expectation(
    data = data,
    name = name,
    prefix = "colnames(dplyr::group_keys(",
    suffix = "))",
    add_fixed = TRUE,
    indentation = indentation
  )
}


##  .................. #< dff513f918fa186b2599f1f429f1339c ># ..................
##  Create sum of element lengths expectation                               ####


create_sum_sub_lengths_expectation <- function(data, name, indentation = 0) {
  create_equality_expectation(
    data = data,
    name = name,
    prefix = "sum(xpectr::element_lengths(",
    suffix = "))",
    indentation = indentation
  )
}


##  .................. #< 1a04985673e4647d6eb888d8bf7e76c1 ># ..................
##  Create number of levels expectation                                     ####


create_nlevels_expectation <- function(data, name, indentation = 0) {
  create_equality_expectation(
    data = data,
    name = name,
    prefix = "nlevels(",
    suffix = ")",
    indentation = indentation
  )
}


##  .................. #< eeb4f402c9068c91294cf6654a6bbb3c ># ..................
##  Create levels expectation                                               ####


create_levels_expectation <- function(data, name, sample_n = NULL, indentation = 0) {

  pref_suff <- add_smpl_string(
    condition = !is.null(sample_n) && nlevels(data) > sample_n,
    prefix = "levels(",
    suffix = ")",
    sample_n = sample_n)

  create_equality_expectation(
    data = data,
    name = name,
    prefix = pref_suff[["prefix"]],
    suffix = pref_suff[["suffix"]],
    add_fixed = TRUE,
    indentation = indentation
  )
}


##  .................. #< 27e90f45361edda8544df9c663449047 ># ..................
##  Create values as character expectation                                  ####


create_as_character_expectation <- function(data, name, sample_n = NULL, indentation = 0) {

  pref_suff <- add_smpl_string(
    condition = !is.null(sample_n) && length(data) > sample_n,
    prefix = "as.character(",
    suffix = ")",
    sample_n = sample_n)

  create_equality_expectation(
    data = data,
    name = name,
    prefix = pref_suff[["prefix"]],
    suffix = pref_suff[["suffix"]],
    add_fixed = TRUE,
    indentation = indentation
  )
}


##  .................. #< 4fae33486f86ff2385ce6225efa8c6d8 ># ..................
##  Create is factor expectation                                            ####


create_is_factor_expectation <- function(name, indentation = 0) {
  create_condition_expectation(
    name = name,
    prefix = "is.factor(",
    suffix = ")",
    indentation = indentation
  )
}


##  .................. #< 0abc01061db56c7e4fac5af452a783f2 ># ..................
##  Create column types and classes expectations                            ####


create_element_types_expectation <- function(data, name,
                                             sample_n = NULL,
                                             indentation = 0) {

  pref_suff <- add_smpl_string(
    condition = !is.null(sample_n) && length(data) > sample_n,
    prefix = "xpectr::element_types(",
    suffix = ")",
    sample_n = sample_n)

  create_equality_expectation(
    data = data,
    name = name,
    prefix = pref_suff[["prefix"]],
    suffix = pref_suff[["suffix"]],
    add_fixed = TRUE,
    indentation = indentation
  )
}

# Difference between class() and typeof():
# https://stackoverflow.com/questions/8855589/
#   a-comprehensive-survey-of-the-types-of-things-in-r-mode-and-class-and-type
create_element_classes_expectation <- function(data, name,
                                               sample_n = NULL,
                                               indentation = 0) {

  # Note: length of data frame is ncol why this also works there
  pref_suff <- add_smpl_string(
    condition = !is.null(sample_n) && length(data) > sample_n,
    prefix = "xpectr::element_classes(",
    suffix = ")",
    sample_n = sample_n)

  create_equality_expectation(
    data = data,
    name = name,
    prefix = pref_suff[["prefix"]],
    suffix = pref_suff[["suffix"]],
    add_fixed = TRUE,
    indentation = indentation
  )
}


#   __________________ #< abd5e8f22decefad01cca729a155076c ># __________________
#   Create symmetry expectation                                             ####


create_is_symmetric_expectation <- function(data, name, indentation = 0) {
  negator <- ifelse(isSymmetric(data), "", "!")
  create_condition_expectation(
    name = name,
    prefix = paste0(negator, "isSymmetric("),
    suffix = ")",
    indentation = indentation
  )
}

#   __________________ #< 55b9eab5a484e7388a1bf336aac64ff8 ># __________________
#   Create function formals expectation                                     ####


create_fn_formals_expectation <- function(data, name,
                                          sample_n = NULL,
                                          indentation = 0) {

  pref_suff <- add_smpl_string(
    condition = !is.null(sample_n) && length(formals(data)) > sample_n,
    prefix = "xpectr::simplified_formals(",
    suffix = ")",
    sample_n = sample_n)

  create_equality_expectation(
    data = data,
    name = name,
    prefix = pref_suff[["prefix"]],
    suffix = pref_suff[["suffix"]],
    add_fixed = TRUE,
    indentation = indentation
  )
}


#   __________________ #< 5f01ff7be6d77ad1ae8986bca271e1bc ># __________________
#   Create deparse expectation                                              ####


create_deparse_expectation <- function(data, name,
                                       sample_n = NULL,
                                       indentation = 0) {

  pref_suff <- add_smpl_string(
    condition = !is.null(sample_n) && length(deparse(data)) > sample_n,
    prefix = "deparse(",
    suffix = ")",
    sample_n = sample_n)

  create_equality_expectation(
    data = data,
    name = name,
    prefix = pref_suff[["prefix"]],
    suffix = pref_suff[["suffix"]],
    add_fixed = TRUE,
    indentation = indentation
  )
}


#   __________________ #< 7e5326977a07eccfa83a9397413e7ea4 ># __________________
#   Create formula expectation                                              ####


create_formula_expectation <- function(data, name,
                                       indentation = 0) {

  # Create x
  x <- name
  # Create y
  # Note: It's possible that deparse() would suffice, but this way
  # we ensure that it will fail if data is not a valid formula
  y <- paste0("as.formula(\"", collapse_strings(deparse(data)), "\")")
  y <- apply_capture(y, fn = dput)

  # Create test
  create_expect_equal(
    x = x,
    y = y,
    spaces = 2
  )
}


#   __________________ #< b3caa9bbc4f32ccc4aa583dfb8bc7a47 ># __________________
#   Create expect equal                                                     ####


create_expect_equal <- function(x, y,
                                add_tolerance = FALSE,
                                add_fixed = FALSE,
                                spaces = 2,
                                tolerance = "1e-4",
                                wrap_elements = TRUE) {

  # Create string of spaces
  spaces_string <- create_space_string(n = spaces)

  # Check that only one of the setting strings are specified
  stop_if(isTRUE(add_tolerance) && isTRUE(add_fixed),
          "Cannot add both 'tolerance' and 'fixed' setting.")

  if (isTRUE(add_tolerance)) {
    settings_string <- paste0(",\n", spaces_string,
                              paste0("tolerance = ", tolerance))
  } else if (isTRUE(add_fixed)) {
    settings_string <- paste0(",\n", spaces_string, "fixed = TRUE")
  }else {
    settings_string <- ""
  }

  # In case a string has \n, \t, etc.
  y <- escape_metacharacters(y)

  # Wrap elements
  if (isTRUE(wrap_elements)){
    y <- wrap_elements_in_string(y, max_n = 65 - spaces,
                                 indentation = spaces)
  }

  # Build expect_equal test
  paste0(
    "expect_equal(\n",
    spaces_string,
    x,
    ",\n",
    spaces_string,
    y,
    settings_string,
    ")"
  )
}

#   __________________ #< 644f57f5e60a7e987fa4035d2fa4dd47 ># __________________
#   Create expect type                                                      ####


create_expect_type <- function(x, type, spaces = 2) {

  # Create string of spaces
  spaces_string <- create_space_string(n = spaces)

  paste0(
    "expect_type(\n",
    spaces_string,
    x,
    ",\n",
    spaces_string,
    "type = ", type,
    ")"
  )
}


#   __________________ #< baf0d6d5405f18b4602c2a6143be03cf ># __________________
#   Create expect TRUE                                                      ####

create_expect_true <- function(x, spaces = 2) {

  # Create string of spaces
  spaces_string <- create_space_string(n = spaces)

  paste0(
    "expect_true(\n",
    spaces_string,
    x,
    ")"
  )
}



#   __________________ #< 8045419dc30661aa4868754ca8a7a8fa ># __________________
#   Create side effect expectation                                          ####


create_expect_side_effect <- function(x, y,
                                      side_effect_type = "error",
                                      spaces = 2,
                                      strip = TRUE) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_choice(
    x = side_effect_type,
    choices = c("error", "warning", "message"),
    add = assert_collection
  )
  checkmate::assert_flag(
    x = strip, add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  spaces_string <- create_space_string(n = spaces)

  expect_fn <- dplyr::case_when(
    side_effect_type == "error" ~ "expect_error",
    side_effect_type == "warning" ~ "expect_warning",
    side_effect_type == "message" ~ "expect_message",
    TRUE ~ "" # Won't get here anyway
  )

  y <- escape_metacharacters(y)
  y <- split_to_paste0(y, spaces = ifelse(isTRUE(strip), spaces + 14, spaces))

  paste0(
    expect_fn, "(\n",
    spaces_string,
    add_strip_msg(x, strip = strip),
    ",\n",
    spaces_string,
    add_strip(y, strip = strip),
    ",\n",
    spaces_string,
    "fixed = TRUE",
    ")"
  )
}

add_strip_msg <- function(x, strip) {
  if (isTRUE(strip))
    x <- paste0("xpectr::strip_msg(", x, ")")
  x
}

add_strip <- function(x, strip) {
  if (isTRUE(strip))
    x <- paste0("xpectr::strip(", x, ")")
  x
}


#   __________________ #< ff3057d67f7da6a3155f5034c941f142 ># __________________
#   Create comments                                                         ####

create_test_comment <- function(what, section = "test",
                                indentation = 0,
                                create_comment = TRUE){
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_flag(x = create_comment, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # No need to check things if we won't be using them
  if (!isTRUE(create_comment)){
    return(NULL)
  }
  checkmate::assert_string(x = what, add = assert_collection)
  checkmate::assert_count(x = indentation, add = assert_collection)
  checkmate::assert_choice(x = section,
                           choices = c("intro","outro","test","manual"),
                           add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####



  if (indentation > 40){
    warning("indentation > 40 characters is ignored.")
    indentation <- 40
  }

  # Remove newlines with more from 'what'
  what <- gsub("[\r\n\t\f]", "", what)
  # Reduce multiple consequtive whitespaces
  what <- gsub("[[:blank:]]+", " ", what)

  # Shorten too long calls from intro and outro comments
  if (nchar(what) > 49 - indentation){
    what <- paste0(substring(what, 1, 46 - indentation), "...")
  }

  # Create "   ####" string
  multihashtags <- paste0(create_space_string(54 - indentation - nchar(what)),
                          "####")

  # We only quote in the intro and outro
  quote_string <- ifelse(section == "test", "", "'")

  if (section == "manual"){
    comment <- paste0("# ", what)
  } else if (section == "outro"){
    comment <- paste0("## Finished testing ",
                      quote_string, what, quote_string,
                      multihashtags)
  } else {
    comment <- paste0("# Testing ", quote_string, what, quote_string)
    if (section == "intro"){
      comment <- c(paste0("#", comment, create_space_string(9), multihashtags),
                   paste0("## Initially generated by xpectr"))
    }
  }

  comment
}





#   __________________ #< ed1e08a9337603a4b73a3907674f5e1d ># __________________
#   Add xpectr::smpl string wrapper                                         ####


add_smpl_string <- function(condition, prefix="", suffix="", sample_n=NULL) {

  pref_suff <- list("prefix" = prefix,
                    "suffix" = suffix)

  if (isTRUE(condition)) {
    if(is.null(sample_n))
      stop("'sample_n' cannot be NULL when condition is TRUE.")

    pref_suff <- list(
      "prefix" = paste0("xpectr::smpl(", pref_suff[["prefix"]]),
      "suffix" = paste0(pref_suff[["suffix"]], ", n = ", sample_n, ")")
    )
  }
  pref_suff
}
#   __________________ #< cfe8c57bf8bfa870149930c756c90bf2 ># __________________
#   Wrap elements                                                           ####


wrap_elements_in_string <- function(string, max_n = 60, indentation = 0){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = string, add = assert_collection)
  checkmate::assert_count(x = max_n, add = assert_collection)
  checkmate::assert_count(x = indentation, add = assert_collection)
  # checkmate::assert_environment(x = envir, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Reformat and wrap the string
  spaces <- create_space_string(0)
  lines <- deparse(parse(text = string)[[1]], width.cutoff = max_n)
  if (!any(grepl("\"", lines))){ # Otherwise we risk removing spaces from strings
    lines <- gsub("[[:blank:]]+", " ", lines)
    lines <- trimws(lines, which = "both")
  }
  paste0(lines, collapse = paste0("\n", spaces))
}
