

#   __________________ #< 2451d1703a5d7b006fa4e72e2dcc59ed ># __________________
#   Create Expectations Data Frame                                          ####


create_expectations_data_frame <- function(data, name = NULL, indentation = 0,
                                           sample_n = 30,
                                           tolerance = "1e-4",
                                           add_comments = TRUE) {


##  .................. #< 2271fda988ae80e314ffc80ad1364070 ># ..................
##  Assert arguments                                                        ####


  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, add = assert_collection)
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    tolerance = tolerance,
    sample_n = sample_n,
    add_comments = add_comments
  )
  checkmate::reportAssertions(assert_collection)


##  .................. #< 2da3e73be55460e78db79bd7977467c9 ># ..................
##  Create expectation                                                      ####


  if (is.null(name)) {
    name <- trimws(deparse(substitute(data)))
  }

  # Extra expectations
  # NOTE: Some must come before sampling!
  name_expectation <- create_name_expectation(data, name, indentation = indentation)
  dim_expectation <- create_dim_expectation(data, name, indentation = indentation)
  group_key_names_expectation <- create_group_key_names_expectation(data, name, indentation = indentation)

  # Whether to sample data
  sample_data <- !is.null(sample_n) && nrow(data) > sample_n

  # Sample data
  if (isTRUE(sample_data)){
    data <- smpl(data = data,
                 n = sample_n,
                 keep_order = TRUE)
    }

  # Create expect_equal expectations
  column_expectations <- plyr::llply(colnames(data), function(col_name) {
    # Get current column
    current_col <- data[[col_name]]
    if (is.list(current_col)) {
      return(NULL)
    }

    # Left side of expectation
    x <- paste0(name, "[[\"", col_name, "\"]]")
    if (isTRUE(sample_data)){
      x <- paste0("xpectr::smpl(", x, ", n = ", sample_n, ")")
    }
    # Right side of expectation
    y <- capture.output(dput(current_col))
    # In case dput spanned multiple lines
    # we collapse them to one string
    y <- collapse_strings(y)

    # Sanity check
    if (length(y) > 1) {
      return(NULL)
    }

    # Create expect_equal text
    create_expect_equal(x, y,
                        add_tolerance = is.numeric(current_col),
                        add_fixed = is.character(current_col),
                        spaces = 2 + indentation,
                        tolerance = tolerance)
  })

  null_indices <- get_null_indices(column_expectations)
  if (length(null_indices) > 0) {

    # Warn about skipped elements
    skipped_cols <- colnames(data)[null_indices]
    plural_s <- ifelse(length(skipped_cols) > 1, "s", "")
    warning(paste0(
      "Skipped column", plural_s, " ",
      paste0(skipped_cols, collapse = ", "),
      "."
    ))

    # Remove NULLS
    column_expectations <- column_expectations[-null_indices]
  }

  # Collect expectations and add comments
  expectations <-
    c(
      create_test_comment(name, section = "intro",
                          indentation = indentation,
                          create_comment = add_comments),
      create_test_comment("column values", indentation = indentation,
                          create_comment = add_comments),
      column_expectations,
      create_test_comment("column names", indentation = indentation,
                          create_comment = add_comments),
      name_expectation,
      create_test_comment("dimensions", indentation = indentation,
                          create_comment = add_comments),
      dim_expectation,
      create_test_comment("group keys", indentation = indentation,
                          create_comment = add_comments),
      group_key_names_expectation,
      create_test_comment(name, section = "outro", indentation = indentation,
                          create_comment = add_comments)
    )

  expectations
}


#   __________________ #< 57651c0852811eeaef463b8c09390020 ># __________________
#   Create expectations vector                                              ####


# Only split into multiple tests when all elements are named
# plus some other checks
create_expectations_vector <- function(data, name = NULL, indentation = 0,
                                       sample_n = 30,
                                       tolerance = "1e-4",
                                       add_comments = TRUE) {


##  .................. #< 00dc0af83dcb4c3bb7b5e04a48b8bfbb ># ..................
##  Assert arguments                                                        ####


  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_vector(x = data, add = assert_collection)
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    tolerance = tolerance,
    sample_n = sample_n,
    add_comments = add_comments
  )
  checkmate::reportAssertions(assert_collection)

##  .................. #< fe958b30a0397775f7e311bcff15a411 ># ..................
##  Create expectations                                                     ####


  if (is.null(name)) {
    name <- trimws(deparse(substitute(data)))
  }

  # Create length expectation
  # NOTE: Must be done before sampling!
  length_expectation <- create_length_expectation(data, name, indentation = indentation)
  sublengths_expectation <- create_sum_sub_lengths_expectation(data, name, indentation = indentation)

  # Whether to sample data
  sample_data <- !is.null(sample_n) && length(data) > sample_n

  # Sample data
  if (isTRUE(sample_data)){
    data <- smpl(data = data,
                 n = sample_n,
                 keep_order = TRUE)
  }

  # Get non-empty and non-NULL element names
  element_names <- get_element_names(data, remove_empty_names = TRUE)

  # In order to establish whether we should test each element separately
  # we first check if the elements are simple and have length 1

  is_simple <-
    checkmate::test_list(
      as.list(data),
      types = c(
        "logical",
        "integer",
        "double",
        "numeric",
        "complex",
        "atomic",
        "function",
        "null"
      )
    ) &&
    !any(unlist(lapply(data, length)) > 1)

  # If all elements have names
  # We can test each individually
  if (length(element_names) > 0 &&
      length(data) == length(element_names) &&
      !isTRUE(is_simple)
      ) {

    # Create expect_equal expectations
    value_expectations <- plyr::llply(element_names, function(elem_name) {
      # Get current column
      current_elem <- data[[elem_name]]
      # Left side of expectation
      x <- paste0(name, "[[\"", elem_name, "\"]]")
      if (isTRUE(sample_data)){
        x <- paste0("xpectr::smpl(", x, ", n = ", sample_n, ")")
      }
      # Right side of expectation
      y <- capture.output(dput(current_elem))
      # In case dput spanned multiple lines
      # we collapse them to one string
      y <- collapse_strings(y)

      # sanity check
      if (length(y) > 1) {
        return(NULL)
      }

      # Create expect_equal text
      create_expect_equal(x, y,
                          add_tolerance = is.numeric(current_elem),
                          add_fixed = is.character(current_elem),
                          spaces = indentation + 2,
                          tolerance = tolerance)
    })

  } else {
    x <- name
    if (isTRUE(sample_data)){
      x <- paste0("xpectr::smpl(", x, ", n = ", sample_n, ")")
    }
    y <- capture.output(dput(data))
    # In case dput spanned multiple lines
    # we collapse them to one string
    y <- collapse_strings(y)

    value_expectations <- list(
      create_expect_equal(
        x, y,
        add_tolerance = is.numeric(data),
        add_fixed = is.character(data),
        spaces = indentation + 2,
        tolerance = tolerance
      )
    )
  }

  # Note: as list(1,2,3)[-integer()] returns and empty list
  # We must check if there's a NULL first
  null_indices <- get_null_indices(value_expectations)
  if (length(null_indices) > 0) {

    # Warn about skipped elements
    plural_s <- ifelse(length(null_indices) > 1, "s", "")
    warning(paste0(
      "Skipped element", plural_s, " ",
      paste0(null_indices, collapse = ", "),
      "."
    ))

    # Remove NULLS
    value_expectations <- value_expectations[-null_indices]
  }

  # Create name expectation
  if (isTRUE(sample_data)){
    sampled_name <- paste0("xpectr::smpl(", name, ", n = ", sample_n, ")")
  } else sampled_name <- name
  name_expectation <- create_name_expectation(data, sampled_name, indentation = indentation)

  expectations <-
    c(
      create_test_comment(name, section = "intro", indentation = indentation,
                          create_comment = add_comments),
      create_test_comment("values", indentation = indentation,
                          create_comment = add_comments),
      value_expectations,
      create_test_comment("names", indentation = indentation,
                          create_comment = add_comments),
      name_expectation,
      create_test_comment("length", indentation = indentation,
                          create_comment = add_comments),
      length_expectation,
      create_test_comment("sum of element lengths", indentation = indentation,
                          create_comment = add_comments),
      sublengths_expectation,
      create_test_comment(name, section = "outro", indentation = indentation,
                          create_comment = add_comments)
    )

  expectations
}


#   __________________ #< a7adafd5b429db7b53bf49e26d1a7568 ># __________________
#   Create expectations side effects                                        ####


create_expectations_side_effect <- function(side_effects, name = NULL,
                                            indentation = 0, strip = TRUE,
                                            add_comments = TRUE) {


##  .................. #< cdf3aa2ed9d644f88940281a6a5538ac ># ..................
##  Assert arguments                                                        ####


  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_list(
    x = side_effects, all.missing = FALSE,
    len = 4, add = assert_collection
  )
  checkmate::assert_names(
    x = names(side_effects),
    identical.to = c(
      "error", "warnings",
      "messages", "has_side_effects"
    ),
    type = "named"
  )
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    add_comments = add_comments
  )
  checkmate::assert_flag(
    x = strip, add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)


##  .................. #< fb7ea2de323677062692b4fd0bb73e14 ># ..................
##  Create expectations                                                     ####


  if (is.null(name)) { # TODO Not sure this would work (not used currently)
    name <- deparse(substitute(side_effects))
  }

  expectations <- list()

  if (!is.null(side_effects$error)) {
    expectations <- c(expectations, list(
      create_expect_side_effect(
        name, side_effects$error,
        side_effect_type = "error",
        spaces = 2 + indentation,
        strip = strip
      )
    ))
  } else {
    if (!is.null(side_effects$warnings)) {
      expectations <- c(
        expectations,
        plyr::llply(side_effects$warnings, function(w) {
          create_expect_side_effect(
            name, w,
            side_effect_type = "warning",
            spaces = 2 + indentation,
            strip = strip
          )
        })
      )
    }
    if (!is.null(side_effects$messages)) {
      expectations <- c(
        expectations,
        plyr::llply(side_effects$messages, function(m) {
          create_expect_side_effect(
            name, m,
            side_effect_type = "message",
            spaces = 2 + indentation,
            strip = strip
          )
        })
      )
    }
  }

  # TODO add expectation of side effect counts (for warnings and messages at least)
  expectations <- c(
    create_test_comment(name, section = "intro", indentation = indentation,
                        create_comment = add_comments),
    create_test_comment("side effects", indentation = indentation,
                        create_comment = add_comments),
    expectations,
    create_test_comment(name, section = "outro", indentation = indentation,
                        create_comment = add_comments)
  )
}


#   __________________ #< b3caa9bbc4f32ccc4aa583dfb8bc7a47 ># __________________
#   Create expect equal                                                     ####


create_expect_equal <- function(x, y,
                                add_tolerance = FALSE,
                                add_fixed = FALSE,
                                spaces = 2,
                                tolerance = "1e-4") {

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

  y <- escape_metacharacters(y)
  y <- split_to_paste0(y, spaces = spaces)

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


#   __________________ #< ff3057d67f7da6a3155f5034c941f142 ># __________________
#   Create comments                                                         ####

create_test_comment <- function(what, section = "test",
                                indentation = 0,
                                create_comment = TRUE){
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = what, add = assert_collection)
  checkmate::assert_count(x = indentation, add = assert_collection)
  checkmate::assert_choice(x = section,
                           choices = c("intro","outro","test"),
                           add = assert_collection)
  checkmate::assert_flag(x = create_comment, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (!isTRUE(create_comment)){
    return(NULL)
  }

  if (indentation > 40){
    warning("indentation > 40 characters is ignored.")
    indentation <- 40
  }

  # Shorten too long calls from intro and outro comments
  if (nchar(what) > 49 - indentation){
    what <- paste0(substring(what, 1, 46 - indentation), "...")
  }

  # Create "   ####" string
  multihashtags <- paste0(create_space_string(54 - indentation - nchar(what)),
                          "####")

  # We only quote in the intro and outro
  quote_string <- ifelse(section == "test", "", "'")

  if (section == "outro"){
    comment <- paste0("## Finished testing ",
                      quote_string, what, quote_string,
                      multihashtags)
  } else {
    comment <- paste0("# Testing ", quote_string, what, quote_string)
    if (section == "intro"){
      comment <- paste0("#", comment,
                        create_space_string(9),
                        multihashtags,
                        "\n",create_space_string(indentation),
                        "## Initially generated by xpectr")
    }
  }

  comment
}



#   __________________ #< 3feae1f2a76409360ca8e7fa286cbdf9 ># __________________
#   Common arg checks                                                       ####


# Adds common checks to assert collection
add_create_exps_checks <- function(collection,
                                   name,
                                   indentation = 0,
                                   tolerance = "1e-4",
                                   sample_n = 30,
                                   add_comments = TRUE) {


  checkmate::assert_string(
    x = name, min.chars = 1, null.ok = TRUE,
    add = collection
  )
  checkmate::assert_string(x = tolerance, add = collection)
  checkmate::assert_flag(x = add_comments, add = collection)
  checkmate::assert_count(x = indentation, add = collection)

}
