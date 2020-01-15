

#   __________________ #< 2451d1703a5d7b006fa4e72e2dcc59ed ># __________________
#   Create Expectations Data Frame                                          ####


create_expectations_data_frame <- function(data, name = NULL, indentation = 0,
                                           tolerance = "1e-4") {


##  .................. #< 2271fda988ae80e314ffc80ad1364070 ># ..................
##  Assert arguments                                                        ####


  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, add = assert_collection)
  checkmate::assert_string(
    x = name, min.chars = 1, null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_string(x = tolerance, add = assert_collection)
  checkmate::reportAssertions(assert_collection)


##  .................. #< 2da3e73be55460e78db79bd7977467c9 ># ..................
##  Create expectation                                                      ####


  if (is.null(name)) {
    name <- deparse(substitute(data))
  }

  # Create expect_equal expectations
  expectations <- plyr::llply(colnames(data), function(col_name) {
    # Get current column
    current_col <- data[[col_name]]
    if (is.list(current_col)) {
      return(NULL)
    }

    # Left side of expectation
    x <- paste0(name, "[[\"", col_name, "\"]]")
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

  # Append name expectation
  name_expectation <- create_name_expectation(data, name)
  expectations <- c(expectations, name_expectation)

  null_indices <- get_null_indices(expectations)
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
    expectations <- expectations[-null_indices]
  }

  expectations
}


#   __________________ #< 57651c0852811eeaef463b8c09390020 ># __________________
#   Create expectations vector                                              ####


# Only split into multiple tests when all elements are named
create_expectations_vector <- function(data, name = NULL, indentation = 0,
                                       tolerance = "1e-4") {


##  .................. #< 00dc0af83dcb4c3bb7b5e04a48b8bfbb ># ..................
##  Assert arguments                                                        ####


  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_vector(x = data, add = assert_collection)
  checkmate::assert_string(
    x = name, min.chars = 1, null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_string(x = tolerance, add = assert_collection)
  checkmate::assert_number(x = indentation, lower = 0,
                           add = assert_collection)
  checkmate::reportAssertions(assert_collection)


##  .................. #< fe958b30a0397775f7e311bcff15a411 ># ..................
##  Create expectations                                                     ####


  if (is.null(name)) {
    name <- deparse(substitute(data))
  }

  # Get non-empty and non-NULL element names
  element_names <- get_element_names(data, remove_empty_names = TRUE)

  # If all elements have names
  # We can test each individually
  if (length(element_names) > 0 &&
    length(data) == length(element_names)) {

    # Create expect_equal expectations
    expectations <- plyr::llply(element_names, function(elem_name) {
      # Get current column
      current_elem <- data[[elem_name]]
      # Left side of expectation
      x <- paste0(name, "[[\"", elem_name, "\"]]")
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

    # Append name expectation
    name_expectation <- create_name_expectation(data, name)
    expectations <- c(expectations, name_expectation)
  } else {
    x <- name
    y <- capture.output(dput(data))
    # In case dput spanned multiple lines
    # we collapse them to one string
    y <- collapse_strings(y)

    expectations <- list(
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
  null_indices <- get_null_indices(expectations)
  if (length(null_indices) > 0) {

    # Warn about skipped elements
    plural_s <- ifelse(length(null_indices) > 1, "s", "")
    warning(paste0(
      "Skipped element", plural_s, " ",
      paste0(null_indices, collapse = ", "),
      "."
    ))

    # Remove NULLS
    expectations <- expectations[-null_indices]
  }

  expectations
}


#   __________________ #< a7adafd5b429db7b53bf49e26d1a7568 ># __________________
#   Create expectations side effects                                        ####


create_expectations_side_effect <- function(side_effects, name = NULL, indentation = 0, strip = TRUE) {


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
  checkmate::assert_string(
    x = name, min.chars = 1, null.ok = TRUE,
    add = assert_collection
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

  expectations
}


#   __________________ #< bccbc6fd9c7ff1b37cd1bcd884f72b0d ># __________________
#   Utils                                                                   ####


# returns: expect_equal(names(name), c("a","b"))
create_name_expectation <- function(data, name) {
  x <- paste("names(", name, ")")
  y <- capture.output(dput(names(data)))
  create_expect_equal(
    x = x,
    y = y,
    add_tolerance = FALSE,
    add_fixed = TRUE
  )
}

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
