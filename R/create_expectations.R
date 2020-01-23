

#   __________________ #< 2451d1703a5d7b006fa4e72e2dcc59ed ># __________________
#   Create expectations data frame                                          ####


create_expectations_data_frame <- function(data, name = NULL, indentation = 0,
                                           sample_n = 30,
                                           tolerance = "1e-4",
                                           round_to_tolerance = TRUE,
                                           add_wrapper_comments = TRUE,
                                           add_test_comments = TRUE,
                                           evaluate_once = FALSE) {


##  .................. #< 2271fda988ae80e314ffc80ad1364070 ># ..................
##  Assert arguments                                                        ####


  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, add = assert_collection)
  checkmate::assert_flag(x = round_to_tolerance, add = assert_collection)
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    tolerance = tolerance,
    sample_n = sample_n,
    add_wrapper_comments = add_wrapper_comments,
    add_test_comments = add_test_comments,
    evaluate_once = evaluate_once
  )
  checkmate::reportAssertions(assert_collection)


##  .................. #< 2da3e73be55460e78db79bd7977467c9 ># ..................
##  Create expectation                                                      ####


  if (is.null(name)) {
    name <- trimws(deparse(substitute(data)))
  }

  # Make a copy
  # Used when assigning the call's output to var
  call_name <- name
  if (isTRUE(evaluate_once)){
    name <- create_output_var_name()
  }
  # Create assignment string
  assign_string <- create_assignment_strings(
    call_name = call_name, new_name = name,
    evaluate_once = evaluate_once)

  # Find digits for rounding
  if (isTRUE(round_to_tolerance)){
    numeric_tolerance <- as.numeric(tolerance)
    digits <- nchar(1/numeric_tolerance) # tolerance + 1
  }

  # Create expectations
  # NOTE: Some must come before sampling!
  class_expectation <- create_class_expectation(name = name,
                                                data = data,
                                                indentation = indentation)
  names_expectation <- create_names_expectation(data = data, name = name,
                                                indentation = indentation)
  dim_expectation <- create_dim_expectation(data = data, name = name,
                                            indentation = indentation)
  column_types_expectation <- create_element_types_expectation(data = data,
                                                               name = name,
                                                               sample_n = sample_n,
                                                               indentation = indentation)
  column_classes_expectation <- create_element_classes_expectation(data = data,
                                                                   name = name,
                                                                   sample_n = sample_n,
                                                                   indentation = indentation)
  group_key_names_expectation <- create_group_key_names_expectation(data = data,
                                                                    name = name,
                                                                    indentation = indentation)

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

    if (is.numeric(current_col) && isTRUE(round_to_tolerance)){
      current_col <- round(current_col, digits = digits)
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
      create_test_comment(call_name, section = "intro",
                          indentation = indentation,
                          create_comment = add_wrapper_comments),
      assign_string,
      create_test_comment("class", indentation = indentation,
                          create_comment = add_test_comments),
      class_expectation,
      create_test_comment("column values", indentation = indentation,
                          create_comment = add_test_comments),
      column_expectations,
      create_test_comment("column names", indentation = indentation,
                          create_comment = add_test_comments),
      names_expectation,
      create_test_comment("column classes", indentation = indentation,
                          create_comment = add_test_comments),
      column_classes_expectation,
      create_test_comment("column types", indentation = indentation,
                          create_comment = add_test_comments),
      column_types_expectation,
      create_test_comment("dimensions", indentation = indentation,
                          create_comment = add_test_comments),
      dim_expectation,
      create_test_comment("group keys", indentation = indentation,
                          create_comment = add_test_comments),
      group_key_names_expectation,
      create_test_comment(call_name, section = "outro", indentation = indentation,
                          create_comment = add_wrapper_comments)
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
                                       add_wrapper_comments = TRUE,
                                       add_test_comments = TRUE,
                                       evaluate_once = FALSE) {


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
    add_wrapper_comments = add_wrapper_comments,
    add_test_comments = add_test_comments,
    evaluate_once = evaluate_once
  )
  checkmate::reportAssertions(assert_collection)

##  .................. #< fe958b30a0397775f7e311bcff15a411 ># ..................
##  Create expectations                                                     ####


  if (is.null(name)) {
    name <- trimws(deparse(substitute(data)))
  }

  # Make a copy
  # Used when assigning the call's output to var
  call_name <- name
  if (isTRUE(evaluate_once)){
    name <- create_output_var_name()
  }
  # Create assignment string
  assign_string <- create_assignment_strings(
    call_name = call_name, new_name = name,
    evaluate_once = evaluate_once)

  # Create expectations
  # Without sampling
  type_expectation <- create_type_expectation(name = name, type = typeof(data),
                                              indentation = indentation)
  class_expectation <- create_class_expectation(name = name, data = data,
                                                indentation = indentation)
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
    !any(element_lengths(data, length) > 1)

  all_uniquely_named <- checkmate::test_list(x = as.list(data), names = "unique")

  # If all elements have names
  # We can test each individually
  if (isTRUE(all_uniquely_named) &&
      !isTRUE(is_simple)
      ) {

    element_types_expectation <-
      create_element_types_expectation(
        data = data,
        name = name,
        sample_n = sample_n,
        indentation = indentation
      )
    element_classes_expectation <-
      create_element_classes_expectation(
        data = data,
        name = name,
        sample_n = sample_n,
        indentation = indentation
      )

    # Create expect_equal expectations
    value_expectations <- plyr::llply(element_names, function(elem_name) {
      # Get current column
      current_elem <- data[[elem_name]]
      # Left side of expectation
      x <- paste0(name, "[[\"", elem_name, "\"]]")
      if (isTRUE(sample_data)){ # TODO are the elements themselves sampled? is this correct????
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

  # Create names expectation
  if (isTRUE(sample_data)){
    sampled_name <- paste0("xpectr::smpl(", name, ", n = ", sample_n, ")")
  } else sampled_name <- name
  names_expectation <- create_names_expectation(data, sampled_name, indentation = indentation)

  if (exists("element_types_expectation")){
    element_types_classes_expectations <- c(
      create_test_comment("element classes", indentation = indentation,
                          create_comment = add_test_comments),
      element_classes_expectation,
      create_test_comment("element types", indentation = indentation,
                          create_comment = add_test_comments),
      element_types_expectation
    )
  } else {
    element_types_classes_expectations <- NULL
  }

  expectations <-
    c(
      create_test_comment(call_name, section = "intro", indentation = indentation,
                          create_comment = add_wrapper_comments),
      assign_string,
      create_test_comment("class", indentation = indentation,
                          create_comment = add_test_comments),
      class_expectation,
      create_test_comment("type", indentation = indentation,
                          create_comment = add_test_comments),
      type_expectation,
      create_test_comment("values", indentation = indentation,
                          create_comment = add_test_comments),
      value_expectations,
      create_test_comment("names", indentation = indentation,
                          create_comment = add_test_comments),
      names_expectation,
      create_test_comment("length", indentation = indentation,
                          create_comment = add_test_comments),
      length_expectation,
      create_test_comment("sum of element lengths", indentation = indentation,
                          create_comment = add_test_comments),
      sublengths_expectation,
      element_types_classes_expectations,
      create_test_comment(call_name, section = "outro", indentation = indentation,
                          create_comment = add_wrapper_comments)
    )

  expectations
}


#   __________________ #< a7adafd5b429db7b53bf49e26d1a7568 ># __________________
#   Create expectations side effects                                        ####


create_expectations_side_effect <- function(side_effects, name = NULL,
                                            indentation = 0, strip = TRUE,
                                            add_wrapper_comments = TRUE,
                                            add_test_comments = TRUE,
                                            evaluate_once = FALSE) {


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
    add_wrapper_comments = add_wrapper_comments,
    add_test_comments = add_wrapper_comments
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
                        create_comment = add_wrapper_comments),
    create_test_comment("side effects", indentation = indentation,
                        create_comment = add_test_comments),
    expectations,
    create_test_comment(name, section = "outro", indentation = indentation,
                        create_comment = add_wrapper_comments)
  )
}


#   __________________ #< ff3057d67f7da6a3155f5034c941f142 ># __________________
#   Create expectations factor                                              ####

# Only split into multiple tests when all elements are named
# plus some other checks
create_expectations_factor <- function(data, name = NULL,
                                       indentation = 0,
                                       sample_n = 30,
                                       tolerance = "1e-4",
                                       add_wrapper_comments = TRUE,
                                       add_test_comments = TRUE,
                                       evaluate_once = FALSE) {


##  .................. #< c8abec0740b04c7d59fbaa30de451cb9 ># ..................
##  Assert arguments                                                        ####


  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_factor(x = data, add = assert_collection)
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    tolerance = tolerance,
    sample_n = sample_n,
    add_wrapper_comments = add_wrapper_comments,
    add_test_comments = add_test_comments,
    evaluate_once = evaluate_once
  )
  checkmate::reportAssertions(assert_collection)


##  .................. #< 36893b0a8da49f8aca8d237642f5958f ># ..................
##  Create expectations                                                     ####


  if (is.null(name)) {
    name <- trimws(deparse(substitute(data)))
  }

  # Make a copy
  # Used when assigning the call's output to var
  call_name <- name
  if (isTRUE(evaluate_once)){
    name <- create_output_var_name()
  }
  # Create assignment string
  assign_string <- create_assignment_strings(
    call_name = call_name, new_name = name,
    evaluate_once = evaluate_once)

  # Without sampling
  factor_expectation <- create_is_factor_expectation(name = name,
                                                     indentation = indentation)
  length_expectation <- create_length_expectation(data = data, name = name,
                                                  indentation = indentation)
  n_levels_expectation <- create_nlevels_expectation(data = data, name = name,
                                                     indentation = indentation)

  # With sampling
  value_expectations <- create_as_character_expectation(data = data,
                                                        name = name,
                                                        sample_n = sample_n,
                                                        indentation = indentation)
  levels_expectation <- create_levels_expectation(data = data, name = name,
                                                  sample_n = sample_n,
                                                  indentation = indentation)
  names_expectation <- create_names_expectation(data = data,
                                               name = name,
                                               sample_n = sample_n,
                                               indentation = indentation)

  expectations <-
    c(
      create_test_comment(call_name, section = "intro", indentation = indentation,
                          create_comment = add_wrapper_comments),
      assign_string,
      create_test_comment("is factor", indentation = indentation,
                          create_comment = add_test_comments),
      factor_expectation,
      create_test_comment("values", indentation = indentation,
                          create_comment = add_test_comments),
      value_expectations,
      create_test_comment("names", indentation = indentation,
                          create_comment = add_test_comments),
      names_expectation,
      create_test_comment("length", indentation = indentation,
                          create_comment = add_test_comments),
      length_expectation,
      create_test_comment("number of levels", indentation = indentation,
                          create_comment = add_test_comments),
      n_levels_expectation,
      create_test_comment("levels", indentation = indentation,
                          create_comment = add_test_comments),
      levels_expectation,
      create_test_comment(call_name, section = "outro", indentation = indentation,
                          create_comment = add_wrapper_comments)
    )

  expectations
}


#   __________________ #< 3feae1f2a76409360ca8e7fa286cbdf9 ># __________________
#   Common arg checks                                                       ####


# Adds common checks to assert collection
add_create_exps_checks <- function(collection,
                                   name,
                                   indentation = 0,
                                   tolerance = "1e-4",
                                   sample_n = 30,
                                   add_wrapper_comments = TRUE,
                                   add_test_comments = TRUE,
                                   evaluate_once = FALSE) {

  checkmate::assert_string(
    x = name, min.chars = 1, null.ok = TRUE,
    add = collection
  )
  checkmate::assert_string(x = tolerance, add = collection)
  checkmate::assert_count(x = indentation, add = collection)
  checkmate::assert_count(x = sample_n, null.ok = TRUE, add = collection)
  checkmate::assert_flag(x = add_wrapper_comments, add = collection)
  checkmate::assert_flag(x = add_test_comments, add = collection)
  checkmate::assert_flag(x = evaluate_once, add = collection)

}


#   __________________ #< 8a79652e81ccaedb871a49e1f8421c1c ># __________________
#   Create output name                                                      ####

create_output_var_name <- function(prefix = "output_"){
  paste0(prefix, floor(runif(1, 10000, 19999)))
}


#   __________________ #< ab5891096b0ab8586b37f58552cbda23 ># __________________
#   Create assignment strings                                               ####


create_assignment_strings <- function(call_name, new_name, evaluate_once){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_flag(x = evaluate_once, add = assert_collection)
  checkmate::assert_string(x = call_name, add = assert_collection)
  checkmate::assert_string(x = new_name, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (isTRUE(evaluate_once)){
    assign_string <- c(
      "# Assigning output",
      paste0(new_name, " <- ", call_name))
  } else {
    assign_string <- NULL
  }
  assign_string
}


