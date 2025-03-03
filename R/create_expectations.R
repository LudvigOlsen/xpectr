
# For overview: cmd-alt-o

#   __________________ #< a728fc4a8cf823f5d1f50ada4565aa66 ># __________________
#   Create expectations for NULL                                            ####

create_expectations_null <- function(name = NULL,
                                     indentation = 0,
                                     add_comments = TRUE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    add_comments = add_comments
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Create test
  null_expectation <- create_expect_true(
    x = paste0("is.null(", name, ")"),
    spaces = 2
  )

  # Collect expectations and add comments
  expectations <-
    c(create_test_comment("is NULL", indentation = indentation,
                          create_comment = add_comments),
      null_expectation
    )

  expectations
}



#   ____________________________________________________________________________
#   Create expectations for function                                        ####

create_expectations_function <- function(data, name = NULL, indentation = 0,
                                         sample_n = 30,
                                         evaluate_once = FALSE,
                                         add_comments = TRUE,
                                         test_id = 10000) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_function(x = data, add = assert_collection)
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    sample_n = sample_n,
    evaluate_once = evaluate_once,
    add_comments = add_comments,
    test_id = test_id
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Make a copy
  # Used when assigning the call's output to var
  call_name <- name
  if (isTRUE(evaluate_once)){
    name <- create_output_var_name(id = test_id)
  }
  # Create assignment string
  assign_string <- create_assignment_strings(
    call_name = call_name, new_name = name,
    evaluate_once = evaluate_once)

  # Create test
  is_fn_expectation <- create_expect_true(
    x = paste0("is.function(", name, ")"),
    spaces = 2
  )

  # Test the formals (arguments' names and default values)
  formals_expectation <- create_fn_formals_expectation(
    data = data,
    name = name,
    sample_n = sample_n,
    indentation = indentation
  )

  # Test the function string
  definition_expectation <- create_deparse_expectation(
    data = data,
    name = name,
    sample_n = sample_n,
    indentation = indentation
  )

  # Collect expectations and add comments
  expectations <-
    c(assign_string,
      create_test_comment("is function", indentation = indentation,
                          create_comment = add_comments),
      is_fn_expectation,
      create_test_comment("argument names and default values",
                          indentation = indentation,
                          create_comment = add_comments),
      formals_expectation,
      create_test_comment("function definition", indentation = indentation,
                          create_comment = add_comments),
      definition_expectation
    )

  expectations
}




#   __________________ #< 2451d1703a5d7b006fa4e72e2dcc59ed ># __________________
#   Create expectations data frame                                          ####


create_expectations_data_frame <- function(data, name = NULL, indentation = 0,
                                           sample_n = 30,
                                           tolerance = "1e-4",
                                           round_to_tolerance = TRUE,
                                           add_comments = TRUE,
                                           evaluate_once = FALSE,
                                           test_id = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, add = assert_collection)
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    tolerance = tolerance,
    sample_n = sample_n,
    add_comments = add_comments,
    evaluate_once = evaluate_once,
    round_to_tolerance = round_to_tolerance,
    test_id = test_id
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (is.null(name)) {
    name <- trimws(deparse(substitute(data)))
  }

  # Make a copy
  # Used when assigning the call's output to var
  call_name <- name
  if (isTRUE(evaluate_once)){
    name <- create_output_var_name(id = test_id)
  }
  # Create assignment string
  assign_string <- create_assignment_strings(
    call_name = call_name, new_name = name,
    evaluate_once = evaluate_once)

  # Find digits for rounding
  if (isTRUE(round_to_tolerance)){
    numeric_tolerance <- as.numeric(tolerance)
    digits <- ndigits(1/numeric_tolerance) # tolerance + 1
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
    create_expect_equal(
      x, y,
      add_tolerance = is.numeric(current_col),
      add_fixed = is.character(current_col),
      spaces = 2,
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
    c(assign_string,
      create_test_comment("class", indentation = indentation,
                          create_comment = add_comments),
      class_expectation,
      create_test_comment("column values", indentation = indentation,
                          create_comment = add_comments),
      column_expectations,
      create_test_comment("column names", indentation = indentation,
                          create_comment = add_comments),
      names_expectation,
      create_test_comment("column classes", indentation = indentation,
                          create_comment = add_comments),
      column_classes_expectation,
      create_test_comment("column types", indentation = indentation,
                          create_comment = add_comments),
      column_types_expectation,
      create_test_comment("dimensions", indentation = indentation,
                          create_comment = add_comments),
      dim_expectation,
      create_test_comment("group keys", indentation = indentation,
                          create_comment = add_comments),
      group_key_names_expectation
    )

  expectations
}


#   __________________ #< 64bfa943afd1c6099390df317edf6e8a ># __________________
#   Create expectations matrix                                              ####


create_expectations_matrix <- function(data, name = NULL, indentation = 0,
                                       sample_n = 30,
                                       tolerance = "1e-4",
                                       round_to_tolerance = TRUE,
                                       add_comments = TRUE,
                                       evaluate_once = FALSE,
                                       test_id = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_matrix(x = data, add = assert_collection)
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    tolerance = tolerance,
    sample_n = sample_n,
    add_comments = add_comments,
    evaluate_once = evaluate_once,
    round_to_tolerance = round_to_tolerance,
    test_id = test_id
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (is.null(name)) {
    name <- trimws(deparse(substitute(data)))
  }

  # Make a copy
  # Used when assigning the call's output to var
  call_name <- name
  if (isTRUE(evaluate_once)){
    name <- create_output_var_name(id = test_id)
  }
  # Create assignment string
  assign_string <- create_assignment_strings(
    call_name = call_name, new_name = name,
    evaluate_once = evaluate_once)

  # Create expectations
  # NOTE: Some must come before sampling!

  # Add [[1]] to class expectation as matrices also inherit from arrays
  # in the devel version of R!
  class_expectation <- create_class_expectation(name = name,
                                                data = data,
                                                suffix = ")[[1]]",
                                                indentation = indentation)
  type_expectation <- create_type_expectation(name = name,
                                              type = typeof(data),
                                              indentation = indentation)
  colnames_expectation <- create_colnames_expectation(data = data, name = name,
                                                   indentation = indentation)
  rownames_expectation <- create_rownames_expectation(data = data, name = name,
                                                      indentation = indentation)
  dim_expectation <- create_dim_expectation(data = data, name = name,
                                            indentation = indentation)
  symmetry_expectation <- tryCatch(
    # Doesn't work for table objects
    create_is_symmetric_expectation(data = data, name = name,
                                    indentation = indentation),
    error = function(e){return(NULL)})

  # Find digits for rounding
  if (isTRUE(round_to_tolerance) && is.numeric(data)){
    numeric_tolerance <- as.numeric(tolerance)
    digits <- ndigits(1/numeric_tolerance) # tolerance + 1
    data <- round(data, digits = digits)
  }

  # Whether to sample data
  sample_data <- !is.null(sample_n) && max(dim(data)) > sample_n
  row_indices <- seq_len(nrow(data))
  col_indices <- seq_len(ncol(data))

  # Sample data (TODO Specify axis!!)
  if (isTRUE(sample_data)){
    if (min(dim(data)) > sample_n){
      slice_axis <- "column"
    } else {
      slice_axis <- ifelse(ncol(data) > nrow(data), "row", "column")
    }
    row_indices <- smpl(data = row_indices, n = sample_n,
                        keep_order = TRUE)
    col_indices <- smpl(data = col_indices, n = sample_n,
                        keep_order = TRUE)
  } else {
    # Whether to test columns or rows
    slice_axis <- ifelse(ncol(data) > nrow(data), "row", "column")
  }

  if (slice_axis == "row"){
    slice_indices <- row_indices
    data <- data[, col_indices]
  } else if (slice_axis == "column"){
    slice_indices <- col_indices
    data <- data[row_indices, ]
  }

  # Create expect_equal expectations
  slice_expectations <- plyr::llply(slice_indices, function(i) {

    # Get current slice
    if (slice_axis == "row"){
      current_slice <- data[i, ]
      x <- paste0(name, "[", i, ", ]")
    } else if (slice_axis == "column"){
      current_slice <- data[, i]
      x <- paste0(name, "[, ", i, "]")
    } else {
      stop("'slice_axis' must be 'row' or 'column'.")
    }

    # Left side of expectation

    if (isTRUE(sample_data)){
      x <- paste0("xpectr::smpl(", x, ", n = ", sample_n, ")")
    }

    # Right side of expectation
    y <- capture.output(dput(current_slice))
    # In case dput spanned multiple lines
    # we collapse them to one string
    y <- collapse_strings(y)

    # Sanity check
    if (length(y) != 1) {
      return(NULL)
    }

    # Create expect_equal text
    create_expect_equal(x, y,
                        add_tolerance = is.numeric(current_slice),
                        add_fixed = is.character(current_slice),
                        spaces = 2,
                        tolerance = tolerance)
  })

  # Collect expectations and add comments
  expectations <-
    c(assign_string,
      create_test_comment("class", indentation = indentation,
                          create_comment = add_comments),
      class_expectation,
      create_test_comment("type", indentation = indentation,
                          create_comment = add_comments),
      type_expectation,
      create_test_comment(paste0(slice_axis, " values"), indentation = indentation,
                          create_comment = add_comments),
      slice_expectations,
      create_test_comment("column names", indentation = indentation,
                          create_comment = add_comments),
      colnames_expectation,
      create_test_comment("row names", indentation = indentation,
                          create_comment = add_comments),
      rownames_expectation,
      create_test_comment("dimensions", indentation = indentation,
                          create_comment = add_comments),
      dim_expectation,
      create_test_comment("symmetry", indentation = indentation,
                          create_comment = add_comments && !is.null(symmetry_expectation)),
      symmetry_expectation
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
                                       round_to_tolerance = TRUE,
                                       add_comments = TRUE,
                                       evaluate_once = FALSE,
                                       test_id = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_vector(x = data, add = assert_collection)
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    tolerance = tolerance,
    sample_n = sample_n,
    add_comments = add_comments,
    evaluate_once = evaluate_once,
    round_to_tolerance = round_to_tolerance,
    test_id = test_id
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (is.null(name)) {
    name <- trimws(deparse(substitute(data)))
  }

  # Make a copy
  # Used when assigning the call's output to var
  call_name <- name
  if (isTRUE(evaluate_once)){
    name <- create_output_var_name(id = test_id)
  }
  # Create assignment string
  assign_string <- create_assignment_strings(
    call_name = call_name, new_name = name,
    evaluate_once = evaluate_once)

  # Find digits for rounding
  if (isTRUE(round_to_tolerance)){
    numeric_tolerance <- as.numeric(tolerance)
    digits <- ndigits(1/numeric_tolerance) # tolerance + 1
  }

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
    !any(element_lengths(data) > 1)

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

      # Determine whether to sample element
      if (is.data.frame(current_elem))
        sample_slice <- !is.null(sample_n) && nrow(current_elem) > sample_n
      else if (is.vector(current_elem))
        sample_slice <- !is.null(sample_n) && length(current_elem) > sample_n
      else
        sample_slice <- FALSE

      # Sample element
      if (isTRUE(sample_slice)){
        current_elem <- smpl(current_elem, n = sample_n, keep_order = TRUE)
        x <- paste0("xpectr::smpl(", x, ", n = ", sample_n, ")")
      }

      # Round to tolerance
      if (is.numeric(current_elem) && isTRUE(round_to_tolerance)){
        current_elem <- round(current_elem, digits = digits)
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
                          spaces = 2,
                          tolerance = tolerance)
    })

  } else {
    x <- name
    if (isTRUE(sample_data)){
      x <- paste0("xpectr::smpl(", x, ", n = ", sample_n, ")")
    }

    # Round to tolerance
    if (is.numeric(data) && isTRUE(round_to_tolerance)){
      data <- round(data, digits = digits)
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
        spaces = 2,
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
                          create_comment = add_comments),
      element_classes_expectation,
      create_test_comment("element types", indentation = indentation,
                          create_comment = add_comments),
      element_types_expectation
    )
  } else {
    element_types_classes_expectations <- NULL
  }

  expectations <-
    c(assign_string,
      create_test_comment("class", indentation = indentation,
                            create_comment = add_comments),
      class_expectation,
      create_test_comment("type", indentation = indentation,
                          create_comment = add_comments),
      type_expectation,
      create_test_comment("values", indentation = indentation,
                          create_comment = add_comments),
      value_expectations,
      create_test_comment("names", indentation = indentation,
                          create_comment = add_comments),
      names_expectation,
      create_test_comment("length", indentation = indentation,
                          create_comment = add_comments),
      length_expectation,
      create_test_comment("sum of element lengths", indentation = indentation,
                          create_comment = add_comments),
      sublengths_expectation,
      element_types_classes_expectations
    )

  expectations
}


#   __________________ #< a7adafd5b429db7b53bf49e26d1a7568 ># __________________
#   Create expectations side effects                                        ####


create_expectations_side_effect <- function(side_effects, name = NULL,
                                            copy_env = FALSE,
                                            indentation = 0, strip = TRUE,
                                            add_comments = TRUE, test_id = 10000) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_list(
    x = side_effects, all.missing = FALSE,
    len = 5, add = assert_collection
  )
  checkmate::assert_names(
    x = names(side_effects),
    identical.to = c(
      "error", "error_class", "warnings",
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
  checkmate::assert_flag(
    x = copy_env, add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (is.null(name)) { # TODO Not sure this would work (not used currently)
    name <- deparse(substitute(side_effects))
  }

  call_name <- name
  name <- create_output_var_name("side_effects_", test_id)

  copy_env_string <- ifelse(isTRUE(copy_env), ", copy_env = TRUE", "")

  # Create assignment string
  assign_string <- create_assignment_strings(
    call_name = paste0("xpectr::capture_side_effects(", call_name, copy_env_string, ", reset_seed = TRUE)"),
    new_name = name,
    evaluate_once = TRUE,
    comment = "# Assigning side effects")

  expectations <- list(assign_string)

  if (!is.null(side_effects$error)) {

    err_expectation <- create_equality_expectation(
      data = side_effects, name = name,
      prefix = "",
      suffix = "[['error']]",
      add_strip = strip,
      add_fixed = TRUE,
      indentation = indentation
    )

    err_class_expectation <- create_equality_expectation(
      data = side_effects, name = name,
      prefix = "",
      suffix = "[['error_class']]",
      add_strip = strip,
      add_fixed = TRUE,
      indentation = indentation
    )

    expectations <- c(
      expectations,
      err_expectation,
      err_class_expectation
    )

  } else {

    msg_expectation <- create_equality_expectation(
      data = side_effects, name = name,
      prefix = "",
      suffix = "[['messages']]",
      add_strip = strip,
      add_fixed = TRUE, indentation = indentation
    )

    warns_expectation <- create_equality_expectation(
      data = side_effects, name = name,
      prefix = "",
      suffix = "[['warnings']]",
      add_strip = strip,
      add_fixed = TRUE, indentation = indentation
    )

    expectations <- c(
      expectations,
      warns_expectation,
      msg_expectation)
  }

  # TODO add expectation of side effect counts (for warnings and messages at least)
  expectations <- c(
    create_test_comment("side effects", indentation = indentation,
                        create_comment = add_comments),
    expectations
  ) %>% unlist() %>%
    as.list()
}


#   __________________ #< ff3057d67f7da6a3155f5034c941f142 ># __________________
#   Create expectations factor                                              ####

# Only split into multiple tests when all elements are named
# plus some other checks
create_expectations_factor <- function(data, name = NULL,
                                       indentation = 0,
                                       sample_n = 30,
                                       tolerance = "1e-4",
                                       add_comments = TRUE,
                                       evaluate_once = FALSE,
                                       test_id = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_factor(x = data, add = assert_collection)
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    tolerance = tolerance,
    sample_n = sample_n,
    add_comments = add_comments,
    evaluate_once = evaluate_once,
    test_id = test_id
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (is.null(name)) {
    name <- trimws(deparse(substitute(data)))
  }

  # Make a copy
  # Used when assigning the call's output to var
  call_name <- name
  if (isTRUE(evaluate_once)){
    name <- create_output_var_name(id = test_id)
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
    c(assign_string,
      create_test_comment("is factor", indentation = indentation,
                          create_comment = add_comments),
      factor_expectation,
      create_test_comment("values", indentation = indentation,
                          create_comment = add_comments),
      value_expectations,
      create_test_comment("names", indentation = indentation,
                          create_comment = add_comments),
      names_expectation,
      create_test_comment("length", indentation = indentation,
                          create_comment = add_comments),
      length_expectation,
      create_test_comment("number of levels", indentation = indentation,
                          create_comment = add_comments),
      n_levels_expectation,
      create_test_comment("levels", indentation = indentation,
                          create_comment = add_comments),
      levels_expectation
    )

  expectations
}


#   __________________ #< b696d526380be550b639f9efdbb1ac91 ># __________________
#   Create expectations formula                                             ####


create_expectations_formula <- function(data, name = NULL, indentation = 0,
                                        add_comments = TRUE,
                                        evaluate_once = FALSE,
                                        test_id = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_formula(x = data, add = assert_collection)
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    add_comments = add_comments,
    evaluate_once = evaluate_once,
    test_id = test_id
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Make a copy
  # Used when assigning the call's output to var
  call_name <- name
  if (isTRUE(evaluate_once)){
    name <- create_output_var_name(id = test_id)
  }
  # Create assignment string
  assign_string <- create_assignment_strings(
    call_name = call_name, new_name = name,
    evaluate_once = evaluate_once)

  # Create is_formula test
  is_formula_expectation <- create_expect_true(
    x = paste0("rlang::is_formula(", name, ")"),
    spaces = 2
  )

  # Test the formula
  formula_expectation <- create_formula_expectation(
    data = data,
    name = name,
    indentation = indentation
  )

  # Collect expectations and add comments
  expectations <-
    c(assign_string,
      create_test_comment("is formula", indentation = indentation,
                          create_comment = add_comments),
      is_formula_expectation,
      create_test_comment("formula", indentation = indentation,
                          create_comment = add_comments),
      formula_expectation
    )

  expectations
}



#   __________________ #< 1a04985673e4647d6eb888d8bf7e76c1 ># __________________
#   Create expectation fallback                                             ####

create_expectations_fallback <- function(data,
                                         name = NULL,
                                         indentation = 0,
                                         add_comments = TRUE,
                                         evaluate_once = FALSE,
                                         test_id = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  add_create_exps_checks(
    collection = assert_collection,
    name = name,
    indentation = indentation,
    add_comments = add_comments,
    evaluate_once = evaluate_once,
    test_id = test_id
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Make a copy
  # Used when assigning the call's output to var
  call_name <- name
  if (isTRUE(evaluate_once)){
    name <- create_output_var_name(id = test_id)
  }

  # Create assignment string
  assign_string <- create_assignment_strings(
    call_name = call_name, new_name = name,
    evaluate_once = evaluate_once)

  class_expectation <- create_class_expectation(name = name,
                                                data = data,
                                                indentation = indentation)
  type_expectation <- create_type_expectation(name = name,
                                               type = typeof(data),
                                               indentation = indentation)
  names_expectation <- create_names_expectation(data = data, name = name,
                                                indentation = indentation)

  dput_expectation <- create_equality_expectation(data = data,
                                                  name = name,
                                                  prefix = "",
                                                  suffix = "",
                                                  indentation = indentation)

  # Collect expectations and add comments
  expectations <-
    c(create_test_comment("Unsupported class: using fallback tests",
                          indentation = indentation,
                          section = "manual",
                          create_comment = add_comments),
      assign_string,
      create_test_comment("class", indentation = indentation,
                          create_comment = add_comments),
      class_expectation,
      create_test_comment("type", indentation = indentation,
                          create_comment = add_comments),
      type_expectation,
      create_test_comment("names", indentation = indentation,
                          create_comment = add_comments),
      names_expectation,
      create_test_comment("dput() content", indentation = indentation,
                          create_comment = add_comments),
      dput_expectation
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
                                   round_to_tolerance = TRUE,
                                   sample_n = 30,
                                   add_wrapper_comments = TRUE,
                                   add_test_comments = TRUE,
                                   add_comments = TRUE,
                                   evaluate_once = FALSE,
                                   test_id = NULL) {

  checkmate::assert_string(
    x = name, min.chars = 1, null.ok = TRUE,
    add = collection
  )
  checkmate::assert_string(x = tolerance, add = collection)
  checkmate::assert_count(x = indentation, add = collection)
  checkmate::assert_count(x = sample_n, null.ok = TRUE, add = collection)
  checkmate::assert_count(x = test_id, null.ok = TRUE, positive = TRUE, add = collection)
  checkmate::assert_flag(x = add_wrapper_comments, add = collection)
  checkmate::assert_flag(x = add_test_comments, add = collection)
  checkmate::assert_flag(x = add_comments, add = collection)
  checkmate::assert_flag(x = evaluate_once, add = collection)
  checkmate::assert_flag(x = round_to_tolerance, add = collection)

}


#   __________________ #< 8a79652e81ccaedb871a49e1f8421c1c ># __________________
#   Create output name                                                      ####

create_output_var_name <- function(prefix = "output_", id = NULL){
  if (is.null(id))
    id <- floor(runif(1, 10000, 19999))
  paste0(prefix, id)
}


#   __________________ #< ab5891096b0ab8586b37f58552cbda23 ># __________________
#   Create assignment strings                                               ####


create_assignment_strings <- function(call_name, new_name, evaluate_once,
                                      comment = "# Assigning output"){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_flag(x = evaluate_once, add = assert_collection)
  checkmate::assert_string(x = call_name, add = assert_collection)
  checkmate::assert_string(x = new_name, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (isTRUE(evaluate_once)){
    assign_string <- c(
      comment,
      paste0(new_name, " <- ", call_name))
  } else {
    assign_string <- NULL
  }
  assign_string
}


