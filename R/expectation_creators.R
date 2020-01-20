

#   __________________ #< bff1ce9ea40e521479e8d8cf57f1bc31 ># __________________
#   Expectation creator wrappers                                            ####

# Creates expect_equal
create_equality_expectation <- function(data,
                                        name,
                                        prefix,
                                        suffix,
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

  # Create test
  create_expect_equal(
    x = x,
    y = y,
    add_tolerance = add_tolerance,
    add_fixed = add_fixed,
    spaces = indentation + 2
  )
}

# returns: expect_equal(names(name), c("a","b"))
create_name_expectation <- function(data, name, indentation = 0) {
  create_equality_expectation(
    data = data,
    name = name,
    prefix = "names(",
    suffix = ")",
    add_fixed = TRUE,
    indentation = indentation
  )
}

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

create_length_expectation <- function(data, name, indentation = 0) {
  create_equality_expectation(
    data = data,
    name = name,
    prefix = "length(",
    suffix = ")",
    indentation = indentation
  )
}

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

create_sum_sub_lengths_expectation <- function(data, name, indentation = 0) {
  create_equality_expectation(
    data = data,
    name = name,
    prefix = "sum(unlist(lapply(",
    suffix = ", length)))",
    indentation = indentation
  )
}
