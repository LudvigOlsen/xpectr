

#   __________________ #< bff1ce9ea40e521479e8d8cf57f1bc31 ># __________________
#   Expectation creator wrappers                                            ####


# returns: expect_equal(names(name), c("a","b"))
create_name_expectation <- function(data, name, indentation = 0) {
  x <- paste("names(", name, ")")
  y <- capture.output(dput(names(data)))
  create_expect_equal(
    x = x,
    y = y,
    add_tolerance = FALSE,
    add_fixed = TRUE,
    spaces = indentation + 2
  )
}

# Does not work for all types of data!
# Use for data frames only for now!
create_dim_expectation <- function(data, name, indentation = 0) {
  x <- paste("dim(", name, ")")
  y <- capture.output(dput(dim(data)))
  create_expect_equal(
    x = x,
    y = y,
    add_tolerance = FALSE,
    add_fixed = FALSE,
    spaces = indentation + 2
  )
}

# Does not work for all types of data!
# Use for data frames only for now!
create_length_expectation <- function(data, name, indentation = 0) {
  x <- paste("length(", name, ")")
  y <- capture.output(dput(length(data)))
  create_expect_equal(
    x = x,
    y = y,
    add_tolerance = FALSE,
    add_fixed = FALSE,
    spaces = indentation + 2
  )
}

create_group_key_names_expectation <- function(data, name, indentation = 0) {
  x <- paste("colnames(dplyr::group_keys(", name, "))")
  y <- capture.output(dput(colnames(dplyr::group_keys(data))))
  create_expect_equal(
    x = x,
    y = y,
    add_tolerance = FALSE,
    add_fixed = TRUE,
    spaces = indentation + 2
  )
}

create_sum_sub_lengths_expectation <- function(data, name, indentation = 0) {
  x <- paste("sum(unlist(lapply(", name, ", length)))")
  y <- capture.output(dput(sum(unlist(lapply(data, length)))))
  create_expect_equal(
    x = x,
    y = y,
    add_tolerance = FALSE,
    add_fixed = FALSE,
    spaces = indentation + 2
  )
}
