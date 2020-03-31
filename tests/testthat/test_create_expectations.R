library(xpectr)
context("create_expectations")

test_that("expectation are created properly with create_expectations_data_frame()", {
  df1 <- tibble::tibble("a" = c(1, 2, 3, 4), "b" = c("a", "f", "g", "s"))
  df1$c <- list(list(1, 2, 3), list(2, 3, 4, 5), list(2, 3, 7, 21, 2), list(2))
  df1[["d"]] <- list(a = list(2, 4), b = list(3), c = list(1), d = list(2, 3))
  # Add factor
  df1$f <- factor(df1$a)

  # We're not getting it from parent environment
  # so we supply current environment
  current_envir <- sys.frame(which = sys.nframe())

  expect_warning(expts <- create_expectations_data_frame(df1),
    "Skipped columns c, d.",
    fixed = TRUE
  )
  expts_expected <- list(
    "# Testing class",
    "expect_equal(\n  class(df1),\n  c(\"tbl_df\", \"tbl\", \"data.frame\"),\n  fixed = TRUE)",
    "# Testing column values",
    "expect_equal(\n  df1[[\"a\"]],\n  c(1, 2, 3, 4),\n  tolerance = 1e-4)",
    "expect_equal(\n  df1[[\"b\"]],\n  c(\"a\", \"f\", \"g\", \"s\"),\n  fixed = TRUE)",
    "expect_equal(\n  df1[[\"f\"]],\n  structure(1:4, .Label = c(\"1\", \"2\", \"3\", \"4\"), class = \"factor\"))",
    "# Testing column names",
    "expect_equal(\n  names(df1),\n  c(\"a\", \"b\", \"c\", \"d\", \"f\"),\n  fixed = TRUE)",
    "# Testing column classes",
    "expect_equal(\n  xpectr::element_classes(df1),\n  c(\"numeric\", \"character\", \"list\", \"list\", \"factor\"),\n  fixed = TRUE)",
    "# Testing column types",
    "expect_equal(\n  xpectr::element_types(df1),\n  c(\"double\", \"character\", \"list\", \"list\", \"integer\"),\n  fixed = TRUE)",
    "# Testing dimensions",
    "expect_equal(\n  dim(df1),\n  4:5)",
    "# Testing group keys",
    "expect_equal(\n  colnames(dplyr::group_keys(df1)),\n  character(0),\n  fixed = TRUE)"
  )

  expect_equal(
    expts,
    expts_expected
  )
  eval_expectations(expts_expected, envir = current_envir)
})

test_that("expectation are created properly with create_expectations_vector()", {
  vec_1 <- c(1, 2, 3, 4)
  vec_2 <- c("a" = 10, "b" = 20, "c" = 30)
  vec_3 <- c("a" = c(10, 2, 4), "b" = c(20, 1, 3), "c" = c(2, 3, 30))
  vec_4 <- c("a" = list(10, 2, 4), "b" = list(20, 1, 3), "c" = list(2, 3, 30))
  vec_5 <- list("a" = list(10, 2, 4), "b" = list(20, 1, 3), "c" = list(2, 3, 30))
  vec_6 <- list("a" = list(10, 2, 4), "b" = list(20, 1, 3), list(2, 3, 30))
  vec_7 <- list(5, 6, 7, 8)

  # We're not getting it from parent environment
  # so we supply current environment
  current_envir <- sys.frame(which = sys.nframe())

  # Vec 1

  exp_vec_1 <- list(
    "# Testing class",
    "expect_equal(\n  class(vec_1),\n  \"numeric\",\n  fixed = TRUE)",
    "# Testing type",
    "expect_type(\n  vec_1,\n  type = \"double\")",
    "# Testing values",
    "expect_equal(\n  vec_1,\n  c(1, 2, 3, 4),\n  tolerance = 1e-4)",
    "# Testing names",
    "expect_equal(\n  names(vec_1),\n  NULL,\n  fixed = TRUE)",
    "# Testing length",
    "expect_equal(\n  length(vec_1),\n  4L)",
    "# Testing sum of element lengths",
    "expect_equal(\n  sum(xpectr::element_lengths(vec_1)),\n  4L)"
  )

  expect_equal(
    create_expectations_vector(vec_1),
    exp_vec_1)

  eval_expectations(exp_vec_1, current_envir)


  # Vec 2

  exp_vec_2 <- list(
    "# Testing class",
    "expect_equal(\n  class(vec_2),\n  \"numeric\",\n  fixed = TRUE)",
    "# Testing type",
    "expect_type(\n  vec_2,\n  type = \"double\")",
    "# Testing values",
    "expect_equal(\n  vec_2,\n  c(a = 10, b = 20, c = 30),\n  tolerance = 1e-4)",
    "# Testing names",
    "expect_equal(\n  names(vec_2),\n  c(\"a\", \"b\", \"c\"),\n  fixed = TRUE)",
    "# Testing length",
    "expect_equal(\n  length(vec_2),\n  3L)",
    "# Testing sum of element lengths",
    "expect_equal(\n  sum(xpectr::element_lengths(vec_2)),\n  3L)"
  )

  expect_equal(
    create_expectations_vector(vec_2),
    exp_vec_2
  )
  eval_expectations(exp_vec_2, current_envir)


  # Vec 3

  exp_vec_3 <- list(
    "# Testing class",
    "expect_equal(\n  class(vec_3),\n  \"numeric\",\n  fixed = TRUE)",
    "# Testing type",
    "expect_type(\n  vec_3,\n  type = \"double\")",
    "# Testing values",
    "expect_equal(\n  vec_3,\n  c(a1 = 10, a2 = 2, a3 = 4, b1 = 20, b2 = 1, b3 = 3, c1 = 2, c2 = 3, \n    c3 = 30),\n  tolerance = 1e-4)",
    "# Testing names",
    "expect_equal(\n  names(vec_3),\n  c(\"a1\", \"a2\", \"a3\", \"b1\", \"b2\", \"b3\", \"c1\", \"c2\", \"c3\"),\n  fixed = TRUE)",
    "# Testing length",
    "expect_equal(\n  length(vec_3),\n  9L)",
    "# Testing sum of element lengths",
    "expect_equal(\n  sum(xpectr::element_lengths(vec_3)),\n  9L)"
  )

  expect_equal(
    create_expectations_vector(vec_3),
    exp_vec_3
  )
  eval_expectations(exp_vec_3, current_envir)


  # Vec 4
  exp_vec_4 <- list(
    "# Testing class",
    "expect_equal(\n  class(vec_4),\n  \"list\",\n  fixed = TRUE)",
    "# Testing type",
    "expect_type(\n  vec_4,\n  type = \"list\")",
    "# Testing values",
    "expect_equal(\n  vec_4,\n  list(a1 = 10, a2 = 2, a3 = 4, b1 = 20, b2 = 1, b3 = 3, c1 = 2, c2 = 3, \n    c3 = 30))",
    "# Testing names",
    "expect_equal(\n  names(vec_4),\n  c(\"a1\", \"a2\", \"a3\", \"b1\", \"b2\", \"b3\", \"c1\", \"c2\", \"c3\"),\n  fixed = TRUE)",
    "# Testing length",
    "expect_equal(\n  length(vec_4),\n  9L)",
    "# Testing sum of element lengths",
    "expect_equal(\n  sum(xpectr::element_lengths(vec_4)),\n  9L)"
  )

  expect_equal(
    create_expectations_vector(vec_4, tolerance = "1e-5"),
    exp_vec_4
  )
  eval_expectations(exp_vec_4, current_envir)

  # vec_5
  exp_vec_5 <- list(
    "# Testing class",
    "expect_equal(\n  class(vec_5),\n  \"list\",\n  fixed = TRUE)",
    "# Testing type",
    "expect_type(\n  vec_5,\n  type = \"list\")",
    "# Testing values",
    "expect_equal(\n  vec_5[[\"a\"]],\n  list(10, 2, 4))",
    "expect_equal(\n  vec_5[[\"b\"]],\n  list(20, 1, 3))",
    "expect_equal(\n  vec_5[[\"c\"]],\n  list(2, 3, 30))",
    "# Testing names",
    "expect_equal(\n  names(vec_5),\n  c(\"a\", \"b\", \"c\"),\n  fixed = TRUE)",
    "# Testing length",
    "expect_equal(\n  length(vec_5),\n  3L)",
    "# Testing sum of element lengths",
    "expect_equal(\n  sum(xpectr::element_lengths(vec_5)),\n  9L)",
    "# Testing element classes",
    "expect_equal(\n  xpectr::element_classes(vec_5),\n  c(\"list\", \"list\", \"list\"),\n  fixed = TRUE)",
    "# Testing element types",
    "expect_equal(\n  xpectr::element_types(vec_5),\n  c(\"list\", \"list\", \"list\"),\n  fixed = TRUE)"
  )

  expect_equal(
    create_expectations_vector(vec_5),
    exp_vec_5
  )
  eval_expectations(exp_vec_5, current_envir)

  # vec_6 (Not all elements are named)
  exp_vec_6 <- list(
    "# Testing class",
    "expect_equal(\n  class(vec_6),\n  \"list\",\n  fixed = TRUE)",
    "# Testing type",
    "expect_type(\n  vec_6,\n  type = \"list\")",
    "# Testing values",
    "expect_equal(\n  vec_6,\n  list(a = list(10, 2, 4), b = list(20, 1, 3), list(2, 3, 30)))",
    "# Testing names",
    "expect_equal(\n  names(vec_6),\n  c(\"a\", \"b\", \"\"),\n  fixed = TRUE)",
    "# Testing length",
    "expect_equal(\n  length(vec_6),\n  3L)",
    "# Testing sum of element lengths",
    "expect_equal(\n  sum(xpectr::element_lengths(vec_6)),\n  9L)"
  )

  expect_equal(
    create_expectations_vector(vec_6),
    exp_vec_6
  )
  eval_expectations(exp_vec_6, current_envir)

  # vec_7

  exp_vec_7 <- list(
    "# Testing class",
    "expect_equal(\n  class(vec_7),\n  \"list\",\n  fixed = TRUE)",
    "# Testing type",
    "expect_type(\n  vec_7,\n  type = \"list\")",
    "# Testing values",
    "expect_equal(\n  vec_7,\n  list(5, 6, 7, 8))",
    "# Testing names",
    "expect_equal(\n  names(vec_7),\n  NULL,\n  fixed = TRUE)",
    "# Testing length",
    "expect_equal(\n  length(vec_7),\n  4L)",
    "# Testing sum of element lengths",
    "expect_equal(\n  sum(xpectr::element_lengths(vec_7)),\n  4L)"
  )

  expect_equal(
    create_expectations_vector(vec_7),
    exp_vec_7
  )
  eval_expectations(exp_vec_7, current_envir)
})

test_that("expectations are created returned by insertExpectationsAddin()", {

  set_test_seed(1)
  error_fn <- function() {
    warning("hehe")
    message("hihi")
    stop("STOP NOW!")
  }

  set_test_seed(1)
  expect_equal(
    insertExpectationsAddin("error_fn()", insert = FALSE),
    list(
      " ",
      "## Testing 'error_fn()'                                                     ####",
      "## Initially generated by xpectr",
      "xpectr::set_test_seed(42)",
      "# Testing side effects",
      "# Assigning side effects",
      "side_effects_12655 <- xpectr::capture_side_effects(error_fn(), reset_seed = TRUE)",
      "expect_equal(\n  xpectr::strip(side_effects_12655[['error']]),\n  xpectr::strip(\"STOP NOW!\"),\n  fixed = TRUE)",
      "expect_equal(\n  xpectr::strip(side_effects_12655[['error_class']]),\n  xpectr::strip(c(\"simpleError\", \"error\", \"condition\")),\n  fixed = TRUE)",
      "## Finished testing 'error_fn()'                                            ####",
      " "
    ),
    fixed = TRUE)

  msgs_warns_fn <- function() {
    warning("hehe")
    message("hihi")
    warning("ohhh")
    message("ihhh")
  }

  set_test_seed(3)
  expect_equal(
    insertExpectationsAddin("msgs_warns_fn()", insert = FALSE),
    list(
      " ",
      "## Testing 'msgs_warns_fn()'                                                ####",
      "## Initially generated by xpectr",
      "xpectr::set_test_seed(42)",
      "# Testing side effects",
      "# Assigning side effects",
      "side_effects_11680 <- xpectr::capture_side_effects(msgs_warns_fn(), reset_seed = TRUE)",
      "expect_equal(\n  xpectr::strip(side_effects_11680[['warnings']]),\n  xpectr::strip(c(\"hehe\", \"ohhh\")),\n  fixed = TRUE)",
      "expect_equal(\n  xpectr::strip(side_effects_11680[['messages']]),\n  xpectr::strip(c(\"hihi\\n\", \"ihhh\\n\")),\n  fixed = TRUE)",
      "# Testing is NULL",
      "expect_true(\n  is.null(xpectr::suppress_mw(msgs_warns_fn())))",
      "## Finished testing 'msgs_warns_fn()'                                       ####",
      " "
    ),
    fixed = TRUE)

  set_test_seed(3)
  expect_equal(
    gxs_selection("msgs_warns_fn()", out = "return", strip = FALSE),
    list(
      " ",
      "## Testing 'msgs_warns_fn()'                                                ####",
      "## Initially generated by xpectr",
      "xpectr::set_test_seed(42)",
      "# Testing side effects",
      "# Assigning side effects",
      "side_effects_11680 <- xpectr::capture_side_effects(msgs_warns_fn(), reset_seed = TRUE)",
      "expect_equal(\n  side_effects_11680[['warnings']],\n  c(\"hehe\", \"ohhh\"),\n  fixed = TRUE)",
      "expect_equal(\n  side_effects_11680[['messages']],\n  c(\"hihi\\n\", \"ihhh\\n\"),\n  fixed = TRUE)",
      "# Testing is NULL",
      "expect_true(\n  is.null(xpectr::suppress_mw(msgs_warns_fn())))",
      "## Finished testing 'msgs_warns_fn()'                                       ####",
      " "
    ),
    fixed = TRUE)

  set_test_seed(42)
  long_df <- data.frame("a" = runif(40), "b" = runif(40), stringsAsFactors = FALSE)
  # Check smpl is used
  expect_equal(
    gxs_selection("long_df", out = "return", strip = FALSE),
    list(
      " ",
      "## Testing 'long_df'                                                        ####",
      "## Initially generated by xpectr",
      "xpectr::set_test_seed(42)",
      "# Testing class",
      "expect_equal(\n  class(long_df),\n  \"data.frame\",\n  fixed = TRUE)",
      "# Testing column values",
      paste0("expect_equal(\n  xpectr::smpl(long_df[[\"a\"]], n = 30),\n ",
             " c(0.93708, 0.28614, 0.64175, 0.5191, 0.73659, 0.13467, 0.70",
             "506, \n    0.45774, 0.71911, 0.93467, 0.25543, 0.97823, 0.11",
             "749, 0.475, \n    0.90403, 0.13871, 0.98889, 0.94667, 0.0824",
             "4, 0.51421, 0.3902, \n    0.44697, 0.836, 0.7376, 0.81106, 0",
             ".00395, 0.83292, 0.00733, \n    0.20766, 0.61178),\n  tolera",
             "nce = 1e-4)"),
      paste0("expect_equal(\n  xpectr::smpl(long_df[[\"b\"]], n = 30),\n ",
             " c(0.43577, 0.03743, 0.43175, 0.95758, 0.88775, 0.63998, 0.6",
             "1884, \n    0.33343, 0.34675, 0.39849, 0.78469, 0.67728, 0.1",
             "7126, 0.26109, \n    0.67561, 0.98282, 0.75954, 0.56649, 0.8",
             "4969, 0.18947, 0.27129, \n    0.6932, 0.24054, 0.04299, 0.14",
             "048, 0.19741, 0.71936, 0.00788, \n    0.37549, 0.00157),\n  ",
             "tolerance = 1e-4)"),
      "# Testing column names",
      "expect_equal(\n  names(long_df),\n  c(\"a\", \"b\"),\n  fixed = TRUE)",
      "# Testing column classes",
      "expect_equal(\n  xpectr::element_classes(long_df),\n  c(\"numeric\", \"numeric\"),\n  fixed = TRUE)",
      "# Testing column types",
      "expect_equal(\n  xpectr::element_types(long_df),\n  c(\"double\", \"double\"),\n  fixed = TRUE)",
      "# Testing dimensions",
      "expect_equal(\n  dim(long_df),\n  c(40L, 2L))",
      "# Testing group keys",
      "expect_equal(\n  colnames(dplyr::group_keys(long_df)),\n  character(0),\n  fixed = TRUE)",
      "## Finished testing 'long_df'                                               ####",
      " "
    ),
    fixed = TRUE)

})

test_that("capture_side_effects() works", {

  # error
  error_fn <- function() {
    message("hey")
    warning("you don't see me")
    stop("lols I'm an error")
  }

  err_sfx <- capture_side_effects(error_fn())
  expect_equal(err_sfx$error, "lols I'm an error", fixed = TRUE)
  expect_null(err_sfx$warnings)
  expect_null(err_sfx$messages)
  expect_true(err_sfx$has_side_effects)

  msgs_warns_fn <- function() {
    message("hey")
    warning("you see me??")
    message("hey again")
    warning("here I aaam!!")
  }

  warn_sfx <- capture_side_effects(msgs_warns_fn())
  expect_null(warn_sfx$error)
  expect_equal(warn_sfx$warnings,
               c("you see me??", "here I aaam!!"),
               fixed = TRUE)
  expect_equal(warn_sfx$messages,
               c("hey\n", "hey again\n"),
               fixed = TRUE)
  expect_true(warn_sfx$has_side_effects)

  random_sfx_fn <- function() {
    r <- round(runif(1), digits=5)
    message(r)
    warning(r)
    r
  }

  set_test_seed(42)
  random_sfx <- capture_side_effects(random_sfx_fn())
  expect_null(random_sfx$error)
  expect_equal(random_sfx$warnings,
               "0.91481",
               fixed = TRUE)
  expect_equal(random_sfx$messages,
               "0.91481\n",
               fixed = TRUE)
  expect_true(random_sfx$has_side_effects)


  ## Testing 'random_sfx_fn()'                                              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(random_sfx_fn(), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['warnings']]),
    xpectr::strip("0.91481"),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['messages']]),
    xpectr::strip("0.91481\n"),
    fixed = TRUE)
  # Assigning output
  output_19148 <- xpectr::suppress_mw(random_sfx_fn())
  # Testing class
  expect_equal(
    class(output_19148),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "double")
  # Testing values
  expect_equal(
    output_19148,
    0.91481,
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    1L)
  ## Finished testing 'random_sfx_fn()'                                     ####



})

test_that("create_test_comment() works",{

  # gxs_function(
  #   fn = create_test_comment,
  #   args_values = list(
  #     "what" = list("a test of \"(x)\"", "", NA, 3, list(1,2,3,4)),
  #     "section" = list("test", "intro", "outro", "manual", "blabla"),
  #     "indentation" = list(0, 3, -3, NA),
  #     "create_comment" = list(TRUE, FALSE, NA)
  #   ), indentation = 2
  # )


  ## Testing 'create_test_comment'                                            ####
  ## Initially generated by xpectr
  # Testing different combinations of argument values

  # Testing create_test_comment(what = "a test of \"(x)\""...
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- create_test_comment(what = "a test of \"(x)\"", section = "test", indentation = 0, create_comment = TRUE)
  # Testing class
  expect_equal(
    class(output_19148),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "character")
  # Testing values
  expect_equal(
    output_19148,
    "# Testing a test of \"(x)\"",
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    1L)

  # Testing create_test_comment(what = "a test of \"(x)\""...
  # Changed from baseline: create_comment
  xpectr::set_test_seed(42)
  # Testing is NULL
  expect_true(
    is.null(create_test_comment(what = "a test of \"(x)\"", section = "test", indentation = 0, create_comment = FALSE)))

  # Testing create_test_comment(what = "a test of \"(x)\""...
  # Changed from baseline: create_comment
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(create_test_comment(what = "a test of \"(x)\"", section = "test", indentation = 0, create_comment = NA)),
    xpectr::strip("1 assertions failed:\n * Variable 'create_comment': May not be NA."),
    fixed = TRUE)

  # Testing create_test_comment(what = "a test of \"(x)\""...
  # Changed from baseline: create_comment
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(create_test_comment(what = "a test of \"(x)\"", section = "test", indentation = 0, create_comment = NULL)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'create_comment': Must be ",
                         "of type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing create_test_comment(what = "a test of \"(x)\""...
  # Changed from baseline: indentation
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(create_test_comment(what = "a test of \"(x)\"", section = "test", indentation = -3, create_comment = TRUE)),
    xpectr::strip("1 assertions failed:\n * Variable 'indentation': Must be >= 0."),
    fixed = TRUE)

  # Testing create_test_comment(what = "a test of \"(x)\""...
  # Changed from baseline: indentation
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(create_test_comment(what = "a test of \"(x)\"", section = "test", indentation = NA, create_comment = TRUE)),
    xpectr::strip("1 assertions failed:\n * Variable 'indentation': May not be NA."),
    fixed = TRUE)

  # Testing create_test_comment(what = "a test of \"(x)\""...
  # Changed from baseline: indentation
  xpectr::set_test_seed(42)
  # Assigning output
  output_17365 <- create_test_comment(what = "a test of \"(x)\"", section = "test", indentation = 3, create_comment = TRUE)
  # Testing class
  expect_equal(
    class(output_17365),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_17365,
    type = "character")
  # Testing values
  expect_equal(
    output_17365,
    "# Testing a test of \"(x)\"",
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_17365),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_17365),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_17365)),
    1L)

  # Testing create_test_comment(what = "a test of \"(x)\""...
  # Changed from baseline: indentation
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(create_test_comment(what = "a test of \"(x)\"", section = "test", indentation = NULL, create_comment = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'indentation': Must be of ",
                         "type 'count', not 'NULL'.")),
    fixed = TRUE)

  # Testing create_test_comment(what = "a test of \"(x)\""...
  # Changed from baseline: section
  xpectr::set_test_seed(42)
  # Assigning output
  output_16569 <- create_test_comment(what = "a test of \"(x)\"", section = "intro", indentation = 0, create_comment = TRUE)
  # Testing class
  expect_equal(
    class(output_16569),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_16569,
    type = "character")
  # Testing values
  expect_equal(
    output_16569,
    c("## Testing 'a test of \"(x)\"'                                                ####",
      "## Initially generated by xpectr"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_16569),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_16569),
    2L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_16569)),
    2L)

  # Testing create_test_comment(what = "a test of \"(x)\""...
  # Changed from baseline: section
  xpectr::set_test_seed(42)
  # Assigning output
  output_17050 <- create_test_comment(what = "a test of \"(x)\"", section = "outro", indentation = 0, create_comment = TRUE)
  # Testing class
  expect_equal(
    class(output_17050),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_17050,
    type = "character")
  # Testing values
  expect_equal(
    output_17050,
    "## Finished testing 'a test of \"(x)\"'                                       ####",
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_17050),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_17050),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_17050)),
    1L)

  # Testing create_test_comment(what = "a test of \"(x)\""...
  # Changed from baseline: section
  xpectr::set_test_seed(42)
  # Assigning output
  output_14577 <- create_test_comment(what = "a test of \"(x)\"", section = "manual", indentation = 0, create_comment = TRUE)
  # Testing class
  expect_equal(
    class(output_14577),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_14577,
    type = "character")
  # Testing values
  expect_equal(
    output_14577,
    "# a test of \"(x)\"",
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_14577),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_14577),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_14577)),
    1L)

  # Testing create_test_comment(what = "a test of \"(x)\""...
  # Changed from baseline: section
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(create_test_comment(what = "a test of \"(x)\"", section = "blabla", indentation = 0, create_comment = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'section': Must be element",
                         " of set {'intro','outro','test','manual'}, but is 'blabla'.")),
    fixed = TRUE)

  # Testing create_test_comment(what = "a test of \"(x)\""...
  # Changed from baseline: section
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(create_test_comment(what = "a test of \"(x)\"", section = NULL, indentation = 0, create_comment = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'section': Must be a subse",
                         "t of {'intro','outro','test','manual'}, not 'NULL'.")),
    fixed = TRUE)

  # Testing create_test_comment(what = "", section = "test...
  # Changed from baseline: what
  xpectr::set_test_seed(42)
  # Assigning output
  output_12554 <- create_test_comment(what = "", section = "test", indentation = 0, create_comment = TRUE)
  # Testing class
  expect_equal(
    class(output_12554),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_12554,
    type = "character")
  # Testing values
  expect_equal(
    output_12554,
    "# Testing ",
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_12554),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_12554),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_12554)),
    1L)

  # Testing create_test_comment(what = NA, section = "test...
  # Changed from baseline: what
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(create_test_comment(what = NA, section = "test", indentation = 0, create_comment = TRUE)),
    xpectr::strip("1 assertions failed:\n * Variable 'what': May not be NA."),
    fixed = TRUE)

  # Testing create_test_comment(what = 3, section = "test"...
  # Changed from baseline: what
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(create_test_comment(what = 3, section = "test", indentation = 0, create_comment = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'what': Must be of type 's",
                         "tring', not 'double'.")),
    fixed = TRUE)

  # Testing create_test_comment(what = list(1, 2, 3, 4), s...
  # Changed from baseline: what
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(create_test_comment(what = list(1, 2, 3, 4), section = "test", indentation = 0, create_comment = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'what': Must be of type 's",
                         "tring', not 'list'.")),
    fixed = TRUE)

  # Testing create_test_comment(what = NULL, section = "te...
  # Changed from baseline: what
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(create_test_comment(what = NULL, section = "test", indentation = 0, create_comment = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'what': Must be of type 's",
                         "tring', not 'NULL'.")),
    fixed = TRUE)

  ## Finished testing 'create_test_comment'                                   ####

})



