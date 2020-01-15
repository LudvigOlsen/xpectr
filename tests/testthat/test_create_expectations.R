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
    "expect_equal(\n  df1[[\"a\"]],\n  c(1, 2, 3, 4),\n  tolerance = 1e-4)",
    "expect_equal(\n  df1[[\"b\"]],\n  c(\"a\", \"f\", \"g\", \"s\"),\n  fixed = TRUE)",
    paste0("expect_equal(\n  df1[[\"f\"]],\n  structure(1:4, .Label = c",
           "(\"1\", \"2\", \"3\", \"4\"), class = \"factor\"))"),
    "expect_equal(\n  names( df1 ),\n  c(\"a\", \"b\", \"c\", \"d\", \"f\"),\n  fixed = TRUE)"
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
  expect_equal(
    create_expectations_vector(vec_1),
    list("expect_equal(\n  vec_1,\n  c(1, 2, 3, 4),\n  tolerance = 1e-4)")
  )
  expect_equal(
    vec_1,
    c(1, 2, 3, 4),
    tolerance = 1e-4
  )

  # Vec 2
  create_expectations_vector(vec_2)
  expect_equal(
    create_expectations_vector(vec_2),
    list(
      "expect_equal(\n  vec_2[[\"a\"]],\n  10,\n  tolerance = 1e-4)",
      "expect_equal(\n  vec_2[[\"b\"]],\n  20,\n  tolerance = 1e-4)",
      "expect_equal(\n  vec_2[[\"c\"]],\n  30,\n  tolerance = 1e-4)",
      "expect_equal(\n  names( vec_2 ),\n  c(\"a\", \"b\", \"c\"),\n  fixed = TRUE)"
    )
  )
  expect_equal(names(vec_2), c("a", "b", "c"))
  expect_equal(vec_2[["a"]], 10, tolerance = 1e-4)
  expect_equal(vec_2[["b"]], 20, tolerance = 1e-4)
  expect_equal(vec_2[["c"]], 30, tolerance = 1e-4)

  exp_vec_3 <- list(
    "expect_equal(\n  vec_3[[\"a1\"]],\n  10,\n  tolerance = 1e-4)",
    "expect_equal(\n  vec_3[[\"a2\"]],\n  2,\n  tolerance = 1e-4)",
    "expect_equal(\n  vec_3[[\"a3\"]],\n  4,\n  tolerance = 1e-4)",
    "expect_equal(\n  vec_3[[\"b1\"]],\n  20,\n  tolerance = 1e-4)",
    "expect_equal(\n  vec_3[[\"b2\"]],\n  1,\n  tolerance = 1e-4)",
    "expect_equal(\n  vec_3[[\"b3\"]],\n  3,\n  tolerance = 1e-4)",
    "expect_equal(\n  vec_3[[\"c1\"]],\n  2,\n  tolerance = 1e-4)",
    "expect_equal(\n  vec_3[[\"c2\"]],\n  3,\n  tolerance = 1e-4)",
    "expect_equal(\n  vec_3[[\"c3\"]],\n  30,\n  tolerance = 1e-4)",
    paste0("expect_equal(\n  names( vec_3 ),\n  c(\"a1\", \"a2\", \"a3",
           "\", \"b1\", \"b2\", \"b3\", \"c1\", \"c2\", \"c3\"),\n  fixed",
           " = TRUE)")
  )
  expect_equal(
    create_expectations_vector(vec_3),
    exp_vec_3
  )
  eval_expectations(exp_vec_3, current_envir)

  # vec_4
  exp_vec_4 <- list(
    "expect_equal(\n  vec_4[[\"a1\"]],\n  10,\n  tolerance = 1e-5)",
    "expect_equal(\n  vec_4[[\"a2\"]],\n  2,\n  tolerance = 1e-5)",
    "expect_equal(\n  vec_4[[\"a3\"]],\n  4,\n  tolerance = 1e-5)",
    "expect_equal(\n  vec_4[[\"b1\"]],\n  20,\n  tolerance = 1e-5)",
    "expect_equal(\n  vec_4[[\"b2\"]],\n  1,\n  tolerance = 1e-5)",
    "expect_equal(\n  vec_4[[\"b3\"]],\n  3,\n  tolerance = 1e-5)",
    "expect_equal(\n  vec_4[[\"c1\"]],\n  2,\n  tolerance = 1e-5)",
    "expect_equal(\n  vec_4[[\"c2\"]],\n  3,\n  tolerance = 1e-5)",
    "expect_equal(\n  vec_4[[\"c3\"]],\n  30,\n  tolerance = 1e-5)",
    paste0("expect_equal(\n  names( vec_4 ),\n  c(\"a1\", \"a2\", \"a3",
         "\", \"b1\", \"b2\", \"b3\", \"c1\", \"c2\", \"c3\"),\n  fixed = TRUE)")

  )
  expect_equal(
    create_expectations_vector(vec_4, tolerance = "1e-5"),
    exp_vec_4
  )
  eval_expectations(exp_vec_4, current_envir)

  # vec_5
  exp_vec_5 <-
    list(
      "expect_equal(\n  vec_5[[\"a\"]],\n  list(10, 2, 4))",
      "expect_equal(\n  vec_5[[\"b\"]],\n  list(20, 1, 3))",
      "expect_equal(\n  vec_5[[\"c\"]],\n  list(2, 3, 30))",
      paste0("expect_equal(\n  names( vec_5 ),\n  c(\"a\", \"b\", \"c\"),",
             "\n  fixed = TRUE)")
    )
  expect_equal(
    create_expectations_vector(vec_5),
    exp_vec_5
  )
  eval_expectations(exp_vec_5, current_envir)

  # vec_6
  exp_vec_6 <- list(paste0("expect_equal(\n  vec_6,\n  list(a = list(10, 2, 4), b = lis",
                           "t(20, 1, 3), list(2, 3, 30)))"))
  expect_equal(
    create_expectations_vector(vec_6),
    exp_vec_6
  )
  eval_expectations(exp_vec_6, current_envir)

  # vec_7
  expect_equal(
    create_expectations_vector(vec_7),
    list("expect_equal(\n  vec_7,\n  list(5, 6, 7, 8))")
  )
  expect_equal(vec_7, list(5, 6, 7, 8))
})

test_that("expectations are created returned by insertExpectationsAddin()", {

  error_fn <- function() {
    warning("hehe")
    message("hihi")
    stop("STOP NOW!")
  }

  expect_equal(
    insertExpectationsAddin("error_fn()", insert = FALSE)[[1]],
    "expect_error(\n  xpectr::strip_msg(error_fn()),\n  xpectr::strip(\"STOP NOW!\"),\n  fixed = TRUE)",
    fixed = TRUE)

  msgs_warns_fn <- function() {
    warning("hehe")
    message("hihi")
    warning("ohhh")
    message("ihhh")
  }

  expect_equal(
    insertExpectationsAddin("msgs_warns_fn()", insert = FALSE),
    list("expect_warning(\n  xpectr::strip_msg(msgs_warns_fn()),\n  xpectr::strip(\"hehe\"),\n  fixed = TRUE)",
         "expect_warning(\n  xpectr::strip_msg(msgs_warns_fn()),\n  xpectr::strip(\"ohhh\"),\n  fixed = TRUE)",
         "expect_message(\n  xpectr::strip_msg(msgs_warns_fn()),\n  xpectr::strip(\"hihi\\n\"),\n  fixed = TRUE)",
         "expect_message(\n  xpectr::strip_msg(msgs_warns_fn()),\n  xpectr::strip(\"ihhh\\n\"),\n  fixed = TRUE)"),
    fixed = TRUE)

  expect_equal(
    gxs_selection("msgs_warns_fn()", out = "return", strip = FALSE),
    list("expect_warning(\n  msgs_warns_fn(),\n  \"hehe\",\n  fixed = TRUE)",
    "expect_warning(\n  msgs_warns_fn(),\n  \"ohhh\",\n  fixed = TRUE)",
    "expect_message(\n  msgs_warns_fn(),\n  \"hihi\\n\",\n  fixed = TRUE)",
    "expect_message(\n  msgs_warns_fn(),\n  \"ihhh\\n\",\n  fixed = TRUE)"),
    fixed = TRUE)

})

test_that("capture_side_effects() works", {

  # error
  error_fn <- function() {
    message("hey")
    warning("you don't see me")
    stop("lols I'm an error")
  }

  err_sfx <- capture_side_effects(error_fn)
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

  warn_sfx <- capture_side_effects(fn = msgs_warns_fn)
  expect_null(warn_sfx$error)
  expect_equal(warn_sfx$warnings,
               c("you see me??", "here I aaam!!"),
               fixed = TRUE)
  expect_equal(warn_sfx$messages,
               c("hey\n", "hey again\n"),
               fixed = TRUE)
  expect_true(warn_sfx$has_side_effects)
})


