library(xpectr)
context("addin_utils")

test_that("apply_capture() works as expected", {

  # We're not getting it from parent environment
  # so we supply current environment
  current_envir <- sys.frame(which = sys.nframe())

  # Create vector
  a_vec <- c(1, 2, 3, 4)

  expect_equal(
    apply_capture("a_vec", dput, envir = current_envir),
    "c(1, 2, 3, 4)"
  )
  expect_equal(
    capture("a_vec", envir = current_envir),
    "[1] 1 2 3 4"
  )

  expect_equal(
    apply_capture("a_vec", function(x) {
      x + 3
    }, envir = current_envir),
    "[1] 4 5 6 7"
  )
  expect_equal(
    apply_capture("a_vec", function(x) {
      dput(x + 3)
    }, envir = current_envir),
    "c(4, 5, 6, 7)"
  )

  # Create list
  a_list <- list("a" = 3, "b" = c(2, 3))

  expect_equal(
    apply_capture("a_list", dput, envir = current_envir),
    "list(a = 3, b = c(2, 3))"
  )
  expect_equal(
    apply_capture("a_list", identity, envir = current_envir),
    c("$a", "[1] 3", "", "$b", "[1] 2 3", "")
  )

  # Create data frame
  a_df <- data.frame("a" = c(1, 2, 3), "b" = c("c", "d", "e"))

  expect_equal(
    apply_capture("a_df", dput, envir = current_envir),
    c(
      "structure(list(a = c(1, 2, 3), b = structure(1:3, .Label = c(\"c\", ",
      paste0("\"d\", \"e\"), class = \"factor\")), class = \"data.frame\"",
             ", row.names = c(NA, "),
      "-3L))"
    )
  )
  expect_equal(
    apply_capture("a_df", identity, envir = current_envir),
    c("  a b", "1 1 c", "2 2 d", "3 3 e")
  )
})

test_that("eval_string() works as expected", {

  # We're not getting it from parent environment
  # so we supply current environment
  current_envir <- sys.frame(which = sys.nframe())

  # Create vector
  a_vec <- c(1, 2, 3, 4)
  expect_equal(eval_string("a_vec", current_envir), a_vec)
})

test_that("eval_string() works as expected", {
  expect_equal(create_space_string(3), "   ")
})

test_that("create_expect_equal() works as expected", {
  expect_equal(
    create_expect_equal(x = "cat", y = "c(1,2,3)",
                        spaces = 5, add_tolerance = FALSE),
    "expect_equal(\n     cat,\n     c(1, 2, 3))"
  )
  expect_equal(
    create_expect_equal(x = "cat", y = "c(1,2,3)",
                        spaces = 3, add_tolerance = FALSE),
    "expect_equal(\n   cat,\n   c(1, 2, 3))"
  )
  expect_equal(
    create_expect_equal(x = "cat", y = "c(1,2,3)",
                        spaces = 2, add_tolerance = TRUE),
    "expect_equal(\n  cat,\n  c(1, 2, 3),\n  tolerance = 1e-4)"
  )
  expect_equal(
    create_expect_equal(x = "cat", y = "c(1,2,3)",
                        spaces = 2, add_fixed = TRUE),
    "expect_equal(\n  cat,\n  c(1, 2, 3),\n  fixed = TRUE)"
  )
})
