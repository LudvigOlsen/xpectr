library(xpectr)
context("do_if()")

test_that("do_if() works", {
  a <- 0
  fn <- function(x, y) {
    x + y
  }
  expect_equal(do_if(a == 0, fn, x = 2, y = 5), 7)
  expect_invisible(do_if(a != 0, fn, x = 2, y = 5))
  expect_error(do_if(a == 0, fn, x = 2),
    "argument \"y\" is missing, with no default",
    fixed = TRUE
  )
  expect_error(do_if(a == 0, x = 2),
    "argument \"fn\" is missing, with no default",
    fixed = TRUE
  )
  expect_error(do_if(a == 0, 2),
    "1 assertions failed:\n * Variable 'fn': Must be a function, not 'double'.",
    fixed = TRUE
  )
})

test_that("identity_if() works", {
  a <- 0
  fn <- function(x, y) {
    x + y
  }
  expect_identical(identity_if(a == 0, fn), fn)
  expect_identical(identity_if(a == 0, a), a)
  expect_equal(identity_if(a == 0, a), 0)
  expect_invisible(identity_if(a != 0, a))
  expect_error(identity_if(a == 0),
    "argument \"x\" is missing, with no default",
    fixed = TRUE
  )
})

test_that("stop_if() works", {
  a <- 0
  stop_fn <- function(cond, msg = NULL) {
    stop_if(cond, msg)
  }
  expect_error(stop_fn(a == 0, "'a' was 0"),
    "'a' was 0",
    fixed = TRUE
  )
  expect_invisible(stop_fn(a != 0, "'a' was not 0"))
  expect_error(stop_fn(a == 0),
    "This was TRUE: cond",
    fixed = TRUE
  )
  expect_error(stop_if(a == 0),
    "This was TRUE: a == 0",
    fixed = TRUE
  )
  expect_error(stop_if(message = "hej"),
    "argument \"condition\" is missing, with no default",
    fixed = TRUE
  )
  expect_error(stop_if(function() {}, message = "hej"),
    "argument is not interpretable as logical",
    fixed = TRUE
  )
  expect_invisible(stop_if(0, message = "hej"))
})

test_that("warning_if() works", {
  a <- 0
  warn_fn <- function(cond, msg = NULL) {
    warn_if(cond, msg)
  }
  expect_warning(warn_fn(a == 0, "'a' was 0"),
    "'a' was 0",
    fixed = TRUE
  )
  expect_invisible(warn_fn(a != 0, "'a' was not 0"))
  expect_invisible(warn_fn(a != 0))
  expect_warning(warn_fn(a == 0),
    "This was TRUE: cond",
    fixed = TRUE
  )
  expect_warning(warn_if(a == 0),
    "This was TRUE: a == 0",
    fixed = TRUE
  )
  expect_error(warn_if(message = "hej"),
    "argument \"condition\" is missing, with no default",
    fixed = TRUE
  )
  expect_error(warn_if(function() {}, message = "hej"),
    "argument is not interpretable as logical",
    fixed = TRUE
  )
  expect_invisible(warn_if(0, message = "hej"))
})

test_that("message_if() works", {
  a <- 0
  message_fn <- function(cond, msg = NULL) {
    message_if(cond, msg)
  }
  expect_message(message_fn(a == 0, "'a' was 0"),
    "'a' was 0",
    fixed = TRUE
  )
  expect_invisible(message_fn(a != 0, "'a' was not 0"))
  expect_invisible(message_fn(a != 0))
  expect_message(message_fn(a == 0),
    "This was TRUE: cond",
    fixed = TRUE
  )
  expect_message(message_if(a == 0),
    "This was TRUE: a == 0",
    fixed = TRUE
  )
  expect_error(message_if(message = "hej"),
    "argument \"condition\" is missing, with no default",
    fixed = TRUE
  )
  expect_error(message_if(function() {}, message = "hej"),
    "argument is not interpretable as logical",
    fixed = TRUE
  )
  expect_invisible(message_if(0, message = "hej"))
})
