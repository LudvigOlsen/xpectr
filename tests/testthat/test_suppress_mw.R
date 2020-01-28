library(xpectr)
context("suppress_mw()")

test_that("suppress_mw() suppresses properly", {

  fn_1 <- function(a, b){
    message("hey")
    warning("you")
    message("there")
    warning("! :)")
    a + b
  }

  fn_2 <- function(a, b){
    message("hey")
    message("there")
    a + b
  }

  expect_equal(suppress_mw(fn_1(3, 7)), 10)
  expect_equal(suppress_mw(fn_2(3, 7)), 10)
  expect_warning(expect_message(expect_equal(fn_1(3, 7), 10)))
  expect_error(expect_warning(expect_message(expect_equal(fn_2(3, 7), 10))),
               class = "expectation_failure")

})
