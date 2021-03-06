library(xpectr)
context("simplified_formals()")

test_that("simplified_formals() works as expects", {

  ## Testing 'simplified_formals(function(x1 = 1, x2 = NA,...'              ####
  ## Initially generated by xpectr
  # Assigning output
  output_19233 <- simplified_formals(function(x1 = 1, x2 = NA, x3 = NULL, x4 = "", x5){})
  # Testing class
  expect_equal(
    class(output_19233),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19233,
    type = "character")
  # Testing values
  expect_equal(
    output_19233,
    c("x1 = 1", "x2 = NA", "x3 = NULL", "x4 = \"\"", "x5"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_19233),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19233),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19233)),
    5L)
  ## Finished testing 'simplified_formals(function(x1 = 1, x2 = NA,...'     ####

  fn <- function(x1 = 1, x2 = NA, x3 = NULL, x4 = "", x5){}
  ## Testing 'simplified_formals(fn)'                                       ####
  ## Initially generated by xpectr
  # Assigning output
  output_12806 <- simplified_formals(fn)
  # Testing class
  expect_equal(
    class(output_12806),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_12806,
    type = "character")
  # Testing values
  expect_equal(
    output_12806,
    c("x1 = 1", "x2 = NA", "x3 = NULL", "x4 = \"\"", "x5"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_12806),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_12806),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_12806)),
    5L)
  ## Finished testing 'simplified_formals(fn)'                              ####

  ## Testing 'simplified_formals(function(){})'                             ####
  ## Initially generated by xpectr
  # Testing is NULL
  expect_true(
    is.null(simplified_formals(function(){})))
  ## Finished testing 'simplified_formals(function(){})'                    ####
})
