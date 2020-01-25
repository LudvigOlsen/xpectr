library(xpectr)
context("element descriptors")

test_that("num_total_elements() works", {

  l <- list(list(list(1, 2, 3), list(2, list(3, 2))),
            list(1, list(list(2, 4), list(7, 1, list(3, 8)))),
            list(list(2, 7, 8), list(10, 2, list(18, 1, 4))))

  expect_equal(num_total_elements(l), 21)
  expect_equal(num_total_elements(l, deduplicated = TRUE), 8)

  l <- list(list(list("1", "2", "3"), list("2", list("3", "2"))),
            list("1", list(list("2", "4"), list("7", "1", list("3", "8")))),
            list(list("2", "7", "8"), list("10", "2", list("18", "1", "4"))))

  expect_equal(num_total_elements(l), 21)
  expect_equal(num_total_elements(l, deduplicated = TRUE), 8)

})

test_that("element_lengths() works", {

  l <- list(list(list(1, 2, 3), list(2, list(3, 2))),
            list(1, list(list(2, 4), list(7, 1, list(3, 8)))),
            list(list(2, 7, 8), list(10, 2, list(18, 1, 4))))

  expect_equal(element_lengths(l), c(2, 2, 2))
  expect_equal(element_lengths(l, keep_names = TRUE), c(2, 2, 2))

  l <- list("x" = list(list(1, 2, 3), list(2, list(3, 2))),
            "y" = list(1, list(list(2, 4), list(7, 1, list(3, 8)))),
            "z" = list(list(2, 7, 8), list(10, 2, list(18, 1, 4))))

  expect_equal(element_lengths(l), c(2, 2, 2))
  expect_equal(element_lengths(l, keep_names = TRUE),
               c(x = 2L, y = 2L, z = 2L))

  vec <- list("a" = c(1,2,3), "b" = c(4,3,2), "c" = c(7,6,5,4,3,2))
  expect_equal(element_lengths(vec), c(3, 3, 6))

})

test_that("element_types() works", {

  l <- list("a" = c(1,2,3), "b" = "a", "c" = NULL, "d" = NA, "e" = formula(x ~ b))

  expect_equal(element_types(l),
               c("double", "character", "NULL", "logical", "language"))
  expect_equal(element_types(l, keep_names = TRUE),
               c(a = "double", b = "character", c = "NULL",
                 d = "logical", e = "language"))

})

test_that("element_classes() works", {

  l <- list("a" = c(1,2,3), "b" = "a", "c" = NULL, "d" = NA,
            "e" = formula(x ~ b))

  expect_equal(element_classes(l),
               c("numeric", "character", "NULL", "logical", "formula"))
  expect_equal(element_classes(l, keep_names = TRUE),
               c(a = "numeric", b = "character",
                 c = "NULL", d = "logical", e = "formula"))

})
