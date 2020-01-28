library(xpectr)
context("helpers")

test_that("create_output_var_name()", {

  set_test_seed(3)
  expect_equal(create_output_var_name(prefix = "output_"), "output_11680")
  expect_equal(create_output_var_name(prefix = "output_",id = 674),
               "output_674")
  expect_equal(create_output_var_name(prefix = "lol_",id = 674),
               "lol_674")

})

test_that("add_condition_prefix()", {

  set_test_seed(3)
  expect_equal(add_condition_prefix(m = "3>8"), "This was TRUE: 3>8")

})

test_that("%c%", {

  set_test_seed(3)
  l <- list(
    list("a" = c(1,2,3),
         "b" = c(2,3,4)),
    list("a" = c(4,5,6),
         "b" = list("a" = c(7,8,9)))
  )
  expect_equal(
    l %c% "a", list(c(1, 2, 3), c(4, 5, 6)))

})

test_that("non_empty_names()", {

  set_test_seed(3)
  l <- list("a" = NULL, "b" = c(1,2), c(2,3,4,5))
  expect_equal(non_empty_names(l), c("a", "b"))
  names(l) <- c("b", "i", NA)
  expect_equal(non_empty_names(l), c("b", "i"))

})

test_that("ndigits()", {

  set_test_seed(3)
  expect_equal(ndigits(x = as.numeric("1e+5")), 6)
  expect_equal(ndigits(x = 1000), 4)
  expect_equal(ndigits(x = as.numeric("1e+70")), 71)

})

test_that("assign_random_state()", {

  set_test_seed(3)
  irs <- .Random.seed
  set_test_seed(98)
  assign_random_state(state = irs)
  expect_equal(.Random.seed, irs)

})



