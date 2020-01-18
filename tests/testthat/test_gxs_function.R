library(xpectr)
context("gxs_function()")

test_that("gxs_function() works", {

  # Some arbitrary function
  # Have constraints on the arguments
  # and will output slightly different things depending on
  # the values of the args
  fn <- function(a, a_fn, b, d = NULL, e = 3, f = "hi"){

    # Check arguments ####
    assert_collection <- checkmate::makeAssertCollection()
    checkmate::assert_number(x = a, lower = 4, upper = 6, add = assert_collection)
    checkmate::assert_function(x = a_fn, add = assert_collection)
    checkmate::assert_character(x = b, min.chars = 3,
                                any.missing = FALSE,
                                len = 2,
                                add = assert_collection)
    checkmate::assert_data_frame(
      x = d,
      min.rows = 3,
      min.cols = 1,
      col.names = "named",
      null.ok = TRUE,
      add = assert_collection
    )
    checkmate::assert_count(
      x = e,
      positive = TRUE,
      null.ok = TRUE,
      add = assert_collection
    )
    checkmate::assert_string(x = f, min.chars = 2, add = assert_collection)
    checkmate::reportAssertions(assert_collection)
    # End of argument checks ####

    n <- a_fn(a * e)
    m <- paste0(f, ": ", paste0(b, collapse = ", "))

    if (!is.null(d)){
      d[["column"]] <- n
      output <- list("d" = d, "m" = m)
    } else {
      output <- list("n" = n, "m" = m)
    }

    output
  }

  current_envir <- sys.frame(which = sys.nframe())

  expected_tests <- c(
    "expect_error(\n  xpectr::strip_msg(fn(a = 2, a_fn = mean, b = \"ah\")),\n  xpectr::strip(paste0(\"2 assertions failed:\\n * Variable 'a': Element 0 is not >= 4\",\n         \".\\n * Variable 'b': Must have length 2, but has length 1.\")),\n  fixed = TRUE)",
    "expect_error(\n  xpectr::strip_msg(fn(a = 1, a_fn = sum, b = \"ah\")),\n  xpectr::strip(paste0(\"2 assertions failed:\\n * Variable 'a': Element 0 is not >= 4\",\n         \".\\n * Variable 'b': Must have length 2, but has length 1.\")),\n  fixed = TRUE)",
    "expect_error(\n  xpectr::strip_msg(fn(a = 1, a_fn = mean, b = \"eh\")),\n  xpectr::strip(paste0(\"2 assertions failed:\\n * Variable 'a': Element 0 is not >= 4\",\n         \".\\n * Variable 'b': Must have length 2, but has length 1.\")),\n  fixed = TRUE)",
    "expect_error(\n  xpectr::strip_msg(fn(a = NULL, a_fn = mean, b = \"ah\")),\n  xpectr::strip(paste0(\"2 assertions failed:\\n * Variable 'a': Must be of type 'numb\",\n         \"er', not 'NULL'.\\n * Variable 'b': Must have length 2, but h\",\n         \"as length 1.\")),\n  fixed = TRUE)",
    "expect_error(\n  xpectr::strip_msg(fn(a = 1, a_fn = NULL, b = \"ah\")),\n  xpectr::strip(paste0(\"3 assertions failed:\\n * Variable 'a': Element 0 is not >= 4\",\n         \".\\n * Variable 'a_fn': Must be a function, not 'NULL'.\\n * V\",\n         \"ariable 'b': Must have length 2, but has length 1.\")),\n  fixed = TRUE)",
    "expect_error(\n  xpectr::strip_msg(fn(a = 1, a_fn = mean, b = NULL)),\n  xpectr::strip(paste0(\"2 assertions failed:\\n * Variable 'a': Element 0 is not >= 4\",\n         \".\\n * Variable 'b': Must be of type 'character', not 'NULL'.\")),\n  fixed = TRUE)",
    "expect_error(\n  xpectr::strip_msg(fn(a = 1, a_fn = mean, b = \"ah\")),\n  xpectr::strip(paste0(\"2 assertions failed:\\n * Variable 'a': Element 0 is not >= 4\",\n         \".\\n * Variable 'b': Must have length 2, but has length 1.\")),\n  fixed = TRUE)"
  )

  expect_equal(
    strip(gxs_function(fn, list(
        "a" = list(1, 2),
        a_fn = list(mean, sum),
        "b" = list("ah", "eh")
      ),
      out = "return")),
    strip(expected_tests),
    fixed = TRUE)
  expect_equal(
  length( gxs_function(fn, list(
        "a" = list(1, 2),
        a_fn = list(mean, sum),
        "b" = list("ah", "eh")
      ),
      out = "return") ),
  7L)

  eval_expectations(expected_tests,envir = current_envir)

})
