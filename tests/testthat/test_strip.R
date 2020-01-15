library(xpectr)
context("strip()")

test_that("strip() works", {
  strings <- c(
    "Hello! I am George.  \n\rDon't call me Frank!",
    "    \tAs that, is, not, my, name!",
    "", NA, ".!\"'",
    "these 2 numbers 4743 and 12378!"
  )

  expect_equal(
    strip(strings),
    c("Hello I am George Dont call me Frank",
      "As that is not my name", "", NA, "",
      "these 2 numbers 4743 and 12378")
  )
  expect_equal(
    strip(strings, remove_spaces = TRUE),
    c("HelloIamGeorgeDontcallmeFrank", "Asthatisnotmyname",
      "", NA, "","these2numbers4743and12378")
  )
  expect_equal(
    strip(strings, remove_numbers = TRUE),
    c("Hello I am George Dont call me Frank",
      "As that is not my name", "", NA, "",
      "these numbers and")
  )

  # errors

  expect_error(strip(strings, allow_na = FALSE),
    "* Variable 'strings': Contains missing values (element 4).",
    fixed = TRUE
  )
  expect_error(strip(NA, allow_na = FALSE),
    " * Variable 'strings': Contains missing values (element 1).",
    fixed = TRUE
  )
  expect_error(strip(NULL),
    "* Variable 'strings': Must be of type 'character', not 'NULL'.",
    fixed = TRUE
  )
  expect_error(strip(23),
    "* Variable 'strings': Must be of type 'character', not 'double'.",
    fixed = TRUE
  )
  expect_equal(
    strip(strings, replacement = " "),
    c(
      "Hello I am George Don t call me Frank",
      "As that is not my name", "", NA, "",
      "these 2 numbers 4743 and 12378"
    )
  )
})

test_that("strip_msg() works", {

  the_string <- paste0(
    "Hello! I am George.  \n\rDon't call me Frank!",
    "    \tAs that, is, not, my, name!",
    "", ".!\"'",
    "these 2 numbers 4743 and 12378!"
  )

  stripped_string <- strip(the_string)

  expect_equal(stripped_string,
               paste0("Hello I am George Dont call me Frank As that is not my name",
                      "these 2 numbers 4743 and 12378"))

  expect_error(
    strip_msg(stop(the_string)),
    stripped_string
  )
  expect_error(
    strip_msg(stop(the_string), remove_spaces = TRUE),
    strip(the_string, remove_spaces = TRUE)
  )
  expect_error(
    strip_msg(stop(the_string), remove_numbers = TRUE),
    strip(the_string, remove_numbers = TRUE)
  )

  expect_message(
    strip_msg(message(the_string)),
    stripped_string
  )
  expect_message(
    strip_msg(message(the_string), remove_spaces = TRUE),
    strip(the_string, remove_spaces = TRUE)
  )
  expect_message(
    strip_msg(message(the_string), remove_numbers = TRUE),
    strip(the_string, remove_numbers = TRUE)
  )

  expect_warning(
    strip_msg(warning(the_string)),
    stripped_string
  )
  expect_warning(
    strip_msg(warning(the_string), remove_spaces = TRUE),
    strip(the_string, remove_spaces = TRUE)
  )
  expect_warning(
    strip_msg(warning(the_string), remove_numbers = TRUE),
    strip(the_string, remove_numbers = TRUE)
  )

  expect_invisible(strip_msg(NULL))
  expect_invisible(strip_msg(NA))
  expect_invisible(strip_msg(3))
  expect_invisible(strip_msg("hello!"))

  # errors

  expect_error(strip_msg(strip_msg(stop(the_string), remove_spaces = NULL)),
               paste0("1 assertions failed Variable removespaces Must be of type l",
                      "ogical flag not NULL"),
               fixed = TRUE
  )

})
