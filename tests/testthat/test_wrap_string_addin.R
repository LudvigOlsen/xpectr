library(xpectr)
context("wrapStringAddin()")

test_that("wrapStringAddin() works", {

  s1 <- paste0("\\fdjslf fdsnm dshfjskdh \\ sdjkfshk\\usjdfu fdsjhksdfkjh",
               "\\asjhdakjshfjdkahsjkdja\\\\\\")

  expect_equal(split_string_every(s1, per = 60),
               c("\\fdjslf fdsnm dshfjskdh \\ sdjkfshk\\usjdfu fdsjhksdfkjh\\asjhd",
                 "akjshfjdkahsjkdja\\\\\\"))

  expect_equal(split_to_paste0(s1, per = 60),
               paste0("paste0(\"\\fdjslf fdsnm dshfjskdh \\ sdjkfshk\\usjdfu fdsjh",
                      "ksdfkjh\\asjhd\",\n         \"akjshfjdkahsjkdja\\\\\\\")"))

  expect_equal(wrapStringAddin(selection=s1, insert = FALSE),
               paste0("paste0(\"\\fdjslf fdsnm dshfjskdh \\ sdjkfshk\\usjdfu fdsjh",
                      "ksdfkjh\\asjhd\",\n       \"akjshfjdkahsjkdja\\\\\\\")"))

  s2 <- paste0("\\fdjslf fdsnm dshfjskdh \\ sdjkfshk\\usjdfu fdsjhksdfkjh",
               "\\asjh\\dakjshfjdkahsjkdja\\\\\\")

  expect_equal(split_string_every(s2, per = 60),
               c("\\fdjslf fdsnm dshfjskdh \\ sdjkfshk\\usjdfu fdsjhksdfkjh\\asjh",
                 "\\dakjshfjdkahsjkdja\\\\\\"))

})
