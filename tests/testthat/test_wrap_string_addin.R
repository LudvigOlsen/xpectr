library(xpectr)
context("wrapStringAddin()")

test_that("wrapStringAddin() works", {

  s1 <- paste0("\\fdjslf fdsnm dshfjskdh \\ sdjkfshk\\usjdfu fdsjhksdfkjh",
               "\\asjhdakjshfjd fdshkjh jkhdjfksdh kahsjkdja\\\\\\")

  expect_equal(split_string_every(s1, per = 60),
               c("\\fdjslf fdsnm dshfjskdh \\ sdjkfshk\\usjdfu fdsjhksdfkjh\\asjhd",
                 "akjshfjd fdshkjh jkhdjfksdh kahsjkdja\\\\\\"))

  expect_equal(split_to_paste0(s1, per = 60),
               paste0("paste0(\"\\fdjslf fdsnm dshfjskdh \\ sdjkfshk\\usjdfu fdsjh",
                      "ksdfkjh\\asjhd\",\n         \"akjshfjd fdshkjh jkhdjfksdh ka",
                      "hsjkdja\\\\\\\")"))

  expect_equal(wrapStringAddin(selection=s1, insert = FALSE),
               paste0("paste0(\"\\fdjslf fdsnm dshfjskdh \\ sdjkfshk\\usjdfu fdsjh",
                      "ksdfkjh\\asjhd\",\n       \"akjshfjd fdshkjh jkhdjfksdh kahs",
                      "jkdja\\\\\\\")"))

  s2 <- paste0("\\fdjslf fdsnm dshfjskdh \\ sdjkfshk\\usjdfu fdsjhksdfkjh",
               "\\asjh\\dakjshfjdkahsjkdja\\\\\\")

  expect_equal(split_string_every(s2, per = 60),
               c("\\fdjslf fdsnm dshfjskdh \\ sdjkfshk\\usjdfu fdsjhksdfkjh\\asjh",
                 "\\dakjshfjdkahsjkdja\\\\\\"))

  # TODO check wrapStringAddin caps correctly at 50 and 70 and takes indentation into account!

})
