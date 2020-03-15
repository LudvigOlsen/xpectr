
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xpectr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/xpectr)](https://CRAN.R-project.org/package=xpectr)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/xpectr)](https://cran.r-project.org/package=xpectr)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![Codecov test
coverage](https://codecov.io/gh/ludvigolsen/xpectr/branch/master/graph/badge.svg)](https://codecov.io/gh/ludvigolsen/xpectr?branch=master)
[![Travis build
status](https://travis-ci.org/LudvigOlsen/xpectr.svg?branch=master)](https://travis-ci.org/LudvigOlsen/xpectr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/LudvigOlsen/xpectr?branch=master&svg=true)](https://ci.appveyor.com/project/LudvigOlsen/xpectr)
[![DOI](https://zenodo.org/badge/234099679.svg)](https://zenodo.org/badge/latestdoi/234099679)
<!-- badges: end -->

`xpectr` provides a set of utilities and RStudio addins for generating
tests for [`testthat`](https://testthat.r-lib.org/) unit testing.

**Author:** [Ludvig R. Olsen](http://ludvigolsen.dk/) (
<r-pkgs@ludvigolsen.dk> ) <br/> **Started:** January 2020

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

When developing R packages, it’s good practice to build a good set of
unit tests that will notify you when something breaks in the future. For
this, the [`testthat`](https://testthat.r-lib.org/) package is commonly
used. Often though, we end up writing a similar set of tests again and
again, which can be both tedious and time-consuming.

`xpectr` helps you by generating common `testthat` tests. Its goal is
**not** to replace the active thinking process of finding meaningful
tests for your functions, but to help **systematize** and **ease** that
process.

One such example is `gxs_function()`, which generates tests for a set of
argument value combinations. This allows you to focus on the selection
of argument values to test. Once the tests have been generated, you can
go through each of them to ensure your function is working as intended
and to add relevant tests.

Note: If you comment out the call to `gxs_function()`, it is easy to
later regenerate the tests. By using a diff tool, you can check that
only the intended changes have been made to the file.

## Call for feedback

While `xpectr` has already shown itself very useful in the scenarios
I’ve used it in, its usefulness is likely to multiply with the amount
of feedback by the community of package developers.

If you have *any* thoughts on the current set of tests and supported
objects, or ideas for improvements/additions, please open an issue or
send me a mail at <r-pkgs@ludvigolsen.dk>

## Installation

Install the CRAN version with:

``` r
install.packages("xpectr")
```

Install the development version with:

``` r
# install.packages("devtools")  
devtools::install_github("ludvigolsen/xpectr")
```

## Main functions

### Generator functions

These functions are used to *generate expectations*
(gxs).

| Function          | Description                                                                            |
| :---------------- | :------------------------------------------------------------------------------------- |
| `gxs_selection()` | Generates `testthat::expect_*` statements from a selection (string of code)            |
| `gxs_function()`  | Generates `testthat::expect_*` statements for combinations of supplied argument values |

### Functions for use in tests

| Function                                                    | Description                                                                  |
| :---------------------------------------------------------- | :--------------------------------------------------------------------------- |
| `strip()`                                                   | Strips strings of non-alphanumeric characters                                |
| `strip_msg()`                                               | Strips side-effect messages of non-alphanumeric characters and rethrows them |
| `suppress_mw()`                                             | Suppresses warnings and messages                                             |
| `capture_side_effects()`                                    | Captures errors, warnings, and messages from an expression                   |
| `smpl()`                                                    | Samples a vector, factor or data frame with internal random seed             |
| `simplified_formals()`                                      | Formats formals as easily testable character vector                          |
| `element_lengths()`, `element_types()`, `element_classes()` | Gets the length/type/class of each element                                   |
| `num_total_elements()`                                      | Unlists recursively and finds the total number of elements                   |
| `set_test_seed()`                                           | Set random seed for unit tests compatible with `R < 3.6.0`                   |

### Helper functions

| Function                                 | Description                                                               |
| :--------------------------------------- | :------------------------------------------------------------------------ |
| `prepare_insertion()`                    | Collapses a vector of expectation strings and adds indentation            |
| `capture_parse_eval_side_effects()`      | Wraps string in `capture_side_effects()` before parsing and evaluating it |
| `stop_if()`, `warn_if()`, `message_if()` | If `TRUE`, generate error/warning/message with the supplied message       |

## Addins

| Addin                                                                                            | Description                                                                       | Suggested Key Command |
| :----------------------------------------------------------------------------------------------- | :-------------------------------------------------------------------------------- | :-------------------- |
| *Insert Expectations* </br><font size="2">`insertExpectationsAddin()`</font>                     | Generates `testthat` `expect_*` tests from selected code (with `gxs_selection()`) | `Alt+E`               |
| *Initialize `test_that()`* </br><font size="2">`initializeTestthatAddin()`</font>                | Inserts `testthat::test_that()` code                                              | `Alt+T`               |
| *Initialize `gxs_function()`* </br><font size="2">`initializeGXSFunctionAddin()`</font>          | Initializes a `gxs_function()` call with default values of a function             | `Alt+F`               |
| *`dput()` selected* </br><font size="2">`dputSelectedAddin()`</font>                             | Applies `dput()` to selected code                                                 | `Alt+D`               |
| *Wrap string with `paste0()`* </br><font size="2">`wrapStringAddin()`</font>                     | Splits selected string every n characters and wraps in `paste0()` call            | `Alt+P`               |
| *Insert `checkmate` `AssertCollection`code* </br><font size="2">`assertCollectionAddin()`</font> | Inserts code for initializing and reporting a `checkmate` `AssertCollection`      | `Alt+C`               |

## Using in packages

Suggestion: Add `xpectr` in the `Suggests` field in the `DESCRIPTION`
file.

## Table of Contents

  - [xpectr](#xpectr)
      - [Call for feedback](#call-for-feedback)
      - [Installation](#installation)
      - [Main functions](#main-functions)
          - [Generator functions](#generator-functions)
          - [Functions for use in tests](#functions-for-use-in-tests)
          - [Helper functions](#helper-functions)
      - [Addins](#addins)
      - [Using in packages](#using-in-packages)
      - [Examples](#examples)
          - [gxs\_selection](#gxs_selection)
              - [Selection is a vector](#selection-is-a-vector)
                  - [Numeric vector](#numeric-vector)
                  - [Factor](#factor)
                  - [Long vector (sampling)](#long-vector-\(sampling\))
              - [Selection is a data frame](#selection-is-a-data-frame)
              - [Selection is a function call with side
                effects](#selection-is-a-function-call-with-side-effects)
          - [gxs\_function](#gxs_function)
          - [RStudio Addins](#rstudio-addins)
              - [How to set up a key command in
                RStudio](#how-to-set-up-a-key-command-in-rstudio)
              - [initializeGXSFunctionAddin](#initializegxsfunctionaddin)
              - [wrapStringAddin](#wrapstringaddin)
              - [initializeTestthatAddin](#initializetestthataddin)
              - [assertCollectionAddin](#assertcollectionaddin)
              - [dputSelectedAddin](#dputselectedaddin)

## Examples

``` r
library(xpectr)
library(testthat)
library(dplyr)

# Set a seed
# When R > 3.6.0, it sets sampling.kind to "Rounding" to make
# tests compatible with previous versions of R
set_test_seed(42)
```

``` r
# Some data
num_vec <- 1:10
long_vec <- c(LETTERS, letters)
a_factor <- factor(c("a","b","c"))

df <- data.frame(
  'a' = c(1, 2, 3),
  'b' = c('t', 'y', 'u'),
  "c" = a_factor,
  stringsAsFactors = FALSE
) %>% 
  dplyr::group_by(a)

# A function with side effects
fn <- function(raise = FALSE){
  message("Hi! I'm Kevin, your favorite message!")
  warning("G'Day Mam! I'm a warning to the world!")
  message("Kevin is ma name! Yesss!")
  warning("Hopefully the whole world will see me :o")
  if (isTRUE(raise)){
    stop("Lord Evil Error has arrived! Yeehaaa")
  }
  "the output"
}
```

### gxs\_selection

Note: `gxs_selection()` can be used with the `Insert Expectations`
addin. See `?insertExpectationsAddin` for instructions on how to set up
a key command.

#### Selection is a vector

##### Numeric vector

``` r
# Inspect num_vec
num_vec
#>  [1]  1  2  3  4  5  6  7  8  9 10
```

``` r
# Generate expectations
gxs_selection("num_vec")

# Inserts the following tests:

## Testing 'num_vec'                                                        ####
## Initially generated by xpectr
xpectr::set_test_seed(42)
# Testing class
expect_equal(
  class(num_vec),
  "integer",
  fixed = TRUE)
# Testing type
expect_type(
  num_vec,
  type = "integer")
# Testing values
expect_equal(
  num_vec,
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  tolerance = 1e-4)
# Testing names
expect_equal(
  names(num_vec),
  NULL,
  fixed = TRUE)
# Testing length
expect_equal(
  length(num_vec),
  10L)
# Testing sum of element lengths
expect_equal(
  sum(xpectr::element_lengths(num_vec)),
  10L)
## Finished testing 'num_vec'                                               ####
```

##### Factor

``` r
# Inspect a_factor
a_factor
#> [1] a b c
#> Levels: a b c
```

``` r
# Generate expectations
gxs_selection("a_factor")

# Inserts the following tests:

## Testing 'a_factor'                                                       ####
## Initially generated by xpectr
xpectr::set_test_seed(42)
# Testing is factor
expect_true(
  is.factor(a_factor))
# Testing values
expect_equal(
  as.character(a_factor),
  c("a", "b", "c"),
  fixed = TRUE)
# Testing names
expect_equal(
  names(a_factor),
  NULL,
  fixed = TRUE)
# Testing length
expect_equal(
  length(a_factor),
  3L)
# Testing number of levels
expect_equal(
  nlevels(a_factor),
  3L)
# Testing levels
expect_equal(
  levels(a_factor),
  c("a", "b", "c"),
  fixed = TRUE)
## Finished testing 'a_factor'                                              ####
```

##### Long vector (sampling)

By default, vectors with more than 30 elements will be sampled. This
adds `smpl()`, which temporarily sets a seed to make sure the same
elements are returned every time.

``` r
# Inspect long_vec
long_vec
#>  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
#> [20] "T" "U" "V" "W" "X" "Y" "Z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l"
#> [39] "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
```

``` r
# Generate expectations
gxs_selection("long_vec")

# Inserts the following tests:

## Testing 'long_vec'                                                       ####
## Initially generated by xpectr
xpectr::set_test_seed(42)
# Testing class
expect_equal(
  class(long_vec),
  "character",
  fixed = TRUE)
# Testing type
expect_type(
  long_vec,
  type = "character")
# Testing values
expect_equal(
  xpectr::smpl(long_vec, n = 30),
  c("C", "E", "G", "J", "K", "N", "O", "Q", "R", "S", "T", "W", "Y", 
    "Z", "b", "c", "d", "e", "h", "i", "j", "k", "l", "o", "p", 
    "r", "v", "w", "y", "z"),
  fixed = TRUE)
# Testing names
expect_equal(
  names(xpectr::smpl(long_vec, n = 30)),
  NULL,
  fixed = TRUE)
# Testing length
expect_equal(
  length(long_vec),
  52L)
# Testing sum of element lengths
expect_equal(
  sum(xpectr::element_lengths(long_vec)),
  52L)
## Finished testing 'long_vec'                                              ####
```

#### Selection is a data frame

Data frames are tested columnwise.

``` r
# Inspect df
df
#> # A tibble: 3 x 3
#> # Groups:   a [3]
#>       a b     c    
#>   <dbl> <chr> <fct>
#> 1     1 t     a    
#> 2     2 y     b    
#> 3     3 u     c
```

``` r
# Generate expectations
gxs_selection("df")

# Inserts the following tests:

## Testing 'df'                                                             ####
## Initially generated by xpectr
xpectr::set_test_seed(42)
# Testing class
expect_equal(
  class(df),
  c("grouped_df", "tbl_df", "tbl", "data.frame"),
  fixed = TRUE)
# Testing column values
expect_equal(
  df[["a"]],
  c(1, 2, 3),
  tolerance = 1e-4)
expect_equal(
  df[["b"]],
  c("t", "y", "u"),
  fixed = TRUE)
expect_equal(
  df[["c"]],
  structure(1:3, .Label = c("a", "b", "c"), class = "factor"))
# Testing column names
expect_equal(
  names(df),
  c("a", "b", "c"),
  fixed = TRUE)
# Testing column classes
expect_equal(
  xpectr::element_classes(df),
  c("numeric", "character", "factor"),
  fixed = TRUE)
# Testing column types
expect_equal(
  xpectr::element_types(df),
  c("double", "character", "integer"),
  fixed = TRUE)
# Testing dimensions
expect_equal(
  dim(df),
  c(3L, 3L))
# Testing group keys
expect_equal(
  colnames(dplyr::group_keys(df)),
  "a",
  fixed = TRUE)
## Finished testing 'df'                                                    ####
```

#### Selection is a function call with side effects

When the selected code generates an error, warning or message, we can
test those as well. An error is tested with `expect_error()`, while
messages and warnings are tested as character vectors, to make sure we
catch additional warnings/messages.

When running `testthat` unit tests on different systems, they sometimes
vary in the use of punctuation and newlines (\\n). The `strip()` and
`strip_msg()` functions are wrapped around the side effects to remove
non-alphanumeric symbols from the tested strings. This helps the
`expect_*` functions to ignore those differences. In cases where such
differences are important to catch, you can set `strip = FALSE` in
`gxs_selection()`.

We assign the output of the function to an `output_12345` variable, so
we don’t have to run it more than once. The number is randomly generated
and is **not** guaranteed to be unique. `suppress_mw()` suppresses the
messages and warnings.

``` r
# Inspect fn
fn
#> function(raise = FALSE){
#>   message("Hi! I'm Kevin, your favorite message!")
#>   warning("G'Day Mam! I'm a warning to the world!")
#>   message("Kevin is ma name! Yesss!")
#>   warning("Hopefully the whole world will see me :o")
#>   if (isTRUE(raise)){
#>     stop("Lord Evil Error has arrived! Yeehaaa")
#>   }
#>   "the output"
#> }
```

``` r
# Generate expectations
gxs_selection("fn()")

# Inserts the following tests:

## Testing 'fn()'                                                           ####
## Initially generated by xpectr
xpectr::set_test_seed(42)
# Testing side effects
# Assigning side effects
side_effects_19148 <- xpectr::capture_side_effects(fn(), reset_seed = TRUE)
expect_equal(
  xpectr::strip(side_effects_19148[['warnings']]),
  xpectr::strip(c("G'Day Mam! I'm a warning to the world!", "Hopefully the whole world will see me :o")),
  fixed = TRUE)
expect_equal(
  xpectr::strip(side_effects_19148[['messages']]),
  xpectr::strip(c("Hi! I'm Kevin, your favorite message!\n", "Kevin is ma name! Yesss!\n")),
  fixed = TRUE)
# Assigning output
output_19148 <- xpectr::suppress_mw(fn())
# Testing class
expect_equal(
  class(output_19148),
  "character",
  fixed = TRUE)
# Testing type
expect_type(
  output_19148,
  type = "character")
# Testing values
expect_equal(
  output_19148,
  "the output",
  fixed = TRUE)
# Testing names
expect_equal(
  names(output_19148),
  NULL,
  fixed = TRUE)
# Testing length
expect_equal(
  length(output_19148),
  1L)
# Testing sum of element lengths
expect_equal(
  sum(xpectr::element_lengths(output_19148)),
  1L)
## Finished testing 'fn()'                                                  ####

# In case of errors, the warnings and messages aren't tested

gxs_selection("fn(raise = TRUE)")

# Inserts the following tests:

## Testing 'fn(raise = TRUE)'                                               ####
## Initially generated by xpectr
xpectr::set_test_seed(42)
# Testing side effects
expect_error(
  xpectr::strip_msg(fn(raise = TRUE)),
  xpectr::strip("Lord Evil Error has arrived! Yeehaaa"),
  fixed = TRUE)
## Finished testing 'fn(raise = TRUE)'                                      ####
```

### gxs\_function

When testing the inputs to a function, `gxs_function()` allows us to
quickly specify values to check and generates tests for each of them.

The first value supplied for an argument is considered the *valid
baseline* value. For each argument, we create tests for each of the
supplied values, where the other arguments have their baseline value.

By default, each argument is tested with the `NULL` object as well, why
we only need to specify it when the baseline value should be `NULL`.

It is important that we manually read through the generated tests to
make sure that our function is behaving as intended, and to check if any
important tests are missing.

``` r
# Define a function with arguments
fn <- function(x, y, z = 10) {
  if (x > 3) stop("'x' > 3")
  if (y < 0) warning("'y'<0")
  if (z == 10) message("'z' was 10!")
  x + y + z
}

# Create tests for the function
# Note: We currently need to specify the list of arguments
# in the function call
gxs_function(fn = fn,
             args_values = list(
               "x" = list(2, 4, NA),
               "y" = list(0,-1),
               "z" = list(5, 10)
             ))

# Inserts the following tests:

## Testing 'fn'                                                             ####
## Initially generated by xpectr
# Testing different combinations of argument values

# Testing fn(x = 2, y = 0, z = 5)
xpectr::set_test_seed(42)
# Assigning output
output_19148 <- fn(x = 2, y = 0, z = 5)
# Testing class
expect_equal(
  class(output_19148),
  "numeric",
  fixed = TRUE)
# Testing type
expect_type(
  output_19148,
  type = "double")
# Testing values
expect_equal(
  output_19148,
  7,
  tolerance = 1e-4)
# Testing names
expect_equal(
  names(output_19148),
  NULL,
  fixed = TRUE)
# Testing length
expect_equal(
  length(output_19148),
  1L)
# Testing sum of element lengths
expect_equal(
  sum(xpectr::element_lengths(output_19148)),
  1L)

# Testing fn(x = 4, y = 0, z = 5)
# Changed from baseline: x = 4
xpectr::set_test_seed(42)
# Testing side effects
expect_error(
  xpectr::strip_msg(fn(x = 4, y = 0, z = 5)),
  xpectr::strip("'x' > 3"),
  fixed = TRUE)

# Testing fn(x = NA, y = 0, z = 5)
# Changed from baseline: x = NA
xpectr::set_test_seed(42)
# Testing side effects
expect_error(
  xpectr::strip_msg(fn(x = NA, y = 0, z = 5)),
  xpectr::strip("missing value where TRUE/FALSE needed"),
  fixed = TRUE)

# Testing fn(x = NULL, y = 0, z = 5)
# Changed from baseline: x = NULL
xpectr::set_test_seed(42)
# Testing side effects
expect_error(
  xpectr::strip_msg(fn(x = NULL, y = 0, z = 5)),
  xpectr::strip("argument is of length zero"),
  fixed = TRUE)

# Testing fn(x = 2, y = -1, z = 5)
# Changed from baseline: y = -1
xpectr::set_test_seed(42)
# Testing side effects
# Assigning side effects
side_effects_16417 <- xpectr::capture_side_effects(fn(x = 2, y = -1, z = 5), reset_seed = TRUE)
expect_equal(
  xpectr::strip(side_effects_16417[['warnings']]),
  xpectr::strip("'y'<0"),
  fixed = TRUE)
expect_equal(
  xpectr::strip(side_effects_16417[['messages']]),
  xpectr::strip(character(0)),
  fixed = TRUE)
# Assigning output
output_16417 <- xpectr::suppress_mw(fn(x = 2, y = -1, z = 5))
# Testing class
expect_equal(
  class(output_16417),
  "numeric",
  fixed = TRUE)
# Testing type
expect_type(
  output_16417,
  type = "double")
# Testing values
expect_equal(
  output_16417,
  6,
  tolerance = 1e-4)
# Testing names
expect_equal(
  names(output_16417),
  NULL,
  fixed = TRUE)
# Testing length
expect_equal(
  length(output_16417),
  1L)
# Testing sum of element lengths
expect_equal(
  sum(xpectr::element_lengths(output_16417)),
  1L)

# Testing fn(x = 2, y = NULL, z = 5)
# Changed from baseline: y = NULL
xpectr::set_test_seed(42)
# Testing side effects
expect_error(
  xpectr::strip_msg(fn(x = 2, y = NULL, z = 5)),
  xpectr::strip("argument is of length zero"),
  fixed = TRUE)

# Testing fn(x = 2, y = 0, z = 10)
# Changed from baseline: z = 10
xpectr::set_test_seed(42)
# Testing side effects
# Assigning side effects
side_effects_17365 <- xpectr::capture_side_effects(fn(x = 2, y = 0, z = 10), reset_seed = TRUE)
expect_equal(
  xpectr::strip(side_effects_17365[['warnings']]),
  xpectr::strip(character(0)),
  fixed = TRUE)
expect_equal(
  xpectr::strip(side_effects_17365[['messages']]),
  xpectr::strip("'z' was 10!\n"),
  fixed = TRUE)
# Assigning output
output_17365 <- xpectr::suppress_mw(fn(x = 2, y = 0, z = 10))
# Testing class
expect_equal(
  class(output_17365),
  "numeric",
  fixed = TRUE)
# Testing type
expect_type(
  output_17365,
  type = "double")
# Testing values
expect_equal(
  output_17365,
  12,
  tolerance = 1e-4)
# Testing names
expect_equal(
  names(output_17365),
  NULL,
  fixed = TRUE)
# Testing length
expect_equal(
  length(output_17365),
  1L)
# Testing sum of element lengths
expect_equal(
  sum(xpectr::element_lengths(output_17365)),
  1L)

# Testing fn(x = 2, y = 0, z = NULL)
# Changed from baseline: z = NULL
xpectr::set_test_seed(42)
# Testing side effects
expect_error(
  xpectr::strip_msg(fn(x = 2, y = 0, z = NULL)),
  xpectr::strip("argument is of length zero"),
  fixed = TRUE)

## Finished testing 'fn'                                                    ####
```

### RStudio Addins

Below, we present the set of `RStudio` addins. The intention is for you
to set up key commands for the ones you’d like access to. Once you get
used to using them, they will speed up your testing process.

#### How to set up a key command in RStudio

Here’s a small guide for setting up key comands in `RStudio`. We use the
`Insert Expectations` addin as an example:

After installing the package, go to:

`Tools >> Addins >> Browse Addins >> Keyboard Shortcuts`.

Find `"Insert Expectations"` and press its field under `Shortcut`.

Press desired key command, e.g. `Alt+E`.

Press `Apply`.

Press `Execute`.

#### initializeGXSFunctionAddin

The `initializeGXSFunctionAddin` initializes the `gxs_function()` call
with the argument names and default values of a selected function. We
can then add the argument values and additional combinations we wish to
test. Note, that we don’t need to use the default values as our baseline
values.

The `Tip` comment tells us to comment out the `gxs_function()` call
after running it, instead of removing it. When you change your code,
it’s often much quicker to regenerate the tests than to update them
manually. You can then use a diff tool to check that only the intended
changes were made.

The `#` in the end helps the code be inserted right after the call to
`gxs_function()`.

Suggested keycommand: `Alt+F`

``` r
initializeGXSFunctionAddin("fn")

# Inserts the following:

# Generate expectations for 'fn'
# Tip: comment out the gxs_function() call
# so it is easy to regenerate the tests
xpectr::set_test_seed(42)
xpectr::gxs_function(
  fn = fn,
  args_values = list(
    "x" = list(),
    "y" = list(),
    "z" = list(10)
  )
)

#
```

#### wrapStringAddin

The `wrapStringAddin` splits long strings and wraps them with
`paste0()`.

Suggested keycommand:
`Alt+P`

``` r
wrapStringAddin("This is a fairly long sentence that we would very very much like to make shorter in our test file!")

# Inserts the following:

paste0("This is a fairly long sentence that we would very very much ",
       "like to make shorter in our test file!")
```

#### initializeTestthatAddin

Suggested keycommand: `Alt+T`

``` r
initializeTestthatAddin()

# Inserts the following:

test_that("testing ...()", {
  xpectr::set_test_seed(42)

  # ...

})
```

#### assertCollectionAddin

Suggested keycommand: `Alt+C`

``` r
assertCollectionAddin()

# Inserts the following:

# Check arguments ####
assert_collection <- checkmate::makeAssertCollection()
# checkmate::assert_ , add = assert_collection)
checkmate::reportAssertions(assert_collection)
# End of argument checks ####
```

#### dputSelectedAddin

Suggested keycommand: `Alt+D`

``` r
v <- c(1, 2, 3)

dputSelectedAddin("v")  # "v" is the selected code

# Inserts the following:

c(1, 2, 3)
```
