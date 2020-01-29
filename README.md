
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xpectr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/xpectr)](https://CRAN.R-project.org/package=xpectr)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/xpectr)](https://cran.r-project.org/package=xpectr)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.2.0-6666ff.svg)](https://cran.r-project.org/)
[![Codecov test
coverage](https://codecov.io/gh/ludvigolsen/xpectr/branch/master/graph/badge.svg)](https://codecov.io/gh/ludvigolsen/xpectr?branch=master)
[![Travis build
status](https://travis-ci.org/LudvigOlsen/xpectr.svg?branch=master)](https://travis-ci.org/LudvigOlsen/xpectr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/LudvigOlsen/xpectr?branch=master&svg=true)](https://ci.appveyor.com/project/LudvigOlsen/xpectr)
[![DOI](https://zenodo.org/badge/234099679.svg)](https://zenodo.org/badge/latestdoi/234099679)
<!-- badges: end -->

`xpectr` provides a set of utilities and RStudio addins for generating
tests for testthat unit testing.

**Author:** [Ludvig R. Olsen](http://ludvigolsen.dk/) (
<r-pkgs@ludvigolsen.dk> ) <br/> **Started:** January 2020

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Installation

You can install the development version with:

``` r
devtools::install_github("ludvigolsen/xpectr")
```

## Main functions

### Generator functions

Note: `gxs` stands for `generate expectations`.

  - `gxs_selection()`
  - `gxs_function()`

### Functions for use in tests

  - `strip()`
  - `strip_msg()`
  - `suppress_mw()`
  - `smpl()`
  - `simplified_formals()`
  - `element_lengths()`, `element_types()`, `element_classes()`
  - `num_total_elements()`
  - `set_test_seed()`

### Helper functions

  - `prepare_insertion()`
  - `capture_side_effects()`
  - `capture_parse_eval_side_effects()`
  - `stop_if()`, `warn_if()`, `message_if()`

## Addins

  - `Insert Expectations` : generates `testthat` `expect_*` tests from
    selected code
  - `dput() selected` : applies `dput()` to selected code
  - `Insert checkmate AssertCollection code` : inserts code for
    initializing and reporting a checkmate AssertCollection
  - `Wrap string with paste0` : splits selected string every n
    characters and wraps in `paste0` call

## Using in packages

It is suggested to add `xpectr` in the `Suggests` field in the
`DESCRIPTION` file. This allows you to use it in unit tests and users
aren’t required to have it installed.

NOTE: This won’t work on CRAN before it is released there.

## Table of Contents

  - [xpectr](#xpectr)
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
                  - [Character vector](#character-vector)
                  - [Factor](#factor)
                  - [Long vector (currently \> 30 elements) adds
                    `smpl()`](#long-vector)
              - [Selection is a data frame](#selection-is-a-data-frame)
              - [Selection is a function call with side
                effects](#selection-is-a-function-call-with-side-effects)
          - [gxs\_function](#gxs_function)
          - [wrapStringAddin](#wrapstringaddin)

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
char_vec <- head(LETTERS, 10)
long_vec <- 1:40
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

##### Character vector

``` r
# Inspect char_vec
char_vec
#>  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
```

``` r
# Generate expectations
gxs_selection("char_vec")

# Inserts the following tests:

## Testing 'char_vec'                                                       ####
## Initially generated by xpectr
xpectr::set_test_seed(42)
# Testing class
expect_equal(
  class(char_vec),
  "character",
  fixed = TRUE)
# Testing type
expect_type(
  char_vec,
  type = "character")
# Testing values
expect_equal(
  char_vec,
  c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  fixed = TRUE)
# Testing names
expect_equal(
  names(char_vec),
  NULL,
  fixed = TRUE)
# Testing length
expect_equal(
  length(char_vec),
  10L)
# Testing sum of element lengths
expect_equal(
  sum(xpectr::element_lengths(char_vec)),
  10L)
## Finished testing 'char_vec'                                              ####
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

##### Long vector (currently \> 30 elements) adds `smpl()`

``` r
# Inspect long_vec
long_vec
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
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
  "integer",
  fixed = TRUE)
# Testing type
expect_type(
  long_vec,
  type = "integer")
# Testing values
expect_equal(
  xpectr::smpl(long_vec, n = 30),
  c(2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 17, 18, 19, 21, 22, 23,
    24, 25, 26, 27, 29, 30, 31, 32, 35, 36, 37, 38, 40),
  tolerance = 1e-4)
# Testing names
expect_equal(
  names(xpectr::smpl(long_vec, n = 30)),
  NULL,
  fixed = TRUE)
# Testing length
expect_equal(
  length(long_vec),
  40L)
# Testing sum of element lengths
expect_equal(
  sum(xpectr::element_lengths(long_vec)),
  40L)
## Finished testing 'long_vec'                                              ####
```

#### Selection is a data frame

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

Note the `strip` and `strip_msg` functions that are wrapped around the
messages and side effects. These remove non-alphanumeric symbols from
the tested strings. When running `testthat` unit tests on different
systems, they sometimes vary in the use of punctuation and newlines
(\\n). These functions will help the `expect_*` function to ignore that.
In case such differences are important to catch, you can set `strip =
FALSE` in `gxs_selection()`.

We assign the output of the function to an `output_xxxxx` variable, so
we don’t have to run it more than once.

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
side_effects_19148 <- xpectr::capture_side_effects(fn())
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

Based on a set of supplied values for each function argument, a set of
`testthat` `expect_*` statements are generated.

The first value supplied for an argument is considered the *valid
baseline* value. For each argument, we create tests for each of the
supplied values, where the other arguments have their baseline value. It
automatically adds `NULL` to the list of values to check for each
argument.

``` r
# Define a function with arguments
fn <- function(x, y, z) {
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
# Changed from baseline: x
xpectr::set_test_seed(42)
# Testing side effects
expect_error(
  xpectr::strip_msg(fn(x = 4, y = 0, z = 5)),
  xpectr::strip("'x' > 3"),
  fixed = TRUE)

# Testing fn(x = NA, y = 0, z = 5)
# Changed from baseline: x
xpectr::set_test_seed(42)
# Testing side effects
expect_error(
  xpectr::strip_msg(fn(x = NA, y = 0, z = 5)),
  xpectr::strip("missing value where TRUE/FALSE needed"),
  fixed = TRUE)

# Testing fn(x = NULL, y = 0, z = 5)
# Changed from baseline: x
xpectr::set_test_seed(42)
# Testing side effects
expect_error(
  xpectr::strip_msg(fn(x = NULL, y = 0, z = 5)),
  xpectr::strip("argument is of length zero"),
  fixed = TRUE)

# Testing fn(x = 2, y = -1, z = 5)
# Changed from baseline: y
xpectr::set_test_seed(42)
# Testing side effects
# Assigning side effects
side_effects_16417 <- xpectr::capture_side_effects(fn(x = 2, y = -1, z = 5))
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
# Changed from baseline: y
xpectr::set_test_seed(42)
# Testing side effects
expect_error(
  xpectr::strip_msg(fn(x = 2, y = NULL, z = 5)),
  xpectr::strip("argument is of length zero"),
  fixed = TRUE)

# Testing fn(x = 2, y = 0, z = 10)
# Changed from baseline: z
xpectr::set_test_seed(42)
# Testing side effects
# Assigning side effects
side_effects_17365 <- xpectr::capture_side_effects(fn(x = 2, y = 0, z = 10))
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
# Changed from baseline: z
xpectr::set_test_seed(42)
# Testing side effects
expect_error(
  xpectr::strip_msg(fn(x = 2, y = 0, z = NULL)),
  xpectr::strip("argument is of length zero"),
  fixed = TRUE)

## Finished testing 'fn'                                                    ####
```

### wrapStringAddin

This addin splits long strings and wraps them with
`paste0()`.

``` r
wrapStringAddin("This is a fairly long sentence that we would very very much like to make shorter in our test file!")

# Inserts the following:

paste0("This is a fairly long sentence that we would very very much ",
       "like to make shorter in our test file!")
```
