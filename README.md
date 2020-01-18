
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xpectr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/xpectr)](https://CRAN.R-project.org/package=xpectr)
<!-- badges: end -->

`xpectr` provides a set of utilities and RStudio addins for generating
tests for testthat unittesting.

**Author:** [Ludvig R. Olsen](http://ludvigolsen.dk/) (
<r-pkgs@ludvigolsen.dk> ) <br/> **Started:** January 2020

## Installation

You can install the development version with:

``` r
devtools::install_github("ludvigolsen/xpectr")
```

## Main functions

Note: `gxs` stands for `generate expectations`.

  - `gxs_selection()`
  - `gxs_function()`
  - `strip()`
  - `strip_msg()`
  - `smpl()`
  - `set_seed()`
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

## Examples

``` r
library(xpectr)
library(testthat)

# Set a seed
# When R > 3.6.0 it sets sampling.kind to "Rounding" to make
# tests compatible with previous versions of R
set_seed(42)
```

``` r
# Some data

num_vec <- 1:10
char_vec <- head(LETTERS, 10)
long_vec <- 1:40

df <- data.frame(
  'a' = c(1, 2, 3),
  'b' = c('t', 'y', 'u'),
  stringsAsFactors = FALSE
)

# A function with side effects
fn <- function(raise = FALSE){
  message("Hi! I'm Kevin, your favorite message!")
  warning("G'Day Mam! I'm a warning to the world!")
  message("Kevin is ma name! Yesss!")
  warning("Hopefully the whole world will see me :o")
  if (isTRUE(raise)){
    stop("Lord Evil Error has arrived! Yeehaaa")
  }
}
```

### `gxs_selection()`

Note: `gxs_selection()` can be used with the `Insert Expectations`
addin. See `?insertExpectationsAddin` for instructions on how to set up
a key command.

#### Selection is a vector

Numeric vector:

``` r
gxs_selection("num_vec")

# Inserts the following tests:

expect_equal(
  num_vec,
  1:10,
  tolerance = 1e-4)
```

Character vector:

``` r
gxs_selection("char_vec")

# Inserts the following tests:

expect_equal(
  char_vec,
  c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  fixed = TRUE)
```

Long vector (currently \> 30 elements) adds `smpl()`:

``` r
gxs_selection("long_vec")

# Inserts the following tests:

expect_equal(
  xpectr::smpl(long_vec, n = 30),
  c(2L, 3L, 5L, 6L, 7L, 8L, 10L, 11L, 12L, 13L, 14L, 17L, 18L, 19L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 29L, 30L, 31L, 32L, 35L, 36L, 37L, 38L, 40L),
  tolerance = 1e-4)
```

#### Selection is a data frame

``` r
gxs_selection("df")

# Inserts the following tests:

expect_equal(
  df[["a"]],
  c(1, 2, 3),
  tolerance = 1e-4)
expect_equal(
  df[["b"]],
  c("t", "y", "u"),
  fixed = TRUE)
expect_equal(
  names( df ),
  c("a", "b"),
  fixed = TRUE)
```

#### Selection is a function with side effects

Note that the `strip_msg` and `strip` functions are wrapped around the
side effect and the message. These remove non-alphanumeric symbols from
the tested strings. When running `testthat` unit tests on different
systems, they sometimes vary in the use of punctuation and newlines
(\\n). These functions will help the `expect_*` function to ignore that.
In case such differences are important to catch, you can set `strip =
FALSE` in `gxs_selection()`.

``` r
gxs_selection("fn()")

# Inserts the following tests:

expect_warning(
  xpectr::strip_msg(fn()),
  xpectr::strip("G'Day Mam! I'm a warning to the world!"),
  fixed = TRUE)
expect_warning(
  xpectr::strip_msg(fn()),
  xpectr::strip("Hopefully the whole world will see me :o"),
  fixed = TRUE)
expect_message(
  xpectr::strip_msg(fn()),
  xpectr::strip("Hi! I'm Kevin, your favorite message!\n"),
  fixed = TRUE)
expect_message(
  xpectr::strip_msg(fn()),
  xpectr::strip("Kevin is ma name! Yesss!\n"),
  fixed = TRUE)

# In case of errors, the warnings and messages aren't tested

gxs_selection("fn(raise = TRUE)")

# Inserts the following tests:

expect_error(
  xpectr::strip_msg(fn(raise = TRUE)),
  xpectr::strip("Lord Evil Error has arrived! Yeehaaa"),
  fixed = TRUE)
```

### `gxs_function()`

Based on a set of supplied values for each function argument, a set of
`testthat` `expect_*` statements are generated.

The first value supplied for an argument is considered the *valid
baseline* value. For each argument, we create tests for each of the
supplied values, where the other arguments have their baseline value. It
automatically adds `NULL` to the list of values to check for each
argument.

``` r
# Define a function with arguments
fn <- function(x, y, z){
  if (x>3) stop("'x' > 3")
  if (y<0) warning("'y'<0")
  if (z==10) message("'z' was 10!")
  x + y + z
}

# Create tests for the function
# Note: We currently need to specify the list of arguments
# in the function call
gxs_function(fn = fn,
             args_values = list(
               "x" = list(2, 4, NA),
               "y" = list(0,-1),
               "z" = list(5, 10, 15)
             ))

# Inserts the following tests:

expect_error(
  xpectr::strip_msg(fn(x = 4, y = 0, z = 5)),
  xpectr::strip("'x' > 3"),
  fixed = TRUE)
expect_error(
  xpectr::strip_msg(fn(x = NA, y = 0, z = 5)),
  xpectr::strip("missing value where TRUE/FALSE needed"),
  fixed = TRUE)
expect_warning(
  xpectr::strip_msg(fn(x = 2, y = -1, z = 5)),
  xpectr::strip("'y'<0"),
  fixed = TRUE)
expect_message(
  xpectr::strip_msg(fn(x = 2, y = 0, z = 10)),
  xpectr::strip("'z' was 10!\n"),
  fixed = TRUE)
expect_equal(
  fn(x = 2, y = 0, z = 15),
  17,
  tolerance = 1e-4)
expect_equal(
  fn(x = 2, y = 0, z = 5),
  7,
  tolerance = 1e-4)
expect_error(
  xpectr::strip_msg(fn(x = NULL, y = 0, z = 5)),
  xpectr::strip("argument is of length zero"),
  fixed = TRUE)
expect_error(
  xpectr::strip_msg(fn(x = 2, y = NULL, z = 5)),
  xpectr::strip("argument is of length zero"),
  fixed = TRUE)
expect_error(
  xpectr::strip_msg(fn(x = 2, y = 0, z = NULL)),
  xpectr::strip("argument is of length zero"),
  fixed = TRUE)
```

### `wrapStringAddin()`

This addin splits long strings and wraps them with
`paste0()`.

``` r
wrapStringAddin("This is a fairly long sentence that we would very very much like to make shorter in our test file!")

# Inserts the following:

paste0("This is a fairly long sentence that we would very very much ",
       "like to make shorter in our test file!")
```
