
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xpectr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!--[![CRAN status](https://www.r-pkg.org/badges/version/xpectr)](https://CRAN.R-project.org/package=xpectr)-->
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

  - `strip()`
  - `strip_msg()`
  - `capture_side_effects()`
  - `capture_parse_eval_side_effects()`

## Addins

  - `Insert Expectations` : generates `testthat` `expect_*` tests from
    selected code
  - `dput() selected` : applies `dput()` to selected code
  - `Insert checkmate AssertCollection code` : inserts code for
    initializing and reporting a checkmate AssertCollection
  - `Wrap string with paste0` : splits selected string every n
    characters and wraps in `paste0` call
