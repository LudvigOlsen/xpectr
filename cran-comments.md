## Test environments
* local OS X install, R 4.2.1
* ubuntu 14.04 (on travis-ci), R 3.6.3, 4.0.0 and devel
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

## Downstream dependencies
`revdepcheck::revdep_check()` reported no problems

## Previous (relevant) comments

 - \dontrun: In some cases I use \dontrun as my examples either throw errors or generate and insert code (rstudio addins).
 
 - Altering the global environment in `capture_side_effects()`: `xpectr` is used to generate regression/unit tests for packages. That is, it is mainly intended for developers creating their test suite. When a function is called, it may return a value AND throw a warning/message. In order to both test the value and warning/message, we need to run the code three times - once for catching the potential message, once for the potential warning and once for the result. But to ensure the same run, we need to use the same random seed all three times. So the `capture_side_effects()` function grabs the existing random state (or creates one), sets it before each run, and then at the output of the function. That is, when leaving the function, the same random state (or none) is assigned to the global environment as when starting the function - and as such, the global environment is not actually changed by the function. I do not see an alternative to this approach, nor do I think it is problematic in practice.
