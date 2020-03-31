# xpectr 0.3.0

* Breaking: `element_classes()` only returns the first class string per element.

* Adds `navigateTestFileAddin()` addin for opening test file from string name. E.g. when running `testthat` in the build window, copy the filename and line number of the failed test, e.g. `test_x.R:5:`, and run the addin. It will then open `/tests/testthat/test_x.R` at line `5`.

* Adds test of `class` attribute for errors. 

* Extracts error message with `conditionMessage()` instead of `$message`.

* Adds `indentation = 2` to generated code from `initializeGXSFunctionAddin()`.

* Bug fix: Does not check symmetry for `table` objects.

# xpectr 0.2.0

* Adds `initializeGXSFunctionAddin()` addin for initializing a `gxs_function()` call for a selected function.

* Adds `initializeTestthatAddin()` addin for inserting `testthat::test_that()` chunk.

* `gxs_function()` gets argument `extra_combinations` for manually adding extra combinations of argument values. In some simple cases, this can help us avoid multiple calls to `gxs_function()` with different baseline values.

* The `Changed from baseline:` comment adds the changed value when only one argument was changed. This makes it faster to see what is tested.

* Tests are now properly ordered as the specified `args_values`. 

# xpectr 0.1.1

* `capture_side_effects()` gains argument `reset_seed`. Whether to reset the random state on exit (default: `FALSE`).

* Bug fix: When generating expectations for expressions with warnings and/or messages, the random state is reset after capturing them, before evaluating the expression for its output.

* Bug fix: Escapes quotation marks in error messages.

* All `data.frame()` calls explicitly sets `stringsAsFactors` to ensure compatibility with `r-devel`.

# xpectr 0.1.0

* Created package :)  

* Main functions are `gxs_selection()` and `gxs_function()`.
