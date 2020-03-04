# xpectr 0.1.0.9000

* `capture_side_effects()` gains argument `reset_seed`. Whether to reset the random state on exit (default: `FALSE`).

* Bug fix: When generating expectations for expressions with warnings and/or messages, the random state is reset after capturing them, before evaluating the expression for its output.

# xpectr 0.1.0

* Created package :)  

* Main functions are `gxs_selection()` and `gxs_function()`.
