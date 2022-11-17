## Test environments
* local OS X install, R 4.2.1
* ubuntu 14.04 (on travis-ci), R 3.6.3, 4.0.0 and devel
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

## Downstream dependencies
`revdepcheck::revdep_check()` reported no problems

## Notes on archival
The package was archived as I had accidentally hidden the email about the problems. It would be nice with an automated reminder the day before archival or similar. The problems, which were not problematic for users, should now be solved.

## Answers to feedback
Beni Altmann reviewed the code and had 2 points:

1) To not use \dontrun when unnecessary. But since the code in my examples either throw errors or generate and insert code (rstudio addins), they will fail in the check. So I have maintained them as dontrun.

2) Not to alter the global environment, as done by `capture_side_effects()`. I replied with the following arguments but did not hear back, so I'm resubmitting to get attention to this, in case you see a better approach. Changing it would likely **break** thousands of tests across packages. Arguments I sent to Beni:

"
Regarding the change in the global environment, I don't see an alternative. Here's the reasoning for it and why I think it is not problematic in this case:

`xpectr` is used to generate regression/unit tests for packages. That is, it is mainly intended for developers creating their test suite.

When a function is called, it may return a value AND throw a warning/message. In order to both test the value and warning/message, we need to run the code three times - once for catching the potential message, once for the potential warning and once for the result. But to ensure the same run, we need to use the same random seed all three times.

So the `capture_side_effects()` function grabs the existing random state (or creates one), sets it before each run, and then at the output of the function. That is, when leaving the function, the same random state (or none) is assigned to the global environment as when starting the function - and as such, the global environment is not actually changed by the function.

Now, if you have a way to do the same without altering the global env, I'm all ears of course :-) Cloning the environment seems quite expensive. Also, it does not seem to have been a problem for the couple of years it's been out.
"

