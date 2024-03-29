% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gxs_function.R
\name{gxs_function}
\alias{gxs_function}
\title{Generate testhat expectations for argument values in a function}
\usage{
gxs_function(
  fn,
  args_values,
  extra_combinations = NULL,
  check_nulls = TRUE,
  indentation = 0,
  tolerance = "1e-4",
  round_to_tolerance = TRUE,
  strip = TRUE,
  sample_n = 30,
  envir = NULL,
  copy_env = FALSE,
  assign_output = TRUE,
  seed = 42,
  add_wrapper_comments = TRUE,
  add_test_comments = TRUE,
  start_with_newline = TRUE,
  end_with_newline = TRUE,
  out = "insert",
  parallel = FALSE
)
}
\arguments{
\item{fn}{Function to create tests for.}

\item{args_values}{The arguments and the values to create tests for.
Should be supplied as a named list of lists, like the following:

\code{args_values = list(}

\code{"x1" = list(1, 2, 3), }

\code{"x2" = list("a", "b", "c")}

\code{)}

The first value for each argument (referred to as the 'baseline' value) should be valid
(not throw an \code{error/}\code{message}/\code{warning}).

\strong{N.B.} This is not checked but should lead to more meaningful tests.

\strong{N.B.} Please define the list directly in the function call.
This is currently necessary.}

\item{extra_combinations}{Additional combinations to test. List of lists, where
each combination is a named sublist.

E.g. the following two combinations:

\code{extra_combinations = list(}

\code{list("x1" = 4, "x2" = "b"),}

\code{list("x1" = 7, "x2" = "c")}

\code{)}

\strong{N.B.} Unspecified arguments gets the baseline value.

If you find yourself adding many combinations,
an additional \code{gxs_function()} call with different baseline values
might be preferable.}

\item{check_nulls}{Whether to try all arguments with \code{NULL}. (Logical)

When enabled, you don't need to add \code{NULL} to your \code{`args_values`},
unless it should be the baseline value.}

\item{indentation}{Indentation of the selection. (Numeric)}

\item{tolerance}{The tolerance for numeric tests as a string, like \code{"1e-4"}. (Character)}

\item{round_to_tolerance}{Whether to round numeric elements to the specified tolerance. (Logical)

This is currently applied to numeric columns and vectors (excluding some lists).}

\item{strip}{Whether to insert
\code{\link[xpectr:strip]{strip_msg()}} and
\code{\link[xpectr:strip]{strip()}}
in tests of side effects. (Logical)

Sometimes testthat tests have differences in punctuation and newlines on different
systems. By stripping both the error message and the expected message of non-alphanumeric symbols,
we can avoid such failed tests.}

\item{sample_n}{The number of elements/rows to sample. Set to \code{NULL} to avoid sampling.

Inserts \code{\link[xpectr:strip]{smpl()}} in the generated tests when sampling was used. A seed is
set internally, setting \code{sample.kind} as \code{"Rounding"} to ensure compatibility with \code{R} versions
\code{< 3.6.0}.

The order of the elements/rows is kept intact. No replacement is used, why no oversampling will
take place.

When testing a big \code{data.frame}, sampling the rows can help keep the test files somewhat readable.}

\item{envir}{Environment to evaluate in. Defaults to
\code{\link[base:sys.parent]{parent.frame()}}.}

\item{copy_env}{Whether each combination should be tested in a deep copy of the environment. (Logical)

Side effects will be captured in copies of the copy, why two copies of the environment will
exist at the same time.

Disabled by default to save memory but is often preferable to enable,
e.g. when the function changes non-local variables.}

\item{assign_output}{Whether to assign the output of a function call or long selection
to a variable. This will avoid recalling the function and decrease cluttering. (Logical)

Heuristic: when the \code{`selection`} isn't of a string and contains a parenthesis, it is considered a function call.
A selection with more than 30 characters will be assigned as well.

The tests themselves can be more difficult to interpret, as you will
have to look at the assignment to see the object that is being tested.}

\item{seed}{\code{seed} to set. (Whole number)}

\item{add_wrapper_comments}{Whether to add intro and outro comments. (Logical)}

\item{add_test_comments}{Whether to add comments for each test. (Logical)}

\item{start_with_newline, end_with_newline}{Whether to have a newline in the beginning/end. (Logical)}

\item{out}{Either \code{"insert"} or \code{"return"}.

\subsection{"insert" (Default)}{
Inserts the expectations via
\code{\link[rstudioapi:rstudio-documents]{rstudioapi::insertText()}}.
}
\subsection{"return"}{
Returns the expectations in a \code{list}.

These can be prepared for insertion with
\code{\link[xpectr:prepare_insertion]{prepare_insertion()}}.
}}

\item{parallel}{Whether to parallelize the generation of expectations. (Logical)

Requires a registered parallel backend. Like with \code{doParallel::registerDoParallel}.}
}
\value{
Either \code{NULL} or the unprepared expectations as a \code{character vector}.
}
\description{
\Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}

Based on a set of supplied values for each function argument,
a set of \code{testthat} \code{expect_*} statements are generated.

\strong{Included tests}: The first value supplied for an argument
is considered the \emph{valid baseline} value. For each argument, we
create tests for each of the supplied values, where the other arguments
have their baseline value.

When testing a function that alters non-local variables, consider enabling \code{`copy_env`}.

See supported objects in \code{details}.
}
\details{
The following "types" are currently supported or intended to be supported in the future.
Please suggest more types and tests in a GitHub issue!

Note: A set of fallback tests will be generated for unsupported objects.

\tabular{rrr}{
\strong{Type} \tab \strong{Supported} \tab \strong{Notes} \cr
Side effects \tab Yes \tab Errors, warnings, and messages. \cr
Vector \tab Yes \tab Lists are treated differently, depending on their structure. \cr
Factor \tab Yes \tab \cr
Data Frame \tab Yes \tab List columns (like nested tibbles) are currently skipped. \cr
Matrix \tab Yes \tab Supported but could be improved. \cr
Formula \tab Yes \tab \cr
Function \tab Yes \tab \cr
\code{NULL} \tab Yes \tab \cr
Array \tab No \tab \cr
Dates \tab No \tab Base and \code{lubridate}. \cr
ggplot2 \tab No \tab This may be a challenge, but would be cool!\cr
}
}
\examples{
# Attach packages
library(xpectr)

\dontrun{
fn <- function(x, y, z){
  if (x>3) stop("'x' > 3")
  if (y<0) warning("'y'<0")
  if (z==10) message("'z' was 10!")
  x + y + z
}

# Create expectations
# Note: define the list in the call
gxs_function(fn,
             args_values = list(
               "x" = list(2, 4, NA),
               "y" = list(0, -1),
               "z" = list(5, 10))
             )

# Add additional combinations
gxs_function(fn,
             args_values = list(
               "x" = list(2, 4, NA),
               "y" = list(0, -1),
               "z" = list(5, 10)),
             extra_combinations = list(
               list("x" = 4, "z" = 10),
               list("y" = 1, "z" = 10))
             )
}
}
\seealso{
Other expectation generators: 
\code{\link{gxs_selection}()},
\code{\link{initializeGXSFunctionAddin}()},
\code{\link{insertExpectationsAddin}()}
}
\author{
Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
}
\concept{expectation generators}
