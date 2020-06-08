

#   __________________ #< d19c97fa8ab3c13382eba03991b20184 ># __________________
#   Extract and simplify formals                                            ####


#' @title Extract and simplify a function's formal arguments
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Extracts \code{\link[base:formals]{formals}} and
#'  formats them as an easily testable \code{character vector}.
#' @param fn \code{Function}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return A \code{character vector} with the simplified formals.
#' @export
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' fn1 <- function(a = "x", b = NULL, c = NA, d){
#'   paste0(a, b, c, d)
#' }
#'
#' simplified_formals(fn1)
simplified_formals <- function(fn) {
  # Extract formals
  frmls <- formals(fn)

  # Simplify formals
  char_formals <- plyr::llply(names(frmls), function(name){
    # Convert default value to character
    if (is.character(frmls[[name]])){
      val_char <- escape_metacharacters(frmls[[name]])
      val_char <- add_quotation_marks(val_char)
    } else {
      val_char <- as.character(frmls[[name]])
    }

    # Format formal
    if (is.null(frmls[[name]])){
      out <- paste0(name, " = NULL")
    } else if (checkmate::test_string(val_char) &&  # avoid errors on NAs etc.
               nchar(val_char) == 0) {
      out <- name
    } else {
      out <- paste0(name, " = ", val_char)
    }
    out
  }) %>% unlist()

  char_formals
}
