#' Normalize Variable Names
#'
#' Normalize variable names
#'
#' @param vars chr vector
#' @param sep a character string to separate the terms
#'
#' @export
#'
#' @details A function used to normalize unusual text formats.
#'
#' @references Williams, G. J. (2011), Data Mining with Rattle and R: The Art of Excavating Data for Knowledge Discovery, Use R!, Springer.
#' @return Returns a character vector
#'
#' @author Graham J. Williams
#'
#' @concept wrangling
#'
normVarNames <- function (vars, sep = "_") {
  if (sep == ".")
    sep <- "\\."
  pat <- "_| |•| |,|-|:|/|&|\\.|\\?|\\[|\\]|\\{|\\}|\\(|\\)"
  rep <- sep
  vars <- gsub(pat, rep, vars)
  pat <- "(?<!\\p{Lu})(\\p{Lu})(\\p{Lu}*)"
  rep <- "\\1\\L\\2"
  vars <- gsub(pat, rep, vars, perl = TRUE)
  pat <- "(?<!^)(\\p{Lu})"
  rep <- paste0(sep, "\\L\\1")
  vars <- gsub(pat, rep, vars, perl = TRUE)
  pat <- paste0("(?<![", sep, "\\p{N}])(\\p{N}+)")
  rep <- paste0(sep, "\\1")
  vars <- gsub(pat, rep, vars, perl = TRUE)
  vars <- gsub("^_+", "", vars)
  vars <- gsub("_+$", "", vars)
  vars <- gsub("__+", "_", vars)
  vars <- tolower(vars)
  pat <- paste0(sep, "+")
  rep <- sep
  vars <- gsub(pat, rep, vars)
  return(vars)
}
