#' Remove non-detect ("<") flags from sampling data
#'
#' Assign seasons to sampling data on a monthly basis or user-defined basis
#'
#' @param x a character vector values
#' @param halfDL logical, substitute half of the detection limit? Default is \code{FALSE}
#' @param ... additional arguments passed to other methods
#'
#' @export
#'
#' @details This function removes the "<" flag from the front a sampling result value.
#' Currently, this function is used in conjunction with \code{nd_classifier}
#'
#' @examples
#' \dontrun{
#' x <- c('0.24', '>0.25', '2', '>0.5', '<2.5', '<6')
#' nd_remover(x)
#'
#' x <- c('  0.24', '> 0.25', '2 ', '>.5', '<2.5', '<  6')
#' nd_remover(x, which = 'both')
#' }
#'
#' @return Returns a numeric vector
#'
#' @seealso \code{nd_classifier}, \code{\link[base]{trimws}}
#'
nd_remover <- function(x, halfDL = FALSE, ...){

  x <- trimws(x, ...)

  x <- gsub('>', '', x, fixed = T)
  x <- as.numeric(gsub('<', '', x, fixed = T))

  return(x)
}

