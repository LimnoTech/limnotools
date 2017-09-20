#' Remove non-detect ("<") flags from sampling data
#'
#' Assign seasons to sampling data on a monthly basis or user-defined basis
#'
#' @param x a character vector values
#'
#' @export
#'
#' @details This function removes the "<" flag from the front a sampling result value.
#' Currently, this function is used in conjunction with \code{nd_classifier}
#'
#' @examples
#' \dontrun{
#' x <- c('0.24', '2', '0.5', '<2.5', '<6')
#' nd_remover(x)
#' }
#'
#' @return Returns a numeric vector
#'
#' @seealso \code{nd_classifier}
#'
nd_remover <- function(x){
  gsub('<', '', x, fixed = T)
}

