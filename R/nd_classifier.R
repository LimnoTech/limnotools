#' Assign detect/non-detect to sampling data
#'
#' Assign seasons to sampling data on a monthly basis or user-defined basis
#'
#' @param x a character vector values
#'
#' @export
#'
#' @details This function assigns detect/non-detect/greater than detect (D/ND/GD) based on the presence of a "<" or ">" flag attached to the sampling result value.
#' Currently, this function is used in conjunction with \code{nd_remover}
#'
#' @examples
#' \dontrun{
#' x <- c('0.24', '>0.25', '2', '>0.5', '<2.5', '<6')
#' nd_classifier(x)
#'
#' x <- c('  0.24', '> 0.25', '2 ', '>.5', '<2.5', '<  6')
#' nd_classifier(x)
#' }
#'
#' @return Returns a character vector
#'
#' @seealso \code{nd_remover}
#'
#' @author Julie Padilla
#'
nd_classifier <- function(x){
  ifelse(grepl('<', x), "ND", ifelse(grepl('>', x), "GD", "D"))
}
