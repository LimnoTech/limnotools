#' Assign detect/non-detect to sampling data
#'
#' Assign seasons to sampling data on a monthly basis or user-defined basis
#'
#' @param x a character vector values
#'
#' @import lubridate magrittr stringr
#'
#' @export
#'
#' @details This function assign D/ND based on the presence of a "<" flag attached to the sampling result value.
#' Currently, this function is used in conjunction with \code{nd_remover}
#'
#' @examples
#' \dontrun{
#' x <- c('0.24', '2', '0.5', '<2.5', '<6')
#' nd_remover(x)
#' }
#'
#' @return Returns a character vector
#'
#' @seealso \code{nd_remover}
#'
##Used to assign "ND"/"D" to values
nd_classifier <- function(x){
  ifelse(grepl('<', x), "ND", "D")
}
