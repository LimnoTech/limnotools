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
#' @return Returns a character vector
#'
#' @seealso \code{nd_remover}
#'
##Used to assign "ND"/"D" to values
nd_classifier <- function(x){
  ifelse(str_detect(x, '<')
         , "ND"
         , "D"
  )
}
