#' Remove non-detect ("<") flags from sampling data
#'
#' Assign seasons to sampling data on a monthly basis or user-defined basis
#'
#' @param x a character vector values
#'
#' @import lubridate magrittr stringr
#'
#' @export
#'
#' @details This function removes the "<" flag from the front a sampling result value.
#' Currently, this function is used in conjunction with \code{nd_classifier}
#'
#' @return Returns a numeric vector
#'
#' @seealso \code{nd_classifier}
#'
nd_remover <- function(x){
  ifelse(str_detect(x, '<')
         , as.numeric(str_replace(x, "[<]", ""))
         , x <- as.numeric(x)
  )
}
