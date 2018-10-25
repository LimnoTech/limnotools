#' Remove commas in numeric values
#'
#' Remove internal commas from numbers and convert to numeric
#'
#' @param x vector of numbers that has been imported as character because of internal commas (e.g., "1,000")
#'
#' @export
#'
#' @details This function handles vectors of numbers that have been imported as \code{character} or \code{factor} values due to the inclusion of an internal comma (e.g. "14,300" instead of "14300"). The function removes all internal commas and then converts it to numeric.
#'
#' @author Julie Padilla
#'
#' @return Returns a numeric vector
#'
#' @examples
#' x <- c('14,300', 1, 2, 5, 7)
#' y <- as.factor(x)
#'
#' remove_comma(x)
#' remove_comma(y)
#'
remove_comma <- function(x) {

  if(is.numeric(x))
    warning('vector is already numeric. No internal commas detected.')

  if(is.factor(x))
    x <- as.character(x)

  x <- as.numeric(gsub(',', '', x))

  return(x)

}
