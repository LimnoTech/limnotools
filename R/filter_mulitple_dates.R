#' Filter a data.frame for multiple dates
#'
#' Filter a data.frame for multiple time periods
#'
#' @param x a data.frame
#' @param date_var chr string of the name for the datetime column
#' @param date_list a list of dates in the format of c(start, end). Dates will be used to filter \code{data.frame}, inclusive. Dates must be in \code{POSIXct} format.
#' @param use_names logical If \code{TRUE} a new column will be added to label events based on names assigned to elements in \code{date_list}
#'
#' @export
#'
#' @details This function allows the user to filter a single data.frame for multiple time periods.
#'
#' @author Julie Padilla
#'
#' @return Returns a data.frame
#'
#' @examples
#' \dontrun{
#' df <- data.frame(datetime = seq(from = as.POSIXct('2000-01-01')
#' , to = as.POSIXct('2000-02-01'), by = 'hour')
#' , value = rnorm(745))
#'
#' events <- lapply(list(c('2000-01-02 0:00', '2000-01-02 05:00')
#' , c('2000-01-04 0:00', '2000-01-04 05:00')), as.POSIXct)
#'
#' filter_multiple_dates(df, date_var = 'datetime', date_list = events, use_names = F)
#' }
#'
filter_multiple_dates <- function(x, date_var, date_list, use_names = T) {
  if(!is.list(date_list))
    stop('Dates must be in list format and include a start and end date for filtering')

  if(use_names & is.null(names(date_list)))
    stop('date_list does not have names. Either add names to date_list or set use_names = F')

  y <- lapply(date_list, function(z) {x[x[ , date_var] >= z[1] & x[ , date_var] <= z[2], ]})

  if(use_names) {
    names(y) <- names(date_list)

    y <- lapply(1:length(y), function(z) cbind(y[[z]], name = names(y[z]), stringsAsFactors = F))

  }

  y <- do.call(rbind, y)

  return(y)

}
