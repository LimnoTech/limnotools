#' Excel Serial Time Conversion
#'
#' Convert serial times from MS Excel to POSIXct format
#'
#' @param x num serial date
#' @param unit a character string specifying a time unit or a multiple of a unit to be rounded to. Valid base units are second, minute, hour, day, week, month, bimonth, quarter, season, halfyear and year. Arbitrary unique English abbreviations as in the \code{\link[lubridate]{period}} constructor are allowed. Rounding to multiple of units (except weeks) is supported. If no unit is specified, unit defaults to "minute".
#'
#' @importFrom lubridate round_date
#'
#' @export
#'
#' @details Converts serial dates from excel into POSIXct dates. \code{\link[lubridate]{round_date}} takes a date-time object and rounds it to the nearest value of the specified time unit. For rounding date-times which is exactly halfway between two consecutive units, the convention is to round up. Note that this is in line with the behavior of R's \code{\link[base]{round.POSIXt}} function but does not follow the convention of the base \code{\link[base]{round}} function which "rounds to the even digit" per IEC 60559.
#'
#' @examples
#' \dontrun{
#' xl_serial_time_conversion(40544.86)
#' }
#'
#' @return Returns a \code{POSIXct} object
#'
#' @author Julie Padilla
#'
#' @seealso \code{\link[lubridate]{round_date}}
#'
#' @concept conversion
#'
xl_serial_time_conversion <- function(x, unit = NA){
  dt <- as.POSIXct(x * (60*60*24)
                   , origin = "1899-12-30"
                   , tz = "UTC")

  if(!is.na(unit)){
    dt <- round_date(dt, unit = unit)
  }

  return(dt)
}
