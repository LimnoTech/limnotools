#' A non-detect substitution function
#'
#' This function allows you to pass a tibble, specify a results vector, and a detection limit vector.
#' Any result value below it's associated detection limit will be updated to half the detection limit value.
#' The result will be the original tibble with the updated results vector.
#' @param df data.frame or tibble containing x and y
#' @param x colname of the vector containing results
#' @param y colname of the vector containing method DLs
#' @param method method for handling substitutions, currently "halfDL" is the only method supported.
#' @keywords non-detect, dl, halfDL
#' @export
#' @examples
#' ndSub()

ndSub <- function(df,x,y,method) {
  if(method==halfDL) {
    subval <- df[[y]]/2
    df[[x]][is.na(df[[x]])] <- subval[is.na(df[[x]])]
    l <- length(df[[x]][is.na(df[[x]])])
    df <- df[!is.na(df[[x]]),]
    cat(c("Warning:", l, "records were excluded because ND replacement value was NULL"))
  } else {
    print("User provided method not exist. Method = halfDL (1/2 MDL substitution) is currently the only method supported.")
  }
  return(df)
}
