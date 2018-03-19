#' Ammonia standard caclulation for Washington, DC 
#'
#' This function calculates the ammonia standards based on season, pH, and water temperature.
#' @param month, temperaure, pH
#' @keywords wqs Ammonia
#' @export
#' @examples
#' cat_function()



ammStd <- function(month, temperature, pH) {
  if(month>6|month<3){
    ((0.0577/(1+10^(7.688-pH))) + (2.487/(1+10^(pH-7.688))))*(1.45*10^(0.028*(25-pmax(temperature,7))))
  } else {
    ((0.0577/(1+10^(7.688-pH))) + (2.487/(1+10^(pH-7.688))))*pmin(2.85,1.45*10^(0.028*(25-temperature)))
  }
}


