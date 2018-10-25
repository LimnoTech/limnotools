#' Import WinModel Model Results
#'
#' Import WinModel model results into R from all worksheets within an xlsb workbook
#'
#' @param x path and workbook name of the WinModel-generated xlsb file
#'
#' @importFrom excel.link xl.workbook.open xl.sheets
#'
#' @export
#'
#' @author Julie Padilla
#'
#' @return Returns a \code{list} of \code{data.frames}, one for each sheet in the workbook
#'
read_winmodel_xlsb <- function(x) {
  wb <- excel.link::xl.workbook.open(filename = x)
  wb_sheets <- excel.link::xl.sheets()

  ls_data <- lapply(wb_sheets, read_winmodel_sheet, wb = x)

  names(ls_data) <- wb_sheets

  return(ls_data)

}
