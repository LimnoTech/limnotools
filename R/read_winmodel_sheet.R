#' Import WinModel Model Results From a xlsb Sheet
#'
#' Import WinModel model results into R from a single worksheet in an xlsb workbook
#'
#' @param sheet name of the sheet to load data from
#' @param wb path and workbook name of the WinModel-generated xlsb file
#' @param data.top.cell top left cell location where the WinModel model results start (in terms of "letter-number")
#' @param header.top.cell top left cell location where the WinModel header information starts (in terms of "letter-number")
#' @param key char, used to determine the name of the key column used by \code{tidyr::gather}
#'
#' @importFrom excel.link xl.read.file
#' @importFrom stringr str_pad
#' @importFrom tidyr gather
#'
#' @export
#'
#' @author Julie Padilla
#'
#' @return Returns a \code{data.frame}
#'
read_winmodel_sheet <- function(sheet, wb, data.top.cell = 'A10', header.top.cell = 'B4', key = 'rchres') {

  sheets <- excel.link::xl.read.file(filename = wb, xl.sheet = sheet, header = T, top.left.cell = data.top.cell)

  par_nm <- names(sheets)[2]

  sheets_head <- excel.link::xl.read.file(filename = wb, xl.sheet = sheet, header = T, top.left.cell = header.top.cell)

  names(sheets) <- c('datetime', paste0(names(sheets_head)[1], '_', stringr::str_pad(sheets_head[1, ], width = 3, side = 'left', pad = '0')))

  sheets <- tidyr::gather(sheets, key = key, value = sheets, 2:length(names(sheets)))

  names(sheets)[3] <- par_nm

  return(sheets)
}
