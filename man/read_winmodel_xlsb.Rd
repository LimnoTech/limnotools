% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_winmodel_xlsb.R
\name{read_winmodel_xlsb}
\alias{read_winmodel_xlsb}
\title{Import WinModel Model Results}
\usage{
read_winmodel_xlsb(x)
}
\arguments{
\item{x}{path and workbook name of the WinModel-generated xlsb file}
}
\value{
Returns a \code{list} of \code{data.frames}, one for each sheet in the workbook
}
\description{
Import WinModel model results into R from all worksheets within an xlsb workbook
}
\author{
Julie Padilla
}
