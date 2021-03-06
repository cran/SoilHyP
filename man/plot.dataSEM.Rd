% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_dataSEM.R
\name{plot.dataSEM}
\alias{plot.dataSEM}
\title{Plot dataSEM}
\usage{
\method{plot}{dataSEM}(
  x,
  ts = "ts",
  tens.up = "tens.up",
  tens.low = "tens.low",
  weight = "weight",
  plot.tens = TRUE,
  plot.weight = TRUE,
  plot.legend = TRUE,
  xlab = "timestamp",
  plot.title,
  color.tens = c("#00FFFF", "#008B8B"),
  color.weight = "#EC382B",
  ...
)
}
\arguments{
\item{x}{object of class dataSEM (see details)}

\item{ts}{character specifying the column containing the time stamp (format must be numeric or POSIXct)}

\item{tens.up}{character specifying the column containing the measurements of the upper tensiometer}

\item{tens.low}{character specifying the column containing the measurements of the lower tensiometer}

\item{weight}{character specifying the column containing the weight}

\item{plot.tens}{plot tensiometer values (TRUE/FALSE)}

\item{plot.weight}{plot weight values (TRUE/FALSE)}

\item{plot.legend}{plot legend (TRUE/FALSE)}

\item{xlab}{lable for the x axis}

\item{plot.title}{character spezifying plot title. If empty no title will be added.}

\item{color.tens}{colors of the plotted tensiometer values}

\item{color.weight}{color of the plotted weight values}

\item{...}{Graphical arguments (see \code{\link{par}}). If plot.tens = T and plot.weight = T, lty only works for tensiometer values.}
}
\description{
Creates plot of object with class 'dataSEM'. If input x is provided as list with more than one elements, the output plot is a grid with multiple plots.
}
\details{
Object x can be:\cr
- class(x): "dataSEM" "data.frame" \cr
- class(x): "dataSEM" "data.table" \cr
- class(x): "dataSEM" (if x is a list)\cr
\cr
If x is a list with more than 1 elements, the output plot is a grid with mutliple plots.
Columns and row number can be adjusted with grafical argument mfrow (see \code{\link{par}}) \cr
\cr
If x has the wrong class, the class can be set with: \cr
class(x) <- c('dataSEM', class(x)) (if x has the class data.frame or data.table) and \cr
class(x) <- 'dataSEM' (if x has the class list). \cr
}
\author{
Ullrich Dettmann
}
