#' @title Plot dataSEM
#' @description Creates plot of object with class 'dataSEM'. If input x is provided as list with more than one elements, the output plot is a grid with multiple plots.
#' @param x object of class dataSEM (see details)
#' @param ts character specifying the column containing the time stamp (format must be numeric or POSIXct)
#' @param tens.up character specifying the column containing the measurements of the upper tensiometer
#' @param tens.low character specifying the column containing the measurements of the lower tensiometer
#' @param weight character specifying the column containing the weight
#' @param color.tens colors of the plotted tensiometer values
#' @param color.weight color of the plotted weight values
#' @param plot.tens plot tensiometer values (TRUE/FALSE)
#' @param plot.weight plot weight values (TRUE/FALSE)
#' @param plot.legend plot legend (TRUE/FALSE)
#' @param plot.title character spezifying plot title. If empty no title will be added.
#' @param xlab lable for the x axis
#' @param ... Graphical arguments (see \code{\link{par}}). If plot.tens = T and plot.weight = T, lty only works for tensiometer values.
#' @details
#' Object x can be:\cr
#' - class(x): "dataSEM" "data.frame" \cr
#' - class(x): "dataSEM" "data.table" \cr
#' - class(x): "dataSEM" (if x is a list)\cr
#' \cr
#' If x is a list with more than 1 elements, the output plot is a grid with mutliple plots.
#' Columns and row number can be adjusted with grafical argument mfrow (see \code{\link{par}}) \cr
#' \cr
#' If x has the wrong class, the class can be set with: \cr
#' class(x) <- c('dataSEM', class(x)) (if x has the class data.frame or data.table) and \cr
#' class(x) <- 'dataSEM' (if x has the class list). \cr
#' @importFrom graphics plot abline axis mtext lines par legend axis.POSIXct
#' @importFrom data.table is.data.table
#' @importFrom lubridate is.timepoint
#' @author Ullrich Dettmann
#' @export

plot.dataSEM <- function(x, ts = 'ts', tens.up = 'tens.up', tens.low = 'tens.low', weight = 'weight',
                         plot.tens = TRUE, plot.weight = TRUE, plot.legend = TRUE,
                         xlab = 'timestamp', plot.title,
                         color.tens = c("#00FFFF", '#008B8B'), color.weight = "#EC382B",
                          ...) {

  if(missing(plot.title)) {plot.title <- NULL}
  if(any(grep('data.frame', class(x)))){ x <- list(x)}
  if(any(grep('data.table', class(x)))){x <- list(x)}
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # plot settings
  def.par <- par(no.readonly = TRUE) # get default par settings
  # set default graphical arguments
  if(is.null(list(...)$cex.axis)) {par(cex.axis = 1.5)}
  if(is.null(list(...)$cex.lab)) {par(cex.lab = 1.5)}
  if(is.null(list(...)$cex.main)) {par(cex.main = 1.5)}
  if(is.null(list(...)$cex)) {par(cex = 1)}
  if(is.null(list(...)$lty)) {par(lty = 3)}


  if(is.null(list(...)$mar)) {
    if((plot.tens & plot.weight == F) | (plot.tens == F & plot.weight)) {
      par(mar = c(5, 5, 4, 2)) # bottom, left, top, right
    }
    if(plot.tens & plot.weight) {
      par(mar = c(5, 5, 4, 5)) # bottom, left, top, right
    }}
  if(is.null(list(...)$lwd)) {par(lwd = 2)}
  if((is.null(list(...)$mfrow) & is.null(list(...)$mfcol)) & length(x) >1){ par(mfrow = c(ceiling(length(x)/2), 2))}
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  par(...)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for(i in 1:length(x)) {
    #setnames
    if(plot.tens) {
      names(x[[i]])[names(x[[i]]) == ts] <- 'ts'
      names(x[[i]])[names(x[[i]]) == tens.up] <- 'tens.up'
      names(x[[i]])[names(x[[i]]) == tens.low] <- 'tens.low'
    }
    if(plot.weight) {
      names(x[[i]])[names(x[[i]]) == ts] <- 'ts'
      names(x[[i]])[names(x[[i]]) == weight] <- 'weight'
    }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(plot.tens == T) {
    ylim <- range(c(x[[i]]$tens.up, x[[i]]$tens.low), na.rm = T)
    ylim[1] <- ylim[1] * 1.05
    ylim[2] <- ylim[2] * 1.05}
  if(plot.tens == F & plot.weight == T) { ylim <- range(x[[i]]$weight, na.rm = T)}
  xlim <- range(x[[i]]$ts, na.rm = T)
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   plot
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(plot.tens) {
    plot(tens.up ~ ts, data = x[[i]],
         xlim, ylim, ylab = 'pressure head [cm]', xlab = xlab,
         type='n',
         yaxs ='i', xaxs='i', axes = FALSE, main = plot.title[i])
    axis(side = 2, pos = min(xlim), col =  NA, col.ticks = 'black', lwd.ticks = 1)
    if(is.timepoint(x[[i]]$ts) == FALSE) {axis(side = 1, pos = min(ylim), col =  NA, col.ticks = 'black', lwd.ticks = 1)}
    if(is.timepoint(x[[i]]$ts)) {axis.POSIXct(side = 1, x = x[[i]]$ts, pos = min(ylim), col =  NA, col.ticks = 'black', lwd.ticks = 1, format = '%y-%m-%d')}
    abline(h = min(ylim), lwd = 1)
    abline(v = min(xlim), lwd = 1)
    lines(tens.up ~ ts,  data = x[[i]], col = color.tens[1])
    lines(tens.low ~ ts, data = x[[i]], col = color.tens[2])
  }
  if(plot.weight) {
    if(plot.tens) {
      par(new = T)
      ylim2 <-  range(x[[i]]$weight, na.rm = T)
      plot(weight ~ ts, data = x[[i]], xlim, ylim = ylim2, ylab = '', xlab = '', type='n',
           yaxs ='i', xaxs='i', axes = FALSE)
      axis(side = 4, pos = max(xlim), col =  NA, col.ticks = 'black', lwd.ticks = 1)
      if(any(par()$mfrow >= 3)){
      mtext('weight [g]', side = 4, padj = 3, cex = par()$cex.lab *0.66) #, cex = par()$cex.lab/2)
      }
      if(all(par()$mfrow == c(2,2))) {
      #if(length(x) == 3){
        mtext('weight [g]', side = 4, padj = 3, cex = par()$cex.lab *0.83) #, cex = par()$cex.lab/2)
      }
      if(any(par()$mfrow >= 3) == FALSE &  all(par()$mfrow == c(2,2)) == FALSE){
        mtext('weight [g]', side = 4, padj = 3, cex = par()$cex.lab)
      }
      abline(v = max(xlim), lwd = 1)
      lines(weight ~ ts,  data = x[[i]], lty = 1, col = color.weight)
    }
    if(plot.tens == F) {
      plot(weight ~ ts, data = x[[i]], xlim, ylim, ylab = 'weight [g]', xlab = xlab, type='n',
           yaxs ='i', xaxs='i', axes = FALSE, main = plot.title[i])
      axis(side = 2, pos = min(xlim), col =  NA, col.ticks = 'black', lwd.ticks = 1)
      if(is.timepoint(x[[i]]$ts) == FALSE) {
        axis(side = 1, pos = min(ylim), col =  NA, col.ticks = 'black', lwd.ticks = 1)
      }
      if(is.timepoint(x[[i]]$ts)) {
        axis.POSIXct(side = 1, x = x[[i]]$ts, pos = min(ylim), col =  NA, col.ticks = 'black', lwd.ticks = 1, format = '%y-%m-%d')
      }
      abline(v = min(xlim), lwd = 1)
      abline(h = min(ylim), lwd = 1)
      lines(weight ~ ts,  data = x[[i]], col = color.weight)
    }}
  # legend
  if (plot.legend) {
  if (plot.tens & plot.weight == F) {
     legend(x = 'bottomleft', legend = c("upper Tensiometer", "lower tensiometer"),
            col = color.tens, box.lwd = 0, bg = "transparent", cex = par()$cex.lab, lty = c(par()$lty, par()$lty))}
  if (plot.tens & plot.weight) {
    #x = -20, y = ylim2[1]*1.02
    legend('bottomleft', legend = c("upper tensiometer", "lower tensiometer", 'weigth'),
           col = c(color.tens, color.weight), lty = c(par()$lty, par()$lty, 'solid'), box.lwd = 0, bg = "transparent", cex = par()$cex.lab)}

  if (plot.tens == F & plot.weight) {
    legend(x = 'bottomleft', legend = c('weigth'),
           col = color.weight, box.lwd = 0, bg = "transparent", cex = par()$cex.lab, lty = par()$lty) #cex=1.5,
  }}}
  par(def.par) # set par to default values
}
