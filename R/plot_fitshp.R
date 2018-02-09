#' @title Plot fitSHP object
#' @description Creates plot of fitSHP object with measured and fitted SWC, KU or both depending on fitSHP object
#' @param x object of class fitSHP
#' @param ... arguments for plot
#' @importFrom graphics lines par plot legend
#' @export

plot.fitSHP <- function(x, ...) {

  if (x$input$suc.negativ == TRUE) {
    if (x$input$fit == 'both') {
      suc.th <- -1 * x$input$suc$th
      suc.K <- -1 * x$input$suc$K
    }
    if (x$input$fit == 'swc') {
      suc.th <- -1 * x$input$suc$th
    }
    if (x$input$fit == 'ku') {
      suc.K <- -1 * x$input$suc$K
    }
  }
  if (x$input$suc.negativ == FALSE) {
    if (x$input$fit == 'both') {
      suc.th <- x$input$suc$th
      suc.K <- x$input$suc$K
    }
    if (x$input$fit == 'swc') {
      suc.th <- x$input$suc$th
    }
    if (x$input$fit == 'ku') {
      suc.K <- x$input$suc$K
    }
  }
  ## Plot
  if(x$input$fit == 'both') {
    temp1 <- exp(seq(log(1), log(max(suc.th)), length.out = 100))
    fit.swc <- SWC(temp1, par.shp = x$par, FUN.shp = x$input$FUN.shp, modality = x$input$modality, suc.negativ = FALSE)
    temp2 <- exp(seq(log(1), log(max(suc.K)), length.out = 100))
    fit.ku <- Ku(temp2, par.shp = x$par, FUN.shp = x$input$FUN.shp, modality = x$input$modality, suc.negativ = FALSE)
  }
  if(x$input$fit == 'swc') {
    temp1 <- exp(seq(log(1), log(max(suc.th)), length.out = 100))

    fit.swc <- SWC(temp1, par.shp = x$par, FUN.shp = x$input$FUN.shp, modality = x$input$modality, suc.negativ = FALSE)
  }
  if(x$input$fit == 'ku') {
    temp2 <- exp(seq(log(1), log(max(suc.K)), length.out = 100))
    fit.ku <- Ku(temp2, par.shp = x$par, FUN.shp = x$input$FUN.shp, modality = x$input$modality, suc.negativ = FALSE)
  }

  ## Plot
  if(x$input$fit == 'both') {
    par(mfrow = c(1, 2))

    plot(x = suc.th[suc.th >0], y = x$input$obs$th[suc.th >0],
         xlim = range(temp1),
         ylim = c(0, 1),
         log = 'x',
         xlab = 'Pressure Heads [cm]',
         ylab = expression(paste(theta," (cm"^3, " cm"^-3,')')),
         pch = 20, ...)
    lines(x = temp1, y = fit.swc, col = 'RED', ...)
    # Unsaturated hydraulic conductivity
    plot(x = suc.K[suc.K >0], y = x$input$obs$K[suc.K >0],
         xlim = range(temp2),
         ylim = range(fit.ku),
         log = 'xy',
         xlab = 'Pressure Heads [cm]',
         ylab = 'Ku', pch = 20, ...)
    lines(x = temp2, y = fit.ku, col = 'RED', ...)
    lines(fit.ku ~ temp2, col = 'RED', ...)
    legend('bottomleft', legend=c('Data', 'Fit'), col=c('Black', 'RED'),
           pch = c(20, NA), lty = c(NA, 1), ...)
  }
  if(x$input$fit == 'swc') {
    # Soil water content
    plot(x = suc.th[suc.th >0], y = x$input$obs$th[suc.th >0],
         xlim = range(temp1),
         ylim = c(0, 1),
         log = 'x',
         xlab = 'Pressure Heads [cm]',
         ylab = expression(paste(theta," (cm"^3, " cm"^-3,')')),
         pch = 20, ...) # ylim = range(fit.swc),
    lines(x = temp1, y = fit.swc, col = 'RED', ...)
    legend('bottomleft', legend=c('Data', 'Fit'), col=c('Black', 'RED'),
           pch = c(20, NA), lty = c(NA, 1), ...)
  }
  if(x$input$fit == 'ku') {
    # Unsaturated hydraulic conductivity
    plot(x = suc.K[suc.K >0], y = x$input$obs$K[suc.K >0],
         xlim = range(temp2),
         ylim = range(fit.ku),
         log = 'xy',
         xlab = 'Pressure Heads [cm]',
         ylab = 'Ku', pch = 20, ...)
    lines(x = temp2, y = fit.ku, col = 'RED', ...)
    legend('bottomleft', legend=c('Data', 'Fit'), col=c('Black', 'RED'),
           pch = c(20, NA), lty = c(NA, 1), ...)
  }
}

