#' @title Predict values using fitSHP object
#' @description Predicts values using fitSHP object with calibrated paramter of SWC, KU or both depending on the fitSHP object
#' @param object object of class fitSHP
#' @param suc Suction/pressure heads for the prediction of the soil hydraulic properties
#' @param length.out output length if suc == NULL
#' @param suc.negativ set TRUE if suction/pressure heads are negative and FALSE if positive
#' @param ... arguments for predict
#' @export

predict.fitSHP <- function(object, suc = NULL, length.out = 100, suc.negativ = FALSE, ...) {
  if(is.null(suc)) {
  suc <- exp(seq(log(0.001), log(1000), length.out = length.out))
  }

  if(suc.negativ == FALSE) { suc <- suc * -1 }
  suc <- ifelse(suc > 0, 0, suc)

    ## Plot
  if(object$input$fit == 'both') {

    fit.swc <- SWC(suc, par.shp = object$par, FUN.shp = object$input$FUN.shp, modality = object$input$modality, suc.negativ = TRUE)

    fit.ku <- Ku(suc, par.shp = object$par, FUN.shp = object$input$FUN.shp, modality = object$input$modality, suc.negativ = TRUE)
    obj <- data.frame(suc = suc, SWC = fit.swc, Ku = fit.ku)
  }
  if(object$input$fit == 'swc') {

    fit.swc <- SWC(suc, par.shp = object$par, FUN.shp = object$input$FUN.shp, modality = object$input$modality, suc.negativ = TRUE)
    obj <- data.frame(suc = suc, SWC = fit.swc)
  }
  if(object$input$fit == 'ku') {

    fit.ku <- Ku(suc, par.shp = object$par, FUN.shp = object$input$FUN.shp, modality = object$input$modality, suc.negativ = TRUE)
    obj <- data.frame(suc = suc, Ku = fit.ku)
  }
obj
}
