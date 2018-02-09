#' @title Relative saturation function
#' @description  Relative saturation function for adsorptive water storage described by a piecwise linear function (Iden and Durner, 2014).
#' @param suc Suction/pressure heads. Negative if suc.negativ = TRUE
#' @param par.shp named parameter of soil hydraulic properties in list or vector (see details)
#' @param modality pore size distribution ('uni' or 'bi')
#' @param suc.negativ set TRUE if suction/pressure heads are negative and FALSE if positive
#' @details
#' \describe{\item{par.shp:}{
#' ths [-]: saturated water content \cr
#' thr [-]: residual water content \cr
#' alfa [1/L]: van Genuchten shape parameter \cr
#' n [-]: van Genuchten shape parameter \cr
#' h0 [L]: suction at water content of 0 (i.e. oven dryness) (h0 = 10^6.8 if missing, corresponding to oven dryness at 105°C (Schneider and Goss, 2012))}
#' \item{}{additional for bimodal (modality == 'bi'): \cr
#' alfa2 [1/L]: van Genuchten parameter alfa for second pore space distribution \cr
#' n2 [-]: van Genuchten parameter n for second pore space distribution}}
#' @references Iden, S., Durner, W. (2014). Comment to Simple consistent models for water retention and hydraulic conductivity in the complete moisture range by A. Peters. Water Resour. Res. 50, 7530–7534.
#' @references Schneider, M., & Goss, K. U. (2012). Prediction of the water sorption isotherm in air dry soils. Geoderma, 170, 64-69.
#' @author Ullrich Dettmann
#' @export

Sad <- function(suc, par.shp , modality = c('uni'), suc.negativ = TRUE) {
  if (!is.list(par.shp)) { par.shp <- as.list(par.shp) }
  # tolower input
  modality <- tolower(modality)
  names(par.shp) <- tolower(names(par.shp))
  # prepare data
  if(suc.negativ == FALSE) {
    suc <- suc * -1
  }
  suc <- ifelse(suc > 0, 0, suc)

  stopifnot(any(names(par.shp) == 'alfa'))
  stopifnot(any(names(par.shp) == 'n'))
  stopifnot(any(names(par.shp) == 'thr'))
  stopifnot(any(names(par.shp) == 'ths'))

  # add h0 if missing
  if(!any(names(par.shp) %in% 'h0')) { par.shp$h0 <- 10^6.8 }
  if(par.shp$h0 < 0) { par.shp$h0 <- abs(par.shp$h0)}

  # Adsorptive saturation function
  # shape parameter b for the adsorption function in dependence to van Genuchtens parameter n (Iden 2014)
  if (modality == 'uni') {
    temp <- par.shp$n
    alfa.max <- par.shp$alfa
  }
  if (modality == 'bi') {
    stopifnot(any(names(par.shp) == 'alfa2'))
    stopifnot(any(names(par.shp) == 'n2'))

    if(par.shp$alfa > par.shp$alfa2) temp <- par.shp$n
    if(par.shp$alfa <= par.shp$alfa2) temp <- par.shp$n2
    alfa.max <- ifelse(par.shp$alfa > par.shp$alfa2, par.shp$alfa, par.shp$alfa2)
  }

  b <- 0.1 + (0.2/temp^2) * (1 - exp(-(par.shp$thr/(par.shp$ths - par.shp$thr))^2))

  x  <- log10(-suc)
  x0 <- log10(par.shp$h0)
  ha <- 1/alfa.max # suction at air entry for the adsorptive retention
  xa <- log10(ha)

  sad <- 1 + (1/(xa - x0)) * (x - xa + b * log(1 + exp(((xa - x)/b)))) # Adsorptive saturation function
sad
}

