#' @title Rescaled capillary saturation function
#' @description Rescaled capillary saturation function by Iden and Durner (2014) \cr
#' @param suc Suction/pressure heads. Negative if suc.negativ = TRUE
#' @param par.shp named parameter in list or vector
#' @param modality pore size distribution ('uni' or 'bi')
#' @param suc.negativ set TRUE if suction/pressure heads are negative and FALSE if positive
#' @details
#' \describe{\item{par.shp:}{
#' alfa [1/L]: van Genuchten shape parameter \cr
#' n [-]: van Genuchten shape parameter \cr
#' m [-]: shape parameter (m = 1-(1/n) if missing) \cr
#' h0 [L]: suction at water content of 0 (i.e. oven dryness) (h0 = 10^6.8 if missing, corresponding to oven dryness at 105°C (Schneider and Goss, 2012))}
#' \item{par.shp (additional for bimodal (modality = 'bi') pore size distribution:)}{
#' w2 [-]: weigthing between pore space distribution \cr
#' alfa2 [1/L]: van Genuchten parameter alfa for second pore space distribution \cr
#' n2 [-]: van Genuchten parameter n for second pore space distribution \cr
#' m2 [-]: shape parameter (m = 1-(1/n2) if missing) \cr}}
#' @details Scap(h) = (Gamma(h)- Gamma(h0))/(1 - Gamma(h0)) \cr
#' Gamma descripes the capillary saturation function. Here the saturation function of van Genuchten is used: \cr
#' gamma(h) = (1/(1 + -suc * alfa)^n)^m (see also \code{\link{Sat}})\cr
#' @references Iden, S., Durner, W. (2014). Comment to Simple consistent models for water retention and hydraulic conductivity in the complete moisture range by A. Peters. Water Resour. Res. 50, 7530–7534.
#' @references Schneider, M., & Goss, K. U. (2012). Prediction of the water sorption isotherm in air dry soils. Geoderma, 170, 64-69.
#' @export
Scap <- function(suc, par.shp,  modality = c('uni'), suc.negativ = FALSE) {
  if (!is.list(par.shp)) { par.shp <- as.list(par.shp) }
  # prepare input
  if(suc.negativ == FALSE) { suc <- suc * -1 }
  suc <- ifelse(suc > 0, 0, suc)
  # add h0 if missing
  if(!any(names(par.shp) %in% 'h0')) { par.shp$h0 <- 10^6.8 }
  if(par.shp$h0 < 0) { par.shp$h0 <- abs(par.shp$h0)}
  # suc[suc < -par.shp$h0] <- -par.shp$h0
  if(!any(names(par.shp) %in% 'm')) { par.shp$m <- 1 - (1/par.shp$n) }

  scap <- (Sat(suc, par.shp = par.shp, modality = modality, suc.negativ = TRUE) - Sat(par.shp$h0, par.shp = par.shp, modality = modality, suc.negativ = FALSE))/
   (1 - Sat(par.shp$h0, par.shp = par.shp, modality = modality, suc.negativ = FALSE))

  scap
}

