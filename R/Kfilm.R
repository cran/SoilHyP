#' @title Relative film conductivity
#' @description Relative film conductivity described by Peters (2013).
#' @param suc Suction/pressure heads. Negative if suc.negativ = TRUE
#' @param par.shp named parameter in list or vector
#' @param suc.negativ set TRUE if suction/pressure heads are negative and FALSE if positive
#' @param modality pore size distribution ('uni' or 'bi')
#' @details
#' \describe{\item{par.shp:}{
#' ths [-]: saturated water content \cr
#' thr [-]: residual water content \cr
#' alfa [1/L]: van Genuchten shape parameter \cr
#' n [-]: van Genuchten shape parameter \cr
#' h0 [L]: suction at water content of 0 (i.e. oven dryness) (h0 = 10^6.8 if missing, corresponding to oven dryness at 105°C (Schneider and Goss, 2012))\cr
#' a: slope at the log scale (a = -1.5 if missing as suggested by Tokunaga (2009) and Peters (2013))}
#' \item{par.shp: additional parameter for bimodal (modality == 'bi')}{
#' alfa2 [1/L]: van Genuchten parameter alfa for second pore space distribution \cr
#' n2 [-]: van Genuchten parameter n for second pore space distribution}}
#' @references Peters, A. (2013). Simple consistent models for water retention and hydraulic conductivity in the complete moisture range. Water Resour. Res. 49, 6765–6780. physics-a review. Vadose Zone J. http://dx.doi.org/10.2136/vzj2012.0163.
#' @references Tokunaga, T. K. (2009). Hydraulic properties of adsorbed water films in unsaturated porous media. Water resources research, 45(6).
#' @references Schneider, M., & Goss, K. U. (2012). Prediction of the water sorption isotherm in air dry soils. Geoderma, 170, 64-69.
#' @seealso \code{\link{Ku}}
#' @export

Kfilm <- function(suc, par.shp, modality = 'uni', suc.negativ = TRUE) {
  if (!is.list(par.shp)) { par.shp <- as.list(par.shp) }
  # tolower input
  modality <- tolower(modality)
  names(par.shp) <- tolower(names(par.shp))
  # prepare input
  if(suc.negativ == FALSE) { suc <- suc * -1 }
  suc <- ifelse(suc > 0, 0, suc)


  stopifnot(any(names(par.shp) == 'alfa'))
  stopifnot(any(names(par.shp) == 'n'))
  stopifnot(any(names(par.shp) == 'thr'))
  stopifnot(any(names(par.shp) == 'ths'))


  # add h0 if missing
  if(!any(names(par.shp) %in% 'h0')) { par.shp$h0 <- 10^6.8 }
  if(!any(names(par.shp) %in% 'a')) { par.shp$a <- -1.5 }

  if (modality == 'uni') {
    alfa.max <- par.shp$alfa
  }
  if (modality == 'bi') {
    stopifnot(any(names(par.shp) == 'alfa2'))
    alfa.max <- ifelse(par.shp$alfa > par.shp$alfa2, par.shp$alfa, par.shp$alfa2)
  }

  ha <- 1/alfa.max # ha suction below which saturation of the adsorptive part is 1

  Kfilm <- (par.shp$h0/ha)^(
  par.shp$a*(1 - Sad(suc, par.shp = par.shp, modality = modality, suc.negativ = TRUE)))
  Kfilm
}

