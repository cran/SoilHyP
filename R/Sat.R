#' @title Capillary saturation function
#' @description Capillary saturation function of van Genuchten (unimodal or bimodal pore space distributions) and Brooks and Corey (unimodal pore space distribution).
#' @param suc Suction/pressure heads. Negative if suc.negativ = TRUE
#' @param par.shp named parameter in list or vector
#' @param modality pore size distribution ('uni' or 'bi')
#' @param FUN.shp Funktion for soil hydraulic properties (vG or bc) (see details)
#' @param suc.negativ set TRUE if suction/pressure heads are negative and FALSE if positive
#' @details
#' \describe{\item{FUN.shp:}{
#' vG: van Genuchten (uni or bimodal) (vGM is working aswell) \cr
#' bc: Brooks and Corey (uni)}}
#' \describe{\item{par.shp (van Genuchten):}{
#' alfa [1/L]: van Genuchten shape parameter \cr
#' n [-]: van Genuchten shape parameter \cr
#' m [-]: shape parameter (m = 1-(1/n) if missing)}
#' \item{par.shp (additional for bimodal (modality = 'bi')):}{
#' w2 [-]: weigthing between pore space distribution \cr
#' alfa2 [1/L]: van Genuchten parameter alfa for second pore space distribution \cr
#' n2 [-]: van Genuchten parameter n for second pore space distribution}}
#' \describe{\item{par.shp (Brooks and Corey):}{
#' alfa [1/L]: inverse of the air-entry value or bubbling pressure \cr
#' lambda [-]: pore size distribution index }}
#' @references Van Genuchten, M. T. (1980). A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil science society of America journal, 44(5), 892-898.
#' @references Durner, W. (1994). Hydraulic conductivity estimation for soils with heterogeneous pore structure. Water Resources Research, 30(2), 211-223.
#' @references Brooks, R.H., and A.T. Corey (1964): Hydraulic properties of porous media. Hydrol. Paper 3. Colorado State Univ., Fort Collins, CO, USA.
#' @export
Sat <- function(suc, par.shp, modality = c('uni'), FUN.shp = 'vg', suc.negativ = TRUE) {

  names(par.shp) <- tolower(names(par.shp))
  FUN.shp <- tolower(FUN.shp)

  if (!is.list(par.shp)) { par.shp <- as.list(par.shp) }

  if(suc.negativ == FALSE) {
    suc <- suc * -1
  }
  suc <- ifelse(suc > 0, 0, suc)

  if (FUN.shp == 'vgm') {FUN.shp <-  'vg'}

  if (FUN.shp == 'vg') {
  if(!any(names(par.shp) %in% 'm')) { par.shp$m <- 1 - (1/par.shp$n) }

  modality <- tolower(modality)
  if(modality == 'bimodal') { modality = 'bi'}
  if(modality == 'unimodal') { modality = 'uni'}
  if (!is.list(par.shp)) par.shp <- as.list(par.shp)

  stopifnot(any(names(par.shp) == 'alfa'))
  stopifnot(any(names(par.shp) == 'n'))

  if (modality == 'uni') {
  sat <- (1/(1 + ((-suc * par.shp$alfa)^par.shp$n)))^par.shp$m
  }
  if (modality == 'bi') {
    stopifnot(any(names(par.shp) == 'alfa2'))
    stopifnot(any(names(par.shp) == 'n2'))
    stopifnot(any(names(par.shp) == 'w2'))
    if(!any(names(par.shp) %in% 'm2')) { par.shp$m2 <- 1 - (1/par.shp$n2) }
    sat <- ((1 - par.shp$w2)* (1/(1 + ((-suc * par.shp$alfa)^par.shp$n)))^par.shp$m) +
      (par.shp$w2 * (1/(1 + ((-suc * par.shp$alfa2)^par.shp$n2)))^par.shp$m2)
  }
  }
  if (FUN.shp == 'bc') {
    sat <- (par.shp$alfa*(-1*suc[suc < -(1/par.shp$alfa)]))^(-par.shp$lambda)
    sat <- c(rep(1, sum(suc >= -(1/par.shp$alfa))), sat)
  }

  sat
}



