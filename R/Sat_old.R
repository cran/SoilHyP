#' @title Capillary saturation function
#' @description Capillary saturation function of van Genuchten for unimodal or bimodal pore space distributions.
#' @param suc Suction/pressure heads. Negative if suc.negativ = TRUE
#' @param par.shp named parameter in list or vector
#' @param modality pore size distribution ('uni' or 'bi')
#' @param suc.negativ set TRUE if suction/pressure heads are negative and FALSE if positive
#' @details
#' \describe{\item{par.shp:}{
#' alfa [1/L]: van Genuchten shape parameter \cr
#' n [-]: van Genuchten shape parameter \cr
#' m [-]: shape parameter (m = 1-(1/n) if missing)}
#' \item{}{additional for bimodal (modality == 'bi'): \cr
#' w2 [-]: weigthing between pore space distribution \cr
#' alfa2 [1/L]: van Genuchten parameter alfa for second pore space distribution \cr
#' n2 [-]: van Genuchten parameter n for second pore space distribution}}
#' @references Van Genuchten, M. T. (1980). A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil science society of America journal, 44(5), 892-898.
#' @references Durner, W. (1994). Hydraulic conductivity estimation for soils with heterogeneous pore structure. Water Resources Research, 30(2), 211-223.
Sat_old <- function(suc, par.shp, modality = c('uni'), suc.negativ = TRUE) {

  names(par.shp) <- tolower(names(par.shp))
  if (!is.list(par.shp)) { par.shp <- as.list(par.shp) }

  if(suc.negativ == FALSE) {
    suc <- suc * -1
  }
  suc <- ifelse(suc > 0, 0, suc)
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

  sat
}

