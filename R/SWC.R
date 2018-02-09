#' @title Soil water content
#' @description Calculates the volumetric soil water content for a corresponding suction/pressure head (th(suc)) for unimodal or bimodal van Genuchten (vG) or Peters-Durner-Iden (PDI) parameterisation.
#' @param suc Suction/pressure heads. Negative if suc.negativ = TRUE
#' @param FUN.shp Funktion for soil hydraulic properties (vG or PDI) (see details)
#' @param par.shp named parameter in list or vector
#' @param modality pore size distribution ('uni' or 'bi')
#' @param suc.negativ set TRUE if suction/pressure heads are negative and FALSE if positive
#' @return volumetric water content theta (th) [L³/L³]
#' @details
#' \describe{\item{FUN.shp:}{
#' vG: van Genuchten (uni or bimodal) (vGM is working aswell) \cr
#' PDI: Peters-Durner-Iden with van Genuchtens saturation function (uni or bimodal)}}
#' \describe{\item{par.shp (vG and PDI):}{
#' ths [-]: saturated water content \cr
#' thr [-]: residual water content\cr
#' alfa [1/L]: van Genuchten shape parameter \cr
#' n [-]: van Genuchten shape parameter \cr
#' m [-]: shape parameter (m = 1-(1/n) if missing)}
#' \item{}{additional for 'PDI':\cr
#' h0 [L]: suction at water content of 0 (i.e. oven dryness) (h0 = 10^6.8 if missing, corresponding to oven dryness at 105°C (Schneider and Goss, 2012))}
#' \item{}{additional for bimodal (modality == 'bi'): \cr
#' w2 [-]: weigthing between pore space distributions \cr
#' alfa2 [1/L]: van Genuchten parameter alfa for second pore space distribution \cr
#' n2 [-]: van Genuchten parameter n for second pore space distribution \cr
#' m2 [-]: shape parameter (m2 = 1-(1/n2) if missing)}}
#' @details PDI:\cr
#' theta(h) = (ths - thr) * Scap(h) + thr * Sad(h) \cr
#' \code{\link{Scap}: Rescaled capillary saturation function } \cr
#' \code{\link{Sad}: Relative saturation function for adsorbed water}
#' @details input for FUN.shp and modality works for upper- and lowercase letters \cr
#' @examples
#' # --------------------------------------------
#' #  Unimodal van Genuchten
#' # --------------------------------------------
#' SWC(suc = seq(1, 1000, by = 1), par.shp = c(ths = 0.4, thr = 0, alfa = 0.02, n = 1.5),
#' FUN.shp = c('vG'), modality = 'uni', suc.negativ = FALSE)
#' # --------------------------------------------
#' #  Bimodal van Genuchten
#' # --------------------------------------------
#' SWC(suc = seq(1, 1000, by = 1),
#' par.shp = c(ths = 0.4, thr = 0, alfa = 0.02, n = 2, w2 = 0.2, alfa2 = 1, n2 = 10),
#' FUN.shp  = c('vG'), modality = c('bi'), suc.negativ = FALSE)
#' # --------------------------------------------
#' #  Unimodal PDI
#' # --------------------------------------------
#' SWC(suc = seq(1, 1000, by = 1), par.shp = list(ths = 0.4, thr = 0, n = 1.6, alfa = 0.02),
#'   FUN.shp = c('pdi'), modality = c('uni'), suc.negativ = FALSE)
#' # --------------------------------------------
#' @references Van Genuchten, M. T. (1980). A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil science society of America journal, 44(5), 892-898.
#' @references Durner, W. (1994). Hydraulic conductivity estimation for soils with heterogeneous pore structure. Water Resources Research, 30(2), 211-223.
#' @references Peters, A. (2013). Simple consistent models for water retention and hydraulic conductivity in the complete moisture range. Water Resour. Res. 49, 6765–6780. physics-a review. Vadose Zone J. http://dx.doi.org/10.2136/vzj2012.0163.
#' @references Iden, S., Durner, W. (2014). Comment to Simple consistent models for water retention and hydraulic conductivity in the complete moisture range by A. Peters. Water Resour. Res. 50, 7530–7534.
#' @references Peters, A. (2014). Reply to comment by S. Iden and W. Durner on Simple consistent models for water retention and hydraulic conductivity in the complete moisture range. Water Resour. Res. 50, 7535–7539.
#' @references Schneider, M., & Goss, K. U. (2012). Prediction of the water sorption isotherm in air dry soils. Geoderma, 170, 64-69.
#' @author Ullrich Dettmann
#' @seealso \code{\link{Ku}} \code{\link{Sat}}
#' @export
SWC <- function(suc, par.shp = c(ths = 0.9, thr = 0, alfa = 0.02, n = 2),
                FUN.shp = 'vg', modality = 'uni', suc.negativ = TRUE) {

  if (!is.list(par.shp)) { par.shp <- as.list(par.shp) }
  # tolower input
  names(par.shp) <- tolower(names(par.shp))
  modality <- tolower(modality)
  FUN.shp <- tolower(FUN.shp)

  if(modality == 'bimodal') { modality = 'bi'}
  if(modality == 'unimodal') { modality = 'uni'}

  if (FUN.shp == 'vgm') {FUN.shp <-  'vg'}
  # prepare input
  if(suc.negativ == FALSE) { suc <- suc * -1 }
  suc <- ifelse(suc > 0, 0, suc)

# check if all necessary parameter are given in input
  stopifnot(any(names(par.shp) == 'alfa'))
  stopifnot(any(names(par.shp) == 'n'))
  stopifnot(any(names(par.shp) == 'thr'))
  stopifnot(any(names(par.shp) == 'ths'))
  # add m if missing
  if(!any(names(par.shp) %in% 'm')) { par.shp$m <- 1 - (1/par.shp$n) }
  ## --------------------------------------------------------------------------------------------------------------------------------------------------
  #    Calculate theta
  ## --------------------------------------------------------------------------------------------------------------------------------------------------
  #
  # van Genuchten (1980)
  if (FUN.shp == 'vg') {
    if (modality == 'uni') {
      th <- par.shp$thr + (par.shp$ths - par.shp$thr) * (1 + (par.shp$alfa* -suc )^par.shp$n)^(-(1-(1/par.shp$n)))
    }
    if (modality == 'bi') {
      # check if all necessary parameter are given in input
      stopifnot(any(names(par.shp) == 'alfa2'))
      stopifnot(any(names(par.shp) == 'n2'))
      stopifnot(any(names(par.shp) == 'w2'))

      th <- (((1-par.shp$w2) *(1+(par.shp$alfa*-suc)^par.shp$n)^(-(1-(1/par.shp$n))))+ #wSe1
               (par.shp$w2 *(1+(par.shp$alfa2*-suc)^par.shp$n2)^(-(1-(1/par.shp$n2)))))* #wSe2
        (par.shp$ths - par.shp$thr) + par.shp$thr
    }
  }
  #
  # pdi model with van Genuchten satuation function
  if (FUN.shp == 'pdi') {
    # add h0 if missing
    if(!any(names(par.shp) %in% 'h0')) { par.shp$h0 <- 10^6.8 }
    # set par.shp$h0 to minimum of suction if minimum is smaller than h0
    if (any(suc < -par.shp$h0)) { print(paste0('Suctions below h0 (',-par.shp$h0,') are set to h0'))}
    suc[suc < -par.shp$h0] <- -par.shp$h0
    # relative saturation of capillary water (defined by Iden and Durner 2014) Scap(h) = (Gamma(h)-Gamma(h0))/(1 - Gamma(h0)))
    scap <- Scap(suc, par.shp = par.shp, modality = modality, suc.negativ = TRUE)

    # Adsorptive saturation function
    sad <- Sad(suc, par.shp = par.shp, modality = modality, suc.negativ = TRUE)
    th <- ((par.shp$ths - par.shp$thr) * scap) + (par.shp$thr * sad)
    th[suc == 0] <- par.shp$ths
  }
  th
}