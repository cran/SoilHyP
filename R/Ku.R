#' @title Unsaturated hydraulic conductivity
#' @description Calculates unsaturated hydraulic conductivity for a given suction for unimodal or bimodal van Genuchten-Mualem (vg/vgm), Peters-Durner-Iden (PDI) and Brooks and Corey (bc) (only unimodal) parameterisation.
#' @param suc Suction/pressure heads. Negative if suc.negativ = TRUE
#' @param FUN.shp Funktion for soil hydraulic properties (vGM or PDI) (see details)
#' @param par.shp named parameter in list or vector
#' @param modality pore size distribution ('uni' or 'bi')
#' @param suc.negativ set TRUE if suction/pressure heads are negative and FALSE if positive
#' @return unsaturated hydraulic conductivity (ku)
#' @details
#' \describe{\item{FUN.shp:}{vGM: van Genuchten-Mualem (uni or bimodal) ('vg' works aswell)\cr
#' PDI: Peters-Durner-Iden with van Genuchtens saturation function (uni or bimodal) \cr
#' bc: Brooks and Corey (unimodal)}}
#' \describe{\item{par.shp (vG and PDI):}{
#' ths [-]: saturated water content\cr
#' thr [-]: residual water content\cr
#' alfa [1/L]: van Genuchten shape parameter\cr
#' n [-]: van Genuchten shape parameter\cr
#' m [-]: shape parameter (m = 1-(1/n) if missing)\cr
#' Ks [L/time]: saturated hydraulic conductivity\cr
#' tau [-]:  tortuosity and connectivity parameter (minimum -1 or -2 for the PDI model; see Peters (2014) for details)}
#' \item{}{additional for 'PDI':\cr
#' omega: weighting between relative capillary and film conductivity \cr
#' h0 [L]: suction at water content of 0 (i.e. oven dryness) (h0 = 10^6.8 if missing, corresponding to oven dryness at 105°C (Schneider and Goss, 2012))\cr
#' a: slope at the log scale (a = -1.5 if missing as suggested by Tokunaga (2009) and Peters (2013))}
#' \item{}{additional for bimodal (modality == 'bi'): \cr
#' w2 [-]: weigthing between pore space distributions \cr
#' alfa2 [1/L]: van Genuchten parameter alfa for second pore space distribution \cr
#' n2 [-]: van Genuchten parameter n for second pore space distribution}}
#' \describe{\item{par.shp (BC):}{
#' ths [-]: saturated water content \cr
#' thr [-]: residual water content\cr
#' alfa [1/L]: inverse of the air-entry value or bubbling pressure \cr
#' lambda [-]: pore size distribution index \cr
#' tau [-]:  tortuosity and connectivity parameter (minimum -1 or -2 for the PDI model; see Peters (2014) for details)}}
#' @details most input works for upper- and lowercase letters
#' @examples
#' # --------------------------------------------
#' #  Unimodal van Genuchten
#' # --------------------------------------------
#' Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'vGM',
#'    par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02, n = 1.5, tau = 0.5),
#'    modality = 'uni', suc.negativ = FALSE)
#' # --------------------------------------------
#' #  Bimodal van Genuchten
#' # --------------------------------------------
#' Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'vGM',
#'    par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02,
#'    n = 1.5, tau = 0.5, w2 = 0.1, alfa2 = 0.1, n2 = 3),
#'    modality = 'bi', suc.negativ = FALSE)
#' # --------------------------------------------
#' #  Unimodal Peters-Durner-Iden (PDI)
#' # --------------------------------------------
#' Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'PDI', modality = 'uni',
#'    par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02, n = 1.5, tau = 0.5, omega = 0.001),
#'    suc.negativ = FALSE)
#' # --------------------------------------------
#' #  Brooks and Corey (BC) (only unimodal)
#' # --------------------------------------------
#' Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'bc', modality = 'uni',
#'    par.shp = list(ths = 0.4, thr = 0, lambda =  0.211, alfa = 0.1, tau = 0.5, ks = 10),
#'    suc.negativ = FALSE)
#' # --------------------------------------------
#' @references Van Genuchten, M. T. (1980). A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil science society of America journal, 44(5), 892-898.
#' @references Mualem, Y. (1976). A new model for predicting the hydraulic conductivity of unsaturated porous media. Water resources research, 12(3), 513-522.
#' @references Peters, A. (2013). Simple consistent models for water retention and hydraulic conductivity in the complete moisture range. Water Resour. Res. 49, 6765–6780. physics-a review. Vadose Zone J. http://dx.doi.org/10.2136/vzj2012.0163.
#' @references Iden, S., Durner, W. (2014). Comment to Simple consistent models for water retention and hydraulic conductivity in the complete moisture range by A. Peters. Water Resour. Res. 50, 7530–7534.
#' @references Peters, A. (2014). Reply to comment by S. Iden and W. Durner on Simple consistent models for water retention and hydraulic conductivity in the complete moisture range. Water Resour. Res. 50, 7535–7539.
#' @references Tokunaga, T. K. (2009), Hydraulic properties of adsorbed water films in unsaturated porous media, Water Resour. Res., 45, W06415, doi: 10.1029/2009WR007734.
#' @references Priesack, E., Durner, W., 2006. Closed-form expression for the multi-modal unsaturated conductivity function. Vadose Zone J. 5, 121–124.
#' @references Durner, W. (1994). Hydraulic conductivity estimation for soils with heterogeneous pore structure. Water Resources Research, 30(2), 211-223.
#' @references Schneider, M., & Goss, K. U. (2012). Prediction of the water sorption isotherm in air dry soils. Geoderma, 170, 64-69.
#' @references Brooks, R.H., and A.T. Corey (1964): Hydraulic properties of porous media. Hydrol. Paper 3. Colorado State Univ., Fort Collins, CO, USA.
#' @author Ullrich Dettmann
#' @seealso \code{\link{SWC}} and \code{\link{Sat}}
#' @export

Ku <- function(suc, FUN.shp = 'vG', par.shp, modality = 'uni', suc.negativ = TRUE) {

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
  stopifnot(any(names(par.shp) == 'thr'))
  stopifnot(any(names(par.shp) == 'ths'))
  stopifnot(any(names(par.shp) == 'ks'))
  stopifnot(any(names(par.shp) == 'tau'))
  # add m if missing
  if(!any(names(par.shp) %in% 'm')) { par.shp$m <- 1 - (1/par.shp$n) }


# --------------------------------------------------------------------------------------------
  ## pdi
  if (FUN.shp == 'pdi') {
    # add h0 if missing
    stopifnot(any(names(par.shp) == 'n'))
    if(!any(names(par.shp) %in% 'h0')) { par.shp$h0 <- 10^6.8 }
    if(!any(names(par.shp) %in% 'a')) { par.shp$a <- -1.5 }
    # stop if a parameter is missing in par.shp
    stopifnot(any(names(par.shp) == 'omega'))

    suc[suc < -par.shp$h0] <- -par.shp$h0

    kfilm <- Kfilm(suc, par.shp = par.shp, modality = modality, suc.negativ = TRUE)

    kcap <-  Kcap(suc, par.shp = par.shp, modality = modality, suc.negativ = TRUE)

    K <- ((par.shp$ks*(1 - par.shp$omega)) * kcap) +
         ((par.shp$ks* par.shp$omega)    * kfilm)
    K[suc == 0] <- par.shp$ks
    }

  if (FUN.shp == 'vg') {
    stopifnot(any(names(par.shp) == 'n'))

    if (modality == 'uni'){
      Se1<-(1+(par.shp$alfa*-suc)^par.shp$n)^(-(1-(1/par.shp$n)))
      K <- par.shp$ks * Se1^par.shp$tau*((1-((1 - (Se1^(1/par.shp$m)))^par.shp$m))^2)

    }
    if (modality == 'bi') {
      # check if all necessary parameter are given in input
      stopifnot(any(names(par.shp) == 'alfa2'))
      stopifnot(any(names(par.shp) == 'n2'))
      stopifnot(any(names(par.shp) == 'w2'))

      if(!any(names(par.shp) %in% 'm2')) { par.shp$m2 <- 1 - (1/par.shp$n2) }
  Se1<-(1+(par.shp$alfa*-suc)^par.shp$n)^(-(1-(1/par.shp$n)))
  Se2<-(1+(par.shp$alfa2*-suc)^par.shp$n2)^(-(1-(1/par.shp$n2)))
  wSe1<- (1 - par.shp$w2) *(1 + (par.shp$alfa*-suc)^par.shp$n)^(-(1-(1/par.shp$n)))
  wSe2<- par.shp$w2 *(1+(par.shp$alfa2*-suc)^par.shp$n2)^(-(1-(1/par.shp$n2)))
  term1_oben<-(wSe1+wSe2)^par.shp$tau;

  w1 <- (1  -par.shp$w2)
  temp_w1<- (w1) * par.shp$alfa *(1-(1- Se1 ^(1/par.shp$m))^par.shp$m)
  temp_w2<- (par.shp$w2) * par.shp$alfa2 *(1-(1- Se2 ^(1/par.shp$m2))^par.shp$m2)

  t_o<- (temp_w1 + temp_w2)^2
  temp_u<- ((w1 *par.shp$alfa)+(par.shp$w2*par.shp$alfa2))^2
  K <- par.shp$ks * ((term1_oben * t_o) / temp_u)
    }
  }

  if (FUN.shp == 'bc') {
    # check if all necessary parameter are given in input
    stopifnot(any(names(par.shp) == 'alfa'))
    stopifnot(any(names(par.shp) == 'lambda'))
    stopifnot(modality == 'uni')

    K <- par.shp$ks * (Sat(suc = suc, par.shp = par.shp, FUN.shp = FUN.shp, modality = 'uni', suc.negativ = TRUE))^((2/par.shp$lambda) + par.shp$tau + 2)
  }

  K
}
