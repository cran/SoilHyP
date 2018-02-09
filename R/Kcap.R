#' @title Relative capillary conductivity
#' @description Relative capillary conductivity based on Mualem's conductivity model for unimodal or bimodal van Genuchten-Mualem.
#' @param suc Suction/pressure heads. Negative if suc.negativ = TRUE
#' @param par.shp named parameter in list or vector
#' @param suc.negativ set TRUE if suction/pressure heads are negative and FALSE if positive
#' @param modality pore size distributions ('uni' or 'bi')
#' @details
#' \describe{\item{par.shp:}{
#' alfa [1/L]: van Genuchten shape parameter \cr
#' n [-]: van Genuchten shape parameter \cr
#' m [-]: shape parameter (m = 1-(1/n) if missing) \cr
#' tau [-]:  tortuosity and connectivity parameter (minimum -1 or -2 for the PDI model; for details see Peters (2014)) \cr
#' h0 [L]: suction at water content of 0 (i.e. oven dryness) (h0 = 10^6.8 if missing, corresponding to oven dryness at 105°C (Schneider and Goss, 2012))}
#' \item{}{additional for bimodal (modality == 'bi') \cr
#' w2 [-]: weigthing between pore space distribution \cr
#' alfa2 [1/L]: van Genuchten parameter alfa for second pore space distribution \cr
#' n2 [-]: van Genuchten parameter n for second pore space distribution}}
#' @references Peters, A. (2014). Reply to comment by S. Iden and W. Durner on Simple consistent models for water retention and hydraulic conductivity in the complete moisture range. Water Resour. Res. 50, 7535–7539.
#' @references Van Genuchten, M. T. (1980). A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil science society of America journal, 44(5), 892-898.
#' @references Mualem, Y. (1976). A new model for predicting the hydraulic conductivity of unsaturated porous media. Water resources research, 12(3), 513-522.
#' @references Schneider, M., & Goss, K. U. (2012). Prediction of the water sorption isotherm in air dry soils. Geoderma, 170, 64-69.
#' @seealso \code{\link{Ku}}
#' @export


Kcap <- function(suc, par.shp, suc.negativ = TRUE, modality = 'uni') {

  if (!is.list(par.shp)) { par.shp <- as.list(par.shp) }
  # prepare input
  if(suc.negativ == FALSE) { suc <- suc * -1 }
  suc <- ifelse(suc > 0, 0, suc)

  modality <- tolower(modality)
  names(par.shp) <- tolower(names(par.shp))

  stopifnot(any(names(par.shp) == 'alfa'))
  stopifnot(any(names(par.shp) == 'n'))
  stopifnot(any(names(par.shp) == 'tau'))
   # add h0 and m if missing
  if(!any(names(par.shp) %in% 'h0')) { par.shp$h0 <- 10^6.8 }
  if(!any(names(par.shp) %in% 'm')) { par.shp$m <- 1 - (1/par.shp$n) }
  if(par.shp$h0 < 0) { par.shp$h0 <- abs(par.shp$h0)}

  if (modality == 'uni') {

    kcap <- (Scap(suc = suc, par.shp = par.shp, suc.negativ = TRUE, modality = modality)^par.shp$tau) *
    (((1-
        (
          ((1- Sat(suc,        par.shp = par.shp, modality = modality, suc.negativ = TRUE)^(1/par.shp$m)))/
          ((1- Sat(par.shp$h0, par.shp = par.shp, modality = modality, suc.negativ = FALSE)^(1/par.shp$m)))
        )^par.shp$m
    )^2))
  }

   if (modality == 'bi') {
     stopifnot(any(names(par.shp) == 'alfa'))
     stopifnot(any(names(par.shp) == 'n'))
     stopifnot(any(names(par.shp) == 'w2'))
     if(!any(names(par.shp) %in% 'm2')) { par.shp$m2 <- 1 - (1/par.shp$n2) }

    w.Scap <- Scap(suc = suc, par.shp = par.shp, modality = 'bi', suc.negativ = TRUE)

    w.top <- ((1 - par.shp$w2)* par.shp$alfa*(1 - (Sat(suc, par.shp = par.shp, modality = 'uni', suc.negativ = TRUE)^(1/par.shp$m)))^par.shp$m) +
             (par.shp$w2* par.shp$alfa2*     (1 - (Sat(suc, par.shp = list(alfa = par.shp$alfa2,
                                                                n = par.shp$n2,
                                                                m = par.shp$m2)   , modality = 'uni', suc.negativ = TRUE)^(1/par.shp$m2)))^par.shp$m2)
    w.bot <- ((1 - par.shp$w2)* par.shp$alfa*(1 - (Sat(par.shp$h0, par.shp = par.shp, modality = 'uni', suc.negativ = FALSE)^(1/par.shp$m)))^par.shp$m) +
             (par.shp$w2* par.shp$alfa2*     (1 - (Sat(par.shp$h0, par.shp = list(alfa = par.shp$alfa2,
                                                                       n = par.shp$n2,
                                                                       m = par.shp$m2), modality = 'uni', suc.negativ = FALSE)^(1/par.shp$m2)))^par.shp$m2)
    kcap <-  (w.Scap^par.shp$tau) * (1-(w.top/w.bot))^2
  }
  kcap
}

