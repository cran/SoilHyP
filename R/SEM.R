#' @title Simplified evaporation method (SEM)
#' @description Determines unsaturated hydraulic conductivity and water retention characteristics from laboratory evaporation experiments.
#' @param suc.up a numeric vector containing the measured suctions [cm] of the upper tensiometer
#' @param suc.low a numeric vector containing the measured suctions [cm] of the lower tensiometer
#' @param weight  a numeric vector containing the measured weights [g]
#' @param t time in seconds [s]
#' @param r sample radius [cm]
#' @param L sample height [cm]
#' @param z1 depth of upper tensiometer [cm]
#' @param z2 depth of lower tensiometer [cm]
#' @param sd.tens measurement accuracy of tensiometer [cm]
#' @param suc.negativ set TRUE if suction/tensiometer values are negative and FALSE if positive
#' @param suc.out 'weighted' (default), arithmetic ('ari') or geometric ('geo') mean of the tensiometer readings (see Peters (2015) for details)
#' @param ths saturated water content (optional) for the calulation of the soil water contents (th)
#' @details
#' \describe{\item{sd.tens:}{
#' At the beginning of the experiment when gradients of the hydraulic head are small, hydraulic conductivities cannot be calculated. Following Peters and Durner (2008) hydraulic conductivities calculated from gradients smaller than (6*sd.tens)/(z2-z1) are set to NA.}}
#' @return
#' \describe{\item{data.frame}{}
#' \item{}{Ki: unsaturated hydraulic conductivity [cm/day]}
#' \item{}{th: water content (th) is returned if ths is provided as input}
#' \item{}{suc: suction, either (1) weighted between arithmetic and geometric mean (default), (2) the arithmetic mean or (3) the geometric mean (see Peters 2015)}}
#' @examples
#' # ----------------------------------------------------------------------------
#' # Calculate hydraulic properties with the 'Simplified Evaporation Method' (SEM)
#' # ----------------------------------------------------------------------------
#' data('dataSEM')
#' ths <- 0.7  # define saturated water content (ths) (optional)
#' shp <- SEM(suc.up     = dataSEM$tens.up,
#'           suc.low     = dataSEM$tens.low,
#'           weight      = dataSEM$weight,
#'           t           = dataSEM$hour*60*60,
#'           r           = 3.6, # radius of sample
#'           L           = 6,   # height of sample
#'           z1          = 1.5, # depth of upper tensiometer [cm]
#'           z2          = 4.5, # depth of lower tensiometer [cm]
#'           sd.tens     = 0.1,  # tensiometer accuracy (see ?SEM)
#'           ths         = ths,
#'           suc.negativ = TRUE,
#'           suc.out     = 'weighted'
#' )
#' @references Wind, G. P. (1966). Capillary conductivity data estimated by a simple method (No. 80). [sn].
#' @references Peters, A., Iden, S. C., & Durner, W. (2015). Revisiting the simplified evaporation method: Identification of hydraulic functions considering vapor, film and corner flow. Journal of Hydrology, 527, 531-542.
#' @references Peters, A., & Durner, W. (2008). Simplified evaporation method for determining soil hydraulic properties. Journal of Hydrology, 356(1), 147-162.
#' @references Schindler, U., 1980. Ein Schnellverfahren zur Messung der Wasserleitfähigkeit im teilgesättigten Boden an Stechzylinderproben. Arch. Acker- Pflanzenbau Bodenkd. 24, 1–7.
#' @author Ullrich Dettmann
#' @export
#'

SEM <- function(suc.up, suc.low, weight = NULL, t, ths = NULL, r = 3.6, L = 6, z1 = 1.5, z2 = 4.5, sd.tens = 0.2, suc.negativ = TRUE, suc.out = 'weighted') {

  if (suc.negativ == TRUE) {
    suc.up <- suc.up *-1
    suc.low <- suc.low *-1
    }

  if (sum(suc.up) < sum(suc.low)) {
    temp <- suc.up
    suc.up <- suc.low
    suc.low <- temp
    rm(temp)
  }
  # Calulate time difference between two tme steps
  t.sec <- diff(t)
  # geometric calculations
  A <- pi * r^2
  deltaZ <- z2 - z1
  vol <- L * A

  # calculate hydraulic conductivity (see Wind (1966))
  deltaV <- diff(weight)
    # calc flux
    q      <- 0.5*(deltaV/(t.sec*A)) # cm/sec
    deltaH <- 0.5* (
      (suc.up[1:(length(suc.up)-1)] - suc.low[1:(length(suc.low)-1)]) +
      (suc.up[2:length(suc.up)]     - suc.low[2:length(suc.low)])
    )


  gradH <- ((deltaH)/(deltaZ) - 1)
  # hydraulic conductivity
  Ki <- -q/gradH
  # calculate th if ths is provided
  if(!is.null(ths)) {
    th <- ((vol*ths) + cumsum(deltaV))/vol
  }

  # get 'arithmetic', 'geometric' and 'weighted' mean of tensiometer readings
  tens.mean.ari <- 0.25* (suc.up[1:length(suc.up)-1] + suc.low[1:length(suc.low)-1] + suc.up[2:length(suc.up)] + suc.low[2:length(suc.low)])
  tens.mean.geo <- suppressWarnings(
    sqrt(((suc.up[1:length(suc.up)-1] + suc.up[2:length(suc.up)]) * (suc.low[1:length(suc.low)-1] + suc.low[2:length(suc.low)])))/2)

  # weighted mean
  w.avg <- 1/gradH
  w.avg <- ifelse(w.avg < 0 | w.avg == Inf, 1, w.avg)
  w.avg <- ifelse(w.avg > 1, 1, w.avg)
  tens.mean.mix <- tens.mean.ari
  # weight geo and ari if tens.mean.geo is not NA
  tens.mean.mix[!is.na(tens.mean.geo)]  <-   w.avg[!is.na(tens.mean.geo)] * tens.mean.ari[!is.na(tens.mean.geo)] +
    (1 - w.avg[!is.na(tens.mean.geo)]) *  tens.mean.geo[!is.na(tens.mean.geo)]
  # based on uncertainties of tensiometer readings unsaturated hydraulic conductivity is only calculated if gradients are > of 6*sd.tens/deltaZ
  filter <- (6*sd.tens)/(z2-z1)
  # calculate cm/day
  Ki <- Ki * 60*60*24
  Ki[gradH < filter] <- NA

  if (suc.out == 'weighted') {
    suc <- -1 * tens.mean.mix
  }
  if (suc.out == 'ari' | suc.out == 'arithmetic') {
    suc <- -1 * tens.mean.ari
  }
  if (suc.out == 'geo' | suc.out == 'geometric') {
    suc <- -1 * tens.mean.geo
  }

if (!is.null(ths)) {

  if( any(th < 0 )) {
    warning('Negative water contents! Check ths!!!')
  }
  obj <- data.frame(Ku = Ki, th = th, suc = suc)
}
  if (is.null(ths)) {
  obj <- data.frame(Ku = Ki, suc = suc)
  }
  obj
}
