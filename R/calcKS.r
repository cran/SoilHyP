#' @title Saturated hydraulic conductivity
#' @description Calculates saturated hydraulic conductivity (ks) following Darcy's law
#' @param V water volume passed sample [L³]
#' @param Tmeas duration of measurement [time]
#' @param L length of the sample [L]
#' @param A cross-sectional area [L²]
#' @param dP pressure difference between top and bottom of the sample during the measurement [L]
#' @return hydraulic conductivity (ks) [L/time].
#' @details Keep units consistent, e.g: V = cm³, dP = cm, A = cm², L = cm, Tmeas = hour
#' @export

calcKS <- function(V, Tmeas, L, A, dP ){
  ks <- V*L/(Tmeas*A*dP)
  ks
}
