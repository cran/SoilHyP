#' @title Akaike Information Criterion (AIC)
#' @description Akaike Information Criterion with or without correction term. Expression from Ye et al. (2008). Correction term by Hurvich and Tsai (1989).
#' @param Phi objective function value
#' @param n.data number of measured data
#' @param n.par number of adjustable parameters
#' @param corr correction term TRUE or FALSE (see details)
#' @details corr: \cr
#' If number of measurements is small compared to the number of parameters, AIC can be extended by a correction term.
#' @references Ye, M., P.D. Meyer, and S.P. Neuman (2008): On model selection criteria in multimodel analysis. Water Resources Research 44 (3) W03428, doi:10.1029/2008WR006803.
#' @references Hurvich, C., and C. Tsai (1989): Regression and time series model selection in small samples. Biometrika 76 (2), 297–307, doi:10.1093/biomet/76.2.297.
#' @references Peters and Durner (2015): SHYPFIT 2.0 User's Manual.
#' @references Akaike, H. (1974): A new look at statistical model identification, IEEE Trans. Autom. Control, AC-19, 716–723.
#' @export
#'
AIC_HY <- function(Phi, n.data, n.par, corr = TRUE) {
  AIC <- n.data * log(Phi/n.data) + 2*n.par
  if (corr == TRUE) {
    AIC <- AIC + ((2*n.par * (n.par+1))/(n.data - n.par - 1))
  }
  AIC
}
