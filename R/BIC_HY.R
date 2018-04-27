#' @title Bayesian Information Criterion (BIC)
#' @description Bayesian Information Criterion (Schwarz, 1978) for least square estimations.
#' @param Phi objective function value
#' @param n.data number of measured data
#' @param n.par number of adjustable parameters
#' @references Ye, M., P.D. Meyer, and S.P. Neuman (2008): On model selection criteria in multimodel analysis. Water Resources Research 44 (3) W03428, doi:10.1029/2008WR006803.
#' @references Schwarz, G. (1978): Estimating the dimension of a model. The Annals of Statistics 6 (2), 461–464. URL: http://dx. doi. org/10.1214/aos/1176344136.
#' @references Peters and Durner (2015): SHYPFIT 2.0 User's Manual.
#' @export
#'
BIC_HY <- function(Phi, n.data, n.par) {
  BIC <- n.data * log(Phi/n.data) + n.par*log(n.data)
  BIC
}
