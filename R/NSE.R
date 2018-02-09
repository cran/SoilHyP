#' @title Nash-Sutcliffe efficiency (NSE)
#' @description Nash-Sutcliffe efficiency (NSE)
#' @param obs measured values
#' @param sim predicted values
#' @references Nash, J. E., and J.V. Sutcliffe (1970): River flow forecasting through conceptual models. 1. a discussion of principles. Journal of Hydrology 10, 282–290.
#' @export

NSE <- function(obs, sim) {

  stopifnot(length(obs) == length(sim))
  1- ((sum((obs - sim)^2))/(sum((obs - mean(obs))^2)))
}
