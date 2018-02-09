#' @title  Root mean square error (RMSE)
#' @description Calculate Root mean square error (RMSE)
#' @param obs measured values
#' @param sim predicted values
#' @export

RMSE <- function(obs, sim) {

  stopifnot(length(obs) == length(sim))
  sqrt(sum((obs - sim)^2)/length(obs))
  }
