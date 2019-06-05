#' @title  Root mean square error (RMSE)
#' @description Calculate Root mean square error (RMSE)
#' @param obs measured values
#' @param sim predicted values
#' @param na.rm logical. Should missing values be removed?
#' @export

RMSE <- function(obs, sim, na.rm = FALSE) {

  stopifnot(length(obs) == length(sim))
  if (na.rm == TRUE) {
    rmse <- sqrt(sum((obs[!is.na(obs) & !is.na(sim)] - sim[!is.na(obs) & !is.na(sim)])^2)/length(obs[!is.na(obs) & !is.na(sim)]))
  }
  if (na.rm == FALSE) {
    rmse <- sqrt(sum((obs - sim)^2)/length(obs))
  }
  rmse
}