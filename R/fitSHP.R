#' @title Fit soil hydraulic properties
#' @description Estimate parameter for soil water retention (SWC) and/or unsaturated hydraulic conductivity function (Ku) using Shuffled Complex Evolution (SCE) optimisation. Parameter can be estimated for van Genuchten-Mualem (vg or vgm) or Peters-Durner-Iden (PDI) parameterisation of the soil hydraulic properties.
#' @param obs list with named observations (th for water content and K for unsaturated hydraulic conductivity data)
#' @param suc list of named suctions corresponding to th and/or K
#' @param par a numeric vector of initial parameter values (see also \code{\link{SCEoptim}}). If missing default values are set.
#' @param lower lower bounds on the parameters. Should be the same length as \code{par} and as \code{upper}, or length 1 if a bound applies to all parameters. If missing default values are set.
#' @param upper upper bounds on the parameters. Should be the same length as \code{par} and as \code{lower}, or length 1 if a bound applies to all parameters. If missing default values are set.
#' @param FUN.shp Funktion for soil hydraulic properties (vG, PDI or bc) (see \code{\link{SWC}} or \code{\link{Ku}})
#' @param par.shp fixed parameter value named in list or vector
#' @param modality pore size distribution ('uni' or 'bi')
#' @param fit fit parameter for 'SWC', 'Ku' or 'both' simultaneous.
#' @param weighting weighting between SWC and Ku. Used if fit == both ('var', 'norm' or '2step').
#' @param log names of parameter in list or vector which should be logarithmized during optimization
#' @param suc.negativ set TRUE if suction/pressure heads are negative and FALSE if positive
#' @param control a list of options as in \code{optim()}, see \code{\link{SCEoptim}}
#' @param integral th as point value vs. suc(h) (FALSE) or th as mean water content over the column divided by the height (L) vs. suc(h) (TRUE) (see details).
#' @param L sample height [cm]. Only needed for integral == TRUE
#' @param log_Ku logarithmize Ku in the objective function and for weighting (TRUE).
#' @param print.info print information about default values for par, lower, and upper if missing or fitting accuracy (TRUE or FALSE)
#' @details
#' \describe{\item{weigthing:\cr}{
#' var: th and K are weighted in the objective fuction by the measurement variance  \cr
#' norm: th and K are normed in objective fuction\cr
#' stepwise: the parameter for th are fitted first and the remaining parameter for K afterwards (2step works aswell)}}
#' \describe{\item{log:\cr}{
#' The use of log is suggested for paramter 'alfa', 'n' and 'ks' for modality == 'uni'. For modality 'bi' additional 'alfa2' and 'n2' and for Fun.shp == 'pdi' additional 'omega'.
#' Parameter in output ($par) are not returned logarithmized. \cr
#' Delfault paramter values for par, lower and upper are logarithmized automatatically \cr
#' If not the default values for par, lower and upper are taken, parameter which are named in 'log' must be scaled by the user in par, lower and upper.
#' }
#' }
#' \describe{\item{integral:\cr}{
#' The "integral" method is suggested from Peters and Durner (2008, 2015) to fit parameter on data from
#' experiments were water contents are measured as mean water contents (e.g. simplified evaporation method or multi-step outflow experiments).
#' Under the assumption that the water content is distributed linear over the column, the measured
#' mean water content of the column is the integral over the whole column divided by the column length (L).
#' Under hydraulic equilibrium this is equal to the integral of the retention function over the matric
#' heads from the lower boundary to the upper boundary of the column divided by the height of the column (Peters 2008, 2015). \cr
#' \cr
#' integral == TRUE can be very slow.
#'
#' }}
#' @importFrom stats integrate
#' @author Ullrich Dettmann
#' @examples
##' \dontrun{
#' data('dataSHP')
#' # -------------------------------------------------------------------
#' # fit Soil Hydraulic Properties (SHP)
#' # -------------------------------------------------------------------
# unimodal van Genuchten-Mualem
#' ans <- fitSHP(obs         = list(th = dataSHP$th, K = dataSHP$Ku),
#'              suc          = list(th = dataSHP$suc, K = dataSHP$suc),
#'              FUN.shp      = 'vg',
#'              modality     = 'uni',
#'              par.shp      =  NULL,
#'              fit          = 'both',
#'              weighting    = 'var',
#'              log          = c('alfa', 'n', 'ks'),
#'              control      = list(ncomplex = 15, reltol = 1e-07,tolsteps = 7),
#'              suc.negativ  = TRUE,
#'              integral     = FALSE,
#'              L            = 0,
#'              print.info   = TRUE
#')
#' ans$par
#' plot(ans)
#' # --------------------------------------------------------------------
#' # bimodal van Genuchten-Mualem
#' ans <- fitSHP(obs         = list(th = dataSHP$th, K = dataSHP$Ku),
#'              suc          = list(th = dataSHP$suc, K = dataSHP$suc),
#'              FUN.shp      = 'vg',
#'              modality     = 'bi',
#'              par.shp      =  c(),
#'              fit          = 'both',
#'              weighting    = 'var',
#'              log          = c('alfa', 'n', 'ks', 'alfa2', 'n2'),
#'              suc.negativ  = TRUE,
#'              integral     = FALSE,
#'              L            = 0,
#'              print.info   = TRUE,
#'              control      = list(ncomplex = 15, reltol = 1e-07,tolsteps = 7)
#' )
#' ans$par
#' plot(ans)
#' }
#' @return "fitSHP" class
#' @seealso \code{\link{SCEoptim}}, \code{\link{SWC}}, \code{\link{Ku}}
#' @importFrom stats sd
#' @references Peters, A., & Durner, W. (2008). Simplified evaporation method for determining soil hydraulic properties. Journal of Hydrology, 356(1), 147-162.
#' @references Peters and Durner (2015). SHYPFIT 2.0 User’s Manual
#' @references Peters, A., Iden, S. C., & Durner, W. (2015). Revisiting the simplified evaporation method: Identification of hydraulic functions considering vapor, film and corner flow. Journal of Hydrology, 527, 531-542.
#' @export
#'
# todo: Add AIC
fitSHP <- function(obs = list(th = NULL, K = NULL),
                   suc = list(th = NULL, K = NULL),
                   par = NULL, lower = NULL, upper = NULL,
                   FUN.shp = 'vg', modality = 'uni',
                   par.shp = NULL, fit = 'both', weighting = 'var',
                   log = c('alfa', 'n', 'ks'), control = list(ncomplex = 15, reltol = 1e-7, tolsteps = 7),
                   suc.negativ = FALSE, integral = FALSE, L = NULL, log_Ku = TRUE, print.info = TRUE) {

  # prepare input
  if (!is.null(par.shp) & !is.list(par.shp)) { par.shp <- as.list(par.shp) }
  # tolower input
  if (!is.null(par.shp)) { names(par.shp) <- tolower(names(par.shp)) }
  modality <- tolower(modality)
  FUN.shp <- tolower(FUN.shp)
  log <- tolower(log)
  if (FUN.shp == 'vgm') {FUN.shp <-  'vg'}
  fit <- tolower(fit)

  if (is.list(par)) { par <- unlist(par)}
  if (is.list(upper)) { upper <- unlist(upper)}
  if (is.list(lower)) { lower <- unlist(lower)}

  if (weighting == 'stepwise') {weighting <- '2step'}
  stopifnot(weighting == 'stepwise' | weighting == '2step' | weighting == 'norm'| weighting == 'var')
  # if (integral == TRUE & is.null(L)) {
  #
  # }
  # remove Na from obs and suc -----------------------------------------------------------------------
  if (fit == 'ku' | fit == 'both') {
  suc.K.na <- !is.na(suc$K)
  suc$K <- suc$K[suc.K.na]
  obs$K <- obs$K[suc.K.na]
  obs.K.na <- !is.na(obs$K)
  suc$K <- suc$K[obs.K.na]
  obs$K <- obs$K[obs.K.na]
  }
  # remove NAs from th
  if (fit == 'swc' | fit == 'both') {
  suc.th.na <- !is.na(suc$th)
  suc$th <- suc$th[suc.th.na]
  obs$th <- obs$th[suc.th.na]
  obs.th.na <- !is.na(obs$th)
  suc$th <- suc$th[obs.th.na]
  obs$th <- obs$th[obs.th.na]
}
  # Add initial estimate if missing
  if(is.null(par)) {
    if (modality == 'uni') {
      if (fit == 'swc')   {
        if (FUN.shp == 'pdi' | FUN.shp == 'vg')   {
          par = c(ths = 0.5, thr = 0, alfa = 0.01, n = 1.2)
        }
        if (FUN.shp == 'bc')   {
          par = c(ths = 0.5, thr = 0, alfa = 0.01, lambda = 1.2)
        }

        }
      if (fit == 'both' | fit == 'ku')   {
        if (FUN.shp == 'pdi' | FUN.shp == 'vg')   {
          par = c(ths = 0.5, thr = 0, alfa = 0.01, n = 1.2, ks = 100, tau = 0.5)
          if (FUN.shp == 'pdi') {
            par <- c(par, omega = 0.0005)
          }
        }
        if (FUN.shp == 'bc')   {
          par = c(ths = 0.5, thr = 0, alfa = 0.01, lambda = 1.2, ks = 100, tau = 0.5)
        }
      }
    }
    if (modality == 'bi') {
      if (fit == 'swc')   {
        par = c(ths = 0.5, thr = 0, alfa = 0.01, n = 1.2, w2 = 0.05, alfa2 = 2, n2 = 2)
      }
      if (fit == 'both' | fit == 'ku')   {
      par = c(ths = 0.5, thr = 0, alfa = 0.01, n = 1.2, ks = 100, tau = 0.5, w2 = 0.05, alfa2 = 2, n2 = 2)
      if (FUN.shp == 'pdi') {
        par <- c(par, omega = 0.0005)
        }
      }
    }
    if (print.info == TRUE) {
    print('Initial parameter input (par) is missing and set to default:')
    print(par)
    }
  }
  # Add parameter boundaries if missing
  if(is.null(lower)) {
    if (modality == 'uni') {
      if (fit == 'swc')   {
        if (FUN.shp == 'pdi' | FUN.shp == 'vg')   {
          lower = c(ths = 0.4, thr = 0, alfa = 0.001, n = 1.01)
        }
        if (FUN.shp == 'bc')   {
          lower = c(ths = 0.5, thr = 0, alfa = 0.01, lambda = 0)
        }
      }

      if (fit == 'both' | fit == 'ku')   {
        if (FUN.shp == 'pdi' | FUN.shp == 'vg')   {
          lower = c(ths = 0.4, thr = 0, alfa = 0.001, n = 1.01, ks = 0.0001, tau = -2)
          if (FUN.shp == 'pdi') {
            lower <- c(lower, omega = 0.000000000001)
          }
        }
        if (FUN.shp == 'bc')   {
          lower = c(ths = 0.5, thr = 0, alfa = 0.01, lambda = 0.000000001, ks = 0.0001, tau = -2)
        }
      }
    }
    if (modality == 'bi') {
      if (fit == 'swc')   {
        lower = c(ths = 0.3, thr = 0, alfa = 0.001, n = 1.01, w2 = 0, alfa2 = 0.01, n2 = 1.01)
      }
      if (fit == 'both' | fit == 'ku')   {
      lower = c(ths = 0.3, thr = 0, alfa = 0.001, n = 1.01, ks = 0.0001, tau = -2, w2 = 0, alfa2 = 0.01, n2 = 1.01)
      if (FUN.shp == 'pdi') {
        lower <- c(lower, omega = 0.000000000001)
      }
      }
    }
    if (print.info == TRUE) {
    print('Lower parameter boundary input (lower) is missing and set to:')
    print(lower)
    }
  }
  # lower        = c(ths = 0.3, thr = 0, alfa = 1e-04, n = 1.01, ks = 1e-04, tau = -2, w2 = 0, alfa2 = 1e-04, n2 = 1.01)
  if(is.null(upper)) {
    if (modality == 'uni') {
      if (fit == 'swc')   {
        if (FUN.shp == 'pdi' | FUN.shp == 'vg')   {
          upper = c(ths = 1, thr = 0.4, alfa = 5, n = 10)
        }
        if (FUN.shp == 'bc')   {
          upper = c(ths = 1, thr = 0.4, alfa = 5, lambda = 10)
        }
      }
      if (fit == 'both' | fit == 'ku')   {
        if (FUN.shp == 'pdi' | FUN.shp == 'vg')   {
          upper = c(ths = 1, thr = 0.4, alfa = 5, n = 10, ks = 5000, tau = 5)
          if (FUN.shp == 'pdi') {
            upper <- c(upper, omega = 0.1)
        }
        }
        if (FUN.shp == 'bc')   {
          upper = c(ths = 1, thr = 0.4, alfa = 5, lambda = 10, ks = 5000, tau = 5)
        }
      }
    }
    if (modality == 'bi') {
      if (fit == 'swc')   {
        upper = c(ths = 1, thr = 0.4, alfa = 0.5, n = 10, w2 = 0.49, alfa2 = 5, n2 = 10)
      }
      if (fit == 'both' | fit == 'ku')   {
      upper = c(ths = 1, thr = 0.4, alfa = 0.5, n = 10, ks = 5000, tau = 10, w2 = 0.49, alfa2 = 5, n2 = 10)
      if (FUN.shp == 'pdi') {
        upper <- c(upper, omega = 0.1)
        }
      }
    }

    if (print.info == TRUE) {
    print('Upper parameter boundary input (upper) is missing and set to:')
    print(upper)
    }
  }

  if (any(names(par) %in% names(par.shp))) {
    par <- par[-which(names(par) %in% names(par.shp))]
    }

  if (any(names(lower) %in% names(par.shp))) {
  lower <- lower[-which(names(lower) %in% names(par.shp))]
  }
  if (any(names(upper) %in% names(par.shp))) {
    upper <- upper[-which(names(upper) %in% names(par.shp))]
  }
  if (print.info == TRUE) {
  print('For optimization logarithmized parameter:')
  print(log)
  }
  #
  # logarithmize parameter
  if (any(!is.na(log))) {
    lower[names(lower) %in% log] <- log10(lower[names(lower) %in% log])
    upper[names(upper) %in% log] <- log10(upper[names(upper) %in% log])
    par[names(par) %in% log] <- log10(par[names(par) %in% log])
    }
  if(fit == 'ku' | fit == 'swc') { weighting <- 'none'}
  # x <- par

  # Define objective function-------------------------------------------------------------------------
  OF <- function(x, obs = obs, suc = suc, fit = fit,
                 FUN.shp = FUN.shp, par.shp = par.shp, modality = modality, weighting = weighting,
                 log = log, integral = integral, L = L, log_Ku = log_Ku) {

  x[names(x) %in% log] <- 10^x[names(x) %in% log]
  par.shp <- c(x, par.shp)

  if (fit == 'both' | fit == 'swc') {
  # soil water content
  if (integral == FALSE) {
    th <- SWC(suc$th, par.shp = par.shp, FUN.shp = FUN.shp, suc.negativ = suc.negativ, modality = modality)
  }
    if (integral == TRUE) {
      suc.lb <- suc$th + (L/2) # suction lower boundary
      suc.ub <- suc$th - (L/2) # suction upper boundary
      th <- mapply(function(x, y, L) {
        1/L * abs(integrate(function(z) {
          SWC(z, par.shp = par.shp, FUN.shp = FUN.shp, suc.negativ = suc.negativ, modality = modality)
        }, lower = x, upper = y)$value) },
        x = suc.lb,
        y = suc.ub,
        L = L)
      th

    }

  res.th <- sum((th - obs$th)^2)
  }
  if (fit == 'both' | fit == 'ku') {
  # conductivity
  K <- Ku(suc = suc$K, par.shp = par.shp, FUN.shp = FUN.shp, suc.negativ = suc.negativ, modality = modality)
  if (log_Ku == TRUE) {
    res.K <- sum((log(K, base = 10) - log(obs$K, 10))^2)
  }
  if (log_Ku == FALSE) {
    res.K <- sum((K - obs$K)^2)
  }
  }

  if (fit == 'both') {
   ## class weights
  # Varianz
  if (weighting == 'var') {
    w.th <- 1/(sd(obs$th)^2)
    if (log_Ku == FALSE) {
      w.K <- 1/(sd(obs$K)^2)
    }
    if (log_Ku == TRUE) {
      w.K <- 1/(sd(log10(obs$K))^2)
    }
  }
  # normalized
  if (weighting == 'norm') {
    w.th <- 1/(max(obs$th) - min(obs$th))
    if (log_Ku == FALSE) {
      w.K <- 1/(max(obs$K) - min(obs$K))
    }
    if (log_Ku == TRUE) {
      w.K <- 1/(max(log10(obs$K)) - min(log10(obs$K)))
    }
  }
  res <- (w.K * res.K) + (w.th * res.th)
  }
  # ------------------------------------------------------------------------------
  if (fit == 'ku')   {res <- res.K}
  if (fit == 'swc')  {res <- res.th}
  if (fit == 'swc')  {res <- (1/(max(obs$th) - min(obs$th))) *res.th}
  res
}

if (weighting != '2step') {
ans <- SCEoptim(OF,
         par       = par,
         lower     = lower,
         upper     = upper,
         suc       = suc,
         obs       = obs,
         log       = log,
         FUN.shp   = FUN.shp,
         par.shp   = par.shp,
         modality  = modality,
         weighting = weighting,
         fit       = fit,
         control   = control,
         integral  = integral,
         log_Ku    = log_Ku,
         L         = L
         )

if (any(!is.na(log))) {
  log.par <- names(ans$par) %in% log
  ans$par[log.par] <- 10^ans$par[log.par]
}
ans$par <- as.list(c(ans$par, unlist(par.shp)))
}

  # 2-Step weighting-------------------------------------------------------------------------------
  if (fit == 'both' & weighting == '2step' ) {

    if (FUN.shp == 'vg') {
    par.fit1 <- par[!(names(par) == 'ks' | names(par) == 'tau')]
    par.fit2 <- par[(names(par) == 'ks' | names(par) == 'tau')]
    lower.fit1 <- lower[!(names(lower) == 'ks' | names(lower) == 'tau')]
    lower.fit2 <- lower[(names(lower) == 'ks' | names(lower) == 'tau')]
    upper.fit1 <- upper[!(names(upper) == 'ks' | names(upper) == 'tau')]
    upper.fit2 <- upper[(names(upper) == 'ks' | names(upper) == 'tau')]
    }
    if (FUN.shp == 'pdi') {
      par.fit1 <- par[!(names(par) == 'ks' | names(par) == 'tau' | names(par) == 'omega')]
      par.fit2 <- par[(names(par) == 'ks' | names(par) == 'tau'| names(par) == 'omega')]
      lower.fit1 <- lower[!(names(lower) == 'ks' | names(lower) == 'tau' | names(lower) == 'omega')]
      lower.fit2 <- lower[(names(lower) == 'ks'  | names(lower) == 'tau' | names(lower) == 'omega')]
      upper.fit1 <- upper[!(names(upper) == 'ks' | names(upper) == 'tau' | names(upper) == 'omega')]
      upper.fit2 <- upper[(names(upper) == 'ks'  | names(upper) == 'tau' | names(upper) == 'omega')]
    }
    if (FUN.shp == 'bc') {
      par.fit1 <- par[!(names(par) == 'ks' | names(par) == 'tau' | names(par) == 'omega')]
      par.fit2 <- par[(names(par) == 'ks' | names(par) == 'tau'| names(par) == 'omega')]
      lower.fit1 <- lower[!(names(lower) == 'ks' | names(lower) == 'tau' | names(lower) == 'omega')]
      lower.fit2 <- lower[(names(lower) == 'ks'  | names(lower) == 'tau' | names(lower) == 'omega')]
      upper.fit1 <- upper[!(names(upper) == 'ks' | names(upper) == 'tau' | names(upper) == 'omega')]
      upper.fit2 <- upper[(names(upper) == 'ks'  | names(upper) == 'tau' | names(upper) == 'omega')]
    }
    ans1 <- SCEoptim(OF,
                    par       = par.fit1,
                    lower     = lower.fit1,
                    upper     = upper.fit1,
                    suc       = suc,
                    obs       = obs,
                    log       = log,
                    FUN.shp   = FUN.shp,
                    par.shp   = par.shp,
                    modality  = modality,
                    weighting = weighting,
                    fit       = 'swc',
                    control   = control,
                    integral  = integral,
                    L         = L,
                    log_Ku    = log_Ku)


    if (any(!is.na(log))) {
      log.par <- names(ans1$par) %in% log
      ans1$par[log.par] <- 10^ans1$par[log.par]
    }


    ans1$par <- c(ans1$par, unlist(par.shp))

    ans2 <- SCEoptim(OF,
                     par       = par.fit2,
                     lower     = lower.fit2,
                     upper     = upper.fit2,
                     par.shp   = ans1$par,
                     suc       = suc,
                     obs       = obs,
                     log       = log,
                     FUN.shp   = FUN.shp,
                     modality  = modality,
                     weighting = weighting,
                     fit       = 'ku',
                     control   = control,
                     integral  = integral,
                     L         = L,
                     log_Ku    = log_Ku)

   if (any(!is.na(log))) {
     log.par <- names(ans2$par) %in% log
     ans2$par[log.par] <- 10^ans2$par[log.par]
   }

   ans <- ans1$control
   ans$par <- as.list(c(ans1$par, ans2$par, unlist(par.shp)))
   ans$par <- ans$par[!duplicated(ans$par)] #remove duplicated parameter resulting from par.shp
    }

# output-------------------------------------------------------------------------------
  ans$input <- list(log_Ku = log_Ku, obs = obs, suc = suc, FUN.shp = FUN.shp, modality = modality, fit = fit, suc.negativ = suc.negativ, weighting = weighting)

  if (fit == 'both' | fit == 'swc') {
    ans$RMSE.th <- RMSE(obs$th, SWC(suc = suc$th, par.shp = ans$par, FUN.shp = FUN.shp, modality = modality, suc.negativ = suc.negativ))
    if (print.info == TRUE) {
    print(paste0('RMSE_Th: ', ans$RMSE.th))
    }
  }
  if (fit == 'both' | fit == 'ku') {
    ans$RMSE.K <- RMSE(obs$K, Ku(suc$K, par.shp = ans$par, FUN.shp = FUN.shp, modality = modality, suc.negativ = suc.negativ), na.rm = T)
    if (print.info == TRUE) {
    print(paste0('RMSE_K: ', ans$RMSE.K))
    }
  }
attr(ans, "class") <- 'fitSHP'
ans
}
