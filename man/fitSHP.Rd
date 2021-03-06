% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitSHP.R
\name{fitSHP}
\alias{fitSHP}
\title{Fit soil hydraulic properties}
\usage{
fitSHP(
  obs = list(th = NULL, K = NULL),
  suc = list(th = NULL, K = NULL),
  par = NULL,
  lower = NULL,
  upper = NULL,
  FUN.shp = "vg",
  modality = "uni",
  par.shp = NULL,
  fit = "both",
  weighting = "var",
  log = c("alfa", "n", "ks"),
  control = list(ncomplex = 15, reltol = 1e-07, tolsteps = 7),
  suc.negativ = FALSE,
  integral = FALSE,
  L = NULL,
  log_Ku = TRUE,
  print.info = TRUE
)
}
\arguments{
\item{obs}{list with named observations (th for water content and K for unsaturated hydraulic conductivity data)}

\item{suc}{list of named suctions corresponding to th and/or K}

\item{par}{a numeric vector of initial parameter values (see also \code{\link{SCEoptim}}). If missing default values are set.}

\item{lower}{lower bounds on the parameters. Should be the same length as \code{par} and as \code{upper}, or length 1 if a bound applies to all parameters. If missing default values are set.}

\item{upper}{upper bounds on the parameters. Should be the same length as \code{par} and as \code{lower}, or length 1 if a bound applies to all parameters. If missing default values are set.}

\item{FUN.shp}{Funktion for soil hydraulic properties (vG, PDI or bc) (see \code{\link{SWC}} or \code{\link{Ku}})}

\item{modality}{pore size distribution ('uni' or 'bi')}

\item{par.shp}{fixed parameter value named in list or vector}

\item{fit}{fit parameter for 'SWC', 'Ku' or 'both' simultaneous.}

\item{weighting}{weighting between SWC and Ku. Used if fit == both ('var', 'norm' or '2step').}

\item{log}{names of parameter in list or vector which should be logarithmized during optimization}

\item{control}{a list of options as in \code{optim()}, see \code{\link{SCEoptim}}}

\item{suc.negativ}{set TRUE if suction/pressure heads are negative and FALSE if positive}

\item{integral}{th as point value vs. suc(h) (FALSE) or th as mean water content over the column divided by the height (L) vs. suc(h) (TRUE) (see details).}

\item{L}{sample height [cm]. Only needed for integral == TRUE}

\item{log_Ku}{logarithmize Ku in the objective function and for weighting (TRUE).}

\item{print.info}{print information about default values for par, lower, and upper if missing or fitting accuracy (TRUE or FALSE)}
}
\value{
"fitSHP" class
}
\description{
Estimate parameter for soil water retention (SWC) and/or unsaturated hydraulic conductivity function (Ku) using Shuffled Complex Evolution (SCE) optimisation. Parameter can be estimated for van Genuchten-Mualem (vg or vgm) or Peters-Durner-Iden (PDI) parameterisation of the soil hydraulic properties.
}
\details{
\describe{\item{weigthing:\cr}{
var: th and K are weighted in the objective fuction by the measurement variance  \cr
norm: th and K are normed in objective fuction\cr
stepwise: the parameter for th are fitted first and the remaining parameter for K afterwards (2step works aswell)}}
\describe{\item{log:\cr}{
The use of log is suggested for paramter 'alfa', 'n' and 'ks' for modality == 'uni'. For modality 'bi' additional 'alfa2' and 'n2' and for Fun.shp == 'pdi' additional 'omega'.
Parameter in output ($par) are not returned logarithmized. \cr
Delfault paramter values for par, lower and upper are logarithmized automatatically \cr
If not the default values for par, lower and upper are taken, parameter which are named in 'log' must be scaled by the user in par, lower and upper.
}
}
\describe{\item{integral:\cr}{
The "integral" method is suggested from Peters and Durner (2008, 2015) to fit parameter on data from
experiments were water contents are measured as mean water contents (e.g. simplified evaporation method or multi-step outflow experiments).
Under the assumption that the water content is distributed linear over the column, the measured
mean water content of the column is the integral over the whole column divided by the column length (L).
Under hydraulic equilibrium this is equal to the integral of the retention function over the matric
heads from the lower boundary to the upper boundary of the column divided by the height of the column (Peters 2008, 2015). \cr
\cr
integral == TRUE can be very slow.

}}
}
\examples{
\dontrun{
data('dataSHP')
# -------------------------------------------------------------------
# fit Soil Hydraulic Properties (SHP)
# -------------------------------------------------------------------
ans <- fitSHP(obs         = list(th = dataSHP$th, K = dataSHP$Ku),
             suc          = list(th = dataSHP$suc, K = dataSHP$suc),
             FUN.shp      = 'vg',
             modality     = 'uni',
             par.shp      =  NULL,
             fit          = 'both',
             weighting    = 'var',
             log          = c('alfa', 'n', 'ks'),
             control      = list(ncomplex = 15, reltol = 1e-07,tolsteps = 7),
             suc.negativ  = TRUE,
             integral     = FALSE,
             L            = 0,
             print.info   = TRUE
)
ans$par
plot(ans)
# --------------------------------------------------------------------
# bimodal van Genuchten-Mualem
ans <- fitSHP(obs         = list(th = dataSHP$th, K = dataSHP$Ku),
             suc          = list(th = dataSHP$suc, K = dataSHP$suc),
             FUN.shp      = 'vg',
             modality     = 'bi',
             par.shp      =  c(),
             fit          = 'both',
             weighting    = 'var',
             log          = c('alfa', 'n', 'ks', 'alfa2', 'n2'),
             suc.negativ  = TRUE,
             integral     = FALSE,
             L            = 0,
             print.info   = TRUE,
             control      = list(ncomplex = 15, reltol = 1e-07,tolsteps = 7)
)
ans$par
plot(ans)
}
}
\references{
Peters, A., & Durner, W. (2008). Simplified evaporation method for determining soil hydraulic properties. Journal of Hydrology, 356(1), 147-162.

Peters and Durner (2015). SHYPFIT 2.0 User’s Manual

Peters, A., Iden, S. C., & Durner, W. (2015). Revisiting the simplified evaporation method: Identification of hydraulic functions considering vapor, film and corner flow. Journal of Hydrology, 527, 531-542.
}
\seealso{
\code{\link{SCEoptim}}, \code{\link{SWC}}, \code{\link{Ku}}
}
\author{
Ullrich Dettmann
}
