% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sat_old.R
\name{Sat_old}
\alias{Sat_old}
\title{Capillary saturation function}
\usage{
Sat_old(suc, par.shp, modality = c("uni"), suc.negativ = TRUE)
}
\arguments{
\item{suc}{Suction/pressure heads. Negative if suc.negativ = TRUE}

\item{par.shp}{named parameter in list or vector}

\item{modality}{pore size distribution ('uni' or 'bi')}

\item{suc.negativ}{set TRUE if suction/pressure heads are negative and FALSE if positive}
}
\description{
Capillary saturation function of van Genuchten for unimodal or bimodal pore space distributions.
}
\details{
\describe{\item{par.shp:}{
alfa [1/L]: van Genuchten shape parameter \cr
n [-]: van Genuchten shape parameter \cr
m [-]: shape parameter (m = 1-(1/n) if missing)}
\item{par.shp (additional for bimodal (modality = 'bi')):}{
w2 [-]: weigthing between pore space distribution \cr
alfa2 [1/L]: van Genuchten parameter alfa for second pore space distribution \cr
n2 [-]: van Genuchten parameter n for second pore space distribution}}
}
\references{
Van Genuchten, M. T. (1980). A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil science society of America journal, 44(5), 892-898.

Durner, W. (1994). Hydraulic conductivity estimation for soils with heterogeneous pore structure. Water Resources Research, 30(2), 211-223.
}
