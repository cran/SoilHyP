% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_fitSHP.R
\name{predict.fitSHP}
\alias{predict.fitSHP}
\title{Predict values using fitSHP object}
\usage{
\method{predict}{fitSHP}(object, suc = NULL, length.out = 100, suc.negativ = FALSE, ...)
}
\arguments{
\item{object}{object of class fitSHP}

\item{suc}{Suction/pressure heads for the prediction of the soil hydraulic properties}

\item{length.out}{output length if suc == NULL}

\item{suc.negativ}{set TRUE if suction/pressure heads are negative and FALSE if positive}

\item{...}{arguments for predict}
}
\description{
Predicts values using fitSHP object with calibrated paramter of SWC, KU or both depending on the fitSHP object
}
