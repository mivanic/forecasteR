% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecasteR.R
\name{modelToFormula}
\alias{modelToFormula}
\title{Turn an estimated model into a formula}
\usage{
modelToFormula(model)
}
\arguments{
\item{model}{An estimated model which includes the original call and the estimated coefficients}
}
\value{
A formula
}
\description{
\code{modelToFormula} returns a formula with coefficients placed as factors.
}
