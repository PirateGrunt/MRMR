% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetTriangleData.R
\name{GetTriangleData}
\alias{GetTriangleData}
\title{GetTriangleData}
\usage{
GetTriangleData(Triangle, OriginPeriodStart = NULL, DevInteger = NULL,
  EvaluationDate = NULL, Measure)
}
\arguments{
\item{Triangle}{A Triangle object}

\item{OriginPeriodStart}{A vector of origin years. This parameter may be null.}

\item{DevInteger}{A vector of development integers. This parameter may be null.}

\item{EvaluationDate}{A vector of evaluation dates. This parameter may be null.}

\item{Measure}{A character vector with the names of measures to return.}
}
\value{
A data frame
}
\description{
This function will return data values from a triangle.
}

