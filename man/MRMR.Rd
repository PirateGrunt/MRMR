\docType{package}
\name{MRMR}
\alias{MRMR}
\alias{MRMR-package}
\title{Multivariate Regression Models for Reserving}
\description{
  MRMR
}
\details{
  MRMR allows an actuary to create sets of loss data and
  forecast liabilities.

  Triangle

  A Triangle is a collection of aggregate loss data. All
  triangles must have a defined set of OriginPeriods, a
  defined set of DevelopmentIntervals and data along those
  axes. A triangle may carry additional descriptive
  information such as line of business, geographic region
  and so on.

  TriangleModel

  A TriangleModel is a statistical model fit to triangle
  data. The formula may be defined by the user and will
  generally be a linear or generalized linear model. A
  triangle may have more than one model. Indeed, it usually
  will.

  TriangleProjection

  A TriangleProjection is a prediction based on a
  TriangleModel. A TriangleModel may have more than one
  projection.

  \code{\link{CompanyData}} is a data set which contains
  NAIC filed data for a select set of companies.
}

