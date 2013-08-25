\name{newTriangleModel}
\alias{newTriangleModel}
\title{Create a new TriangleModel object}
\usage{
  newTriangleModel(Triangle, Response, Predictor,
    FitCategory, Intercept = FALSE, Alpha = NULL,
    Tail = NULL)
}
\arguments{
  \item{Triangle}{A Triangle object}

  \item{Response}{Character vector indicating the response
  being measured}

  \item{Predictor}{Character vector indicating the variable
  used to predict the response}

  \item{FitCategory}{Character vector indicating the column
  used to categorize the predictor variable}

  \item{Intercept}{Boolean indicating whether or not to
  include an intercept}

  \item{Alpha}{Numeric indicating the parameter used to
  weight the predictors}

  \item{Tail}{Integer indicating the maximum development
  lag for grouping}
}
\description{
  Create a new TriangleModel object
}

