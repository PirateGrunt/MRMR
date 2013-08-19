\name{newTriangle}
\alias{newTriangle}
\title{Create a Triangle object.}
\usage{
  newTriangle(OriginPeriods = NULL, OriginEnd = NULL,
    OriginLength = years(1), StartDay = 1, StartMonth = 1,
    DevelopmentLags = NULL, DevelopmentPeriod = months(1),
    EvaluationDates = NULL,
    OriginPeriodType = "Accident Year",
    TriangleName = NULL, TriangleData = NULL,
    StaticMeasures = NULL, StochasticMeasures = NULL,
    Groups = NULL, Cumulative = TRUE, Verbose = TRUE)
}
\arguments{
  \item{TriangleData}{A dataframe}

  \item{TriangleName}{The name of the triangle}

  \item{OriginPeriodType}{A character string which
  describes the type of origin period.}

  \item{OriginPeriodInterval}{}
}
\description{
  Create a Triangle object.
}

