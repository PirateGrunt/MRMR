\name{TriangleProjection}
\alias{TriangleProjection}
\title{TriangleProjection}
\usage{
  TriangleProjection(objTriangleModel, ProjectToDev = TRUE,
    MaxDev = 10, AsOfDate)
}
\arguments{
  \item{objTriangleModel}{A TriangleModel object}

  \item{ProjectToDev}{Boolean indicating whether one is
  projecting to a maximum development interval. If this
  paramter is FALSE, there must be an argument for
  AsOfDate}

  \item{MaxDev}{The maximum development interval to which
  to project.}

  \item{AsOfDate}{The date to which one wants to project.}
}
\description{
  This will construct a TriangleProjection object
}

