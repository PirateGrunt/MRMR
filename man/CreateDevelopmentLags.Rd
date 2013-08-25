\name{CreateDevelopmentLags}
\alias{CreateDevelopmentLags}
\title{Create triangle development lags}
\usage{
  CreateDevelopmentLags(LagValues,
    DevelopmentPeriod = months(1), EvaluationDates = NULL,
    OriginPeriods = NULL, Verbose = TRUE)
}
\arguments{
  \item{LagValues}{Vector of development lags}

  \item{DevelopmentPeriod}{A period object indicating the
  common time period between evaluations}

  \item{EvaluationDates}{A vector of evaluation dates}

  \item{OriginPeriods}{A vector of origin periods}

  \item{Verbose}{Show warnings?}
}
\value{
  A vector of intervals
}
\description{
  If the triangle dataframe does not record development
  lags as lubridate periods, they must be created.
  Development lags may be established one of three ways: 1.
  The development lags are passed in as lubridate periods.
  Everything's cool. The evaluation dates are established
  by adding the periods to the starting point of the origin
  periods. 2. The development lags are passed in as
  integers, with a presumed time period.  The program will
  establish lubridate period objects using the integers and
  time periods and then proceed as above. 3. An evaluation
  date is passed in. Here we must take the difference
  between the evaluation dates and the origin periods. We
  will assume months as the default period. The user may
  pass in another.
}
\examples{
library(lubridate)

Sys.setenv(TZ='UTC')
# Case 1
DevelopmentLag = c(months(12), months(24), months(12))
DevelopmentLag

# Case 2
LagValues = c(12, 24, 12)
dPeriod = months(1)
DevelopmentLags = CreateDevelopmentLags(LagValues, DevelopmentPeriod = dPeriod)
DevelopmentLags

# Case 3
OriginStart = c(mdy("1/1/2000"), mdy("1/1/2000"), mdy("1/1/2001"))
OriginEnd = c(mdy("12/31/2000"), mdy("12/31/2000"), mdy("12/31/2001"))
OriginPeriods = CreateOriginPeriods(OriginStart, OriginEnd)

EvaluationDates = c(mdy("12/31/2000"), mdy("12/31/2001"), mdy("12/31/2001"))
DevelopmentLags = CreateDevelopmentLags(DevelopmentPeriod = months(1), EvaluationDates = EvaluationDates
                                       , OriginPeriods = OriginPeriods)
DevelopmentLags

DevelopmentPeriod = years(1)
DevelopmentLags = CreateDevelopmentLags(DevelopmentPeriod = months(1), EvaluationDates = EvaluationDates
                                       , OriginPeriods = OriginPeriods)
DevelopmentLag
}
\seealso{
  \code{\link{CreateDevelopmentLags}},
  \code{\link{CreateEvaluationDates}}
}

