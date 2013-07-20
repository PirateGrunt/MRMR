#' Create triangle development lags
#' 
#' If the triangle dataframe does not record development lags as lubridate periods, they must be created. 
#' Development lags may be established one of three ways:
#' 1. The development lags are passed in as lubridate periods.
#'    Everything's cool. The evaluation dates are established by adding the periods to the starting point
#'    of the origin periods.
#' 2. The development lags are passed in as integers, with a presumed time period.
#'    The program will establish lubridate period objects using the integers and time periods and then proceed 
#'    as above.
#' 3. An evaluation date is passed in. Here we must take the difference between the evaluation dates
#'    and the origin periods. We will assume months as the default period. The user may pass in another.
#' @export CreateDevelopmentLags
#' @param OriginStart Either a vector of date-time objects, or a vector of numbers indicating the year.
#' @param OriginEnd A vector of date-time objects. If this argument is supplied, it is assumed that OriginStart contains date-time objects.
#' @param OriginLength A Period object. These are easily created as shown in the example below. The default is a period of one year. If OriginStart and OriginEnd are supplied, this argument is ignored.
#' @param StartDay If OriginStart and OriginEnd are supplied, this argument is ignored.
#' @param StartMonth If OriginStart and OriginEnd are supplied, this argument is ignored.
#' @return A vector of intervals
#' @seealso \code{\link{CreateDevelopmentLags}}, \code{\link{CreateEvaluationDates}}

#' @examples
#' library(lubridate)
#' 
#' Sys.setenv(TZ='UTC')
#' # Case 1
#' DevelopmentLag = c(months(12), months(24), months(12))
#' DevelopmentLag
#' 
#' # Case 2
#' LagValues = c(12, 24, 12)
#' dPeriod = months(1)
#' DevelopmentLags = CreateDevelopmentLags(LagValues, DevelopmentPeriod = dPeriod)
#' DevelopmentLag
#' 
#' # Case 3
#' OriginStart = c(mdy("1/1/2000"), mdy("1/1/2000"), mdy("1/1/2001"))
#' OriginEnd = c(mdy("12/31/2000"), mdy("12/31/2000"), mdy("12/31/2001"))
#' OriginPeriods = CreateOriginPeriods(OriginStart, OriginEnd)
#' 
#' EvaluationDates = c(mdy("12/31/2000"), mdy("12/31/2001"), mdy("12/31/2001"))
#' DevelopmentLags = CreateDevelopmentLags(DevelopmentPeriod = months(1), EvaluationDates = EvaluationDates, OriginPeriods = OriginPeriods)
#' DevelopmentLag
#' 
#' DevelopmentPeriod = years(1)
#' DevelopmentLags = CreateDevelopmentLags(DevelopmentPeriod = months(1), EvaluationDates = EvaluationDates, OriginPeriods = OriginPeriods)
#' DevelopmentLag

CreateLagsFromIntegers = function(LagValues, DevelopmentPeriod = months(1))
{
  if (!is.period(DevelopmentPeriod)) stop ("A period object was not specified for the DevelopmentPeriod.")
  
  if (!is.integer(LagValues)){
    warning ("LagValues converted to integer.")
    LagValues = as.integer(LagValues)
  }
  
  DevelopmentLag = LagValues * DevelopmentPeriod
  
}

CreateLagsFromEvalDates = function(EvalDates, OriginPeriods, DevelopmentPeriod = months(1))
{
  if (!is.period(DevelopmentPeriod)) stop ("A period object was not specified for the DevelopmentPeriod.")
  
  if (is.null(OriginPeriods)) stop ("OriginPeriods were not specified.")
  
  DevelopmentInterval = new_interval(int_start(OriginPeriod), EvalDates + days(3))
  DevelopmentLag = DevelopmentInterval / DevelopmentPeriod
  DevelopmengLag = DevelopmentLag * DevelopmentPeriod
}

CreateDevelopmentLags = function(LagValues, DevelopmentPeriod = months(1), EvaluationDates = NULL, OriginPeriods = NULL)
{
  if (!is.period(DevelopmentPeriod)) stop ("A period object was not specified for the DevelopmentPeriod.")
  
  if (is.null(EvaluationDates))
  {
    return (CreateLagsFromIntegers(LagValues, DevelopmentPeriod))
  } else {
    return (CreateLagsFromEvalDates(EvaluationDates, OriginPeriods, DevelopmentPeriod))
  }
  
}