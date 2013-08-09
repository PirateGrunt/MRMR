#' ProjectToDate
#' 

ProjectToDate = function(objTriangleModel, lOriginYears, AsOfDate)
{
  
  require(lubridate)
  
  DevelopmentInterval = months(12)
  
  mojo = lapply(lOriginYears, function(x){
    tz(AsOfDate) = tz(x$EvaluationDate)
    # TODO: add a check for a remainder
    ProjectionInterval = new_interval(x$EvaluationDate, AsOfDate)
    ProjectionIntervals = suppressMessages(ProjectionInterval / DevelopmentInterval)
    aList = replicate(ProjectionIntervals, x, simplify=FALSE)
    x = do.call("rbind", aList)
    DevIntervals = (1:ProjectionIntervals) * DevelopmentInterval
    x$EvaluationDate = x$EvaluationDate + DevIntervals
    # TODO: zero out stochastic columns
    x
  })
  
  df = do.call("rbind", mojo)
  df$DevelopmentLag = as.period(new_interval(df$OriginPeriodStart, df$EvaluationDate + days(1)))
  df$DevInteger = df$DevelopmentLag / DevelopmentInterval
  
  df
}

# mojo = new_interval(mdy("1/1/2009"), mdy("12/31/2009")) / days(1)