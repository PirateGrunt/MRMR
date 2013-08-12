#' ProjectToDev
#' 

ProjectToDev = function(objTriangleModel, lOriginYears, MaxDev)
{
  
  require(lubridate)
  
  DevelopmentInterval = months(12)
  
  mojo = lapply(lOriginYears, function(x){
    ProjectionIntervals = MaxDev - x$DevInteger
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
  
  df = CreatePriors(df, c("CumulativePaid", "IncrementalPaid"), Groups = "All")
  
  df
}