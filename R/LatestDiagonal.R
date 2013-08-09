#' adf
#' 
LatestDiagonal = function(df)
{
#  df = objTriangle@TriangleData
  lOriginYear = dlply(df, "OriginPeriodStart")
  lOriginYear = lapply(lOriginYear, function(x) {
    whichRows = x$EvaluationDate == max(x$EvaluationDate)
    latest = x[whichRows,]
  })
  
  dfLatest = do.call("rbind", lOriginYear)
  
  dfLatest
  
}