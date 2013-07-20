#' @include Triangle.R
NULL

setClass("TriangleProjection", 
         representation(ProjectionName = "character"
                        , TriangleModel = "TriangleModel"
                        , AsOfDate = "POSIXct"))

setValidity("TriangleProjection"
            , function(object){
                return (TRUE)
              }
            )

GetProjectionDates = function(rowNum, df, ProjectionDate, DevelopmentInterval, ResponseName){
  
  # check that the df holds a column called "EvaluationDate"
  # check that the DevelopmentInterval is positive
  
  require(lubridate)
  df = df[rowNum,]
  ProjectionInterval = new_interval(df$EvaluationDate + days(1), ProjectionDate)
  # TODO: add a check for a remainder
  ProjectionIntervals = ProjectionInterval / DevelopmentInterval
  print(ProjectionIntervals)
  
  aList = replicate(ProjectionIntervals, df, simplify=FALSE)
  df = do.call("rbind", aList)
  
  DevIntervals = (1:ProjectionIntervals) * DevelopmentInterval
  
  df$EvaluationDate = df$EvaluationDate + DevIntervals
  df$DevelopmentLag = as.period(new_interval(df$OriginPeriodStart, df$EvaluationDate + days(1)))
  df$DevelopmentMultiplier = df$DevelopmentLag / DevelopmentInterval
  df[,ResponseName] = 0
  
  return (df)
}

GetProjectedValues = function(df, fit, PredictorName, CategoryName)
{
  if (CategoryName == "none"){
    newX = df[, colnames(df) %in% PredictorName]
    } else {
    newX = df[, colnames(df) %in% c(PredictorName, CategoryName)]
  }
  print(summary(newX))
  mojo = predict(fit, newX)
  return (mojo)
}

TriangleProjection = function(ProjectionName
                              , Model
                              , ProjectTo = "ProjectDate"
                              , ProjectDate
                              , ProjectLag = NULL)
{
  tri = Model@Triangle
  
  df.latest = LatestDiagonal(tri)
  
  rowNums = 1:nrow(df.latest)
  mojo = lapply(rowNums, GetProjectionDates
                        , df.latest
                        , ProjectToDate
                        , tri@DevelopmentInterval
                        , Model@ResponseName)
  dfResults = do.call("rbind", mojo)
  
  proj = new("TriangleProjection"
             , ProjectionName = ProjectionName
             , TriangleModel = Model
             , AsOfDate = AsOfDate)
  
  return (proj)
}