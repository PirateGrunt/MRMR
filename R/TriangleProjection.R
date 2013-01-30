
setClass("TriangleProjection", 
         representation(ProjectionName = "character"
                        , TriangleModel = "TriangleModel"
                        , AsOfDate = "POSIXct"))

setValidity("TriangleProjection"
            , function(object){
                return (TRUE)
              }
            })

GetProjectionDates = function(LatestEvalDate, ProjectionDate, DevelopmentInterval){
  
  require(lubridate)
  
  ProjectionInterval = as.period(new_interval(LatestEvalDate, ProjectionDate))
  # TODO: add a check for a remainder
  ProjectionIntervals = ProjectionInterval / DevelopmentInterval
  DevIntervals = (1:ProjectionIntervals) * DevelopmentInterval
  EvalDates = LatestEvalDate + DevIntervals
  
  return (as.data.frame(EvalDates))
}

TriangleProjection = function(ProjectionName
                              , Model
                              , AsOfDate)
{
  Model = chainLadder
  Triangle = Model@BaseTriangle
  df = Triangle@TriangleData
  
  df.latest = LatestDiagonal(Triangle)
  latestDate = df.latest$EvaluationDate
  
  mojo = as.data.frame(GetProjectionDates(latestDate[1], AsOfDate, Triangle@DevelopmentInterval))
  
  mojo = lapply(latestDate, GetProjectionDates, AsOfDate, Triangle@DevelopmentInterval)
  dfResults = do.call("rbind", mojo)
  
  df = 
  
  proj = new("TriangleProjection"
             , ProjectionName = ProjectionName
             , TriangleModel = Model
             , AsOfDate = AsOfDate)
  
  return (proj)
}

#==============
# summary will have ability to write TEX output.
# setMethod("summary", "TriangleProjection",
#           function(object, ...) {
#             print(paste("Loss period type = ", object@LossPeriodType))
#             print(paste("Loss period interval = ", object@LossPeriodInterval))
#           })
# 
# setMethod("plot", "TriangleProjection",
#           function(x, y, ...) {
#             
#           })