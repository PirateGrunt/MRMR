
setClass("TriangleProjection", 
         representation(ProjectionName = "character"
                        , TriangleModel = "TriangleModel"
                        , AsOfDate = "POSIXct"))

setValidity("TriangleProjection"
            , function(object){
                return (TRUE)
              }
            })

TriangleProjection = function(ProjectionName
                              , Model
                              , AsOfDate)
{
  # count the number of periods between
  Triangle = Model@BaseTriangle
  df = Triangle@TriangleData
  
  latestDate = max(df$EvaluationDate)
  span = as.duration(AsOfDate - latestDate)
  
  # still not sure how to calculate the number of intervals
  latestDate + tri@DevelopmentInterval
  
  mojo = as.period(new_interval(latestDate, AsOfDate))
  ProjectionIntervals = mojo / tri@DevelopmentInterval
  # TODO: add a check for a remainder
  
  DevIntervals = (1:ProjectionIntervals) * tri@DevelopmentInterval
  DevIntervals
  EvalDates = latestDate + DevIntervals
  EvalDates
  
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