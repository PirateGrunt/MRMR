#==============================================================================================================
# Triangle object
# The Triangle object houses aggregate claim data at regular evaluation dates.
#==============================================================================================================

setClass("Triangle", 
         representation(TriangleData = "data.frame"
                        , TriangleName = "character"
                        , LossPeriodType = "character"
                        , LossPeriodInterval = "Period"
                        , DevelopmentInterval = "Period"))

setValidity("Triangle"
            , function(object){
              ValidPeriodTypes = c("annual", "quarterly", "semiannual", "monthly")
              if (is.na(match(tolower(object@LossPeriodType), ValidPeriodTypes))){
                return ("Invalid loss period type")
              }else{
                return (TRUE)
              }
            })

setMethod("summary", "Triangle",
          function(object, ...) {
            print(paste("Loss period type = ", object@LossPeriodType))
            print(paste("Loss period interval = ", object@LossPeriodInterval))
          })

setMethod("show", "Triangle"
          , function(object){
            cat("This is a loss triangle\n")
            cat("Its name is", object@TriangleName, "\n")
            cat("Its columns are", colnames(object@TriangleData), "\n")
            print(head(object@TriangleData))
          })

.plotTriangle = function(tri)
{
  require(ggplot2)
  df = tri@TriangleData
  df$LossPeriod = as.factor(df$LossPeriod)
  
  #PaidOrIncurred = ifelse(Paid, "Paid", "Incurred")
  PaidOrIncurred = "Paid"
  #CumulativeOrIncremental = ifelse(Cumulative, "Cumulative", "Incremental")
  CumulativeOrIncremental = "Cumulative"
  LossValue = paste0(CumulativeOrIncremental, PaidOrIncurred)
  
  df$LossValue = df[,LossValue]
  
  PlotTitle = paste(tri@TriangleName, tolower(CumulativeOrIncremental), tolower(PaidOrIncurred), "loss by accident year")
  
  plt = ggplot(df, aes(x = DevelopmentLag, y = LossValue, group = LossPeriod, colour = LossPeriod)) 
  plt = plt + geom_line(show_guide=FALSE) + geom_point(show_guide=FALSE) + labs(title=PlotTitle)
  
  print(plt)
  
  return (plt)
}

setMethod("plot", "Triangle"
          , function(x,y,...){
            .plotTriangle(x)
          })

# User-friendly constructor
Triangle = function(TriangleData, TriangleName, LossPeriodType, LossPeriodInterval, DevelopmentInterval)
{
  # a slew of logic to ensure that the data within the df is consistent with the loss and dev intervals
  tri = new("Triangle", TriangleData = TriangleData
            , TriangleName = TriangleName
            , LossPeriodType = LossPeriodType
            , LossPeriodInterval = LossPeriodInterval
            , DevelopmentInterval = DevelopmentInterval)
  
  return (tri)
}

is.Triangle = function(object)
{
  is(object, "Triangle")
}




# CreateProjection(dfTriangle, NumberOfPeriods, Response, IntervalWidth)
# {
#   
#   return (dfProjection)
# }

# setMethod(
#   f = "Initialize"
#   , signature = "Triangle"
#   , definition = function(.Object, TriangleData
#                           , TriangleName
#                           , LossPeriodType
#                           , LossPeriodInterval
#                           , DevelopmentInterval){
#     cat("Initialize triangle")
#     .Object@TriangleData = TriangleData
#     .Object@LossPeriodType = LossPeriodType
#     .ObjectLossPeriodInterval = LossPeriodInterval
#   }
#   )