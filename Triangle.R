#==============================================================================================================
# Triangle object
# The Triangle object houses aggregate claim data at regular evaluation dates.
#==============================================================================================================

ColumnExists = function(df, ColumnName)
{  
  ColumnExists = sum(colnames(df) %in% ColumnName)
  
  return (ColumnExists != 0)
}

setClass("Triangle", 
         representation(TriangleData = "data.frame"
                        , TriangleName = "character"
                        , LossPeriodType = "character"
                        , LossPeriodInterval = "Period"
                        , DevelopmentInterval = "Period"))

setValidity("Triangle"
            , function(object){
              # the dataframe must have the following named columns:
                # LossPeriodStart
                # LossPeriodEnd
                # EvaluationDate
                # DevelopmentInterval
                # one of the following: Incremental or cumulative PaidLoss, ReportedLoss, CaseReserve, ReportedClaims, ClosedClaims
              ValidPeriodTypes = c("accident", "policy", "report")
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

setGeneric("LatestDiagonal", function(x){
  standardGeneric("LatestDiagonal")
})

setMethod("LatestDiagonal", "Triangle", function(x){
  df = x@TriangleData
  latestDev = max(df$DevelopmentYear)
  df.latest = subset(df, DevelopmentYear == latestDev)
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
          , function(x, y, ...){
            .plotTriangle(x)
          })

#==================================================================================================
# User-friendly constructor
# This is a giant pile of code which basically does the following:
#   * Ensure that we have a proper date for the loss period start
#   * Ensure that we have a column for the development lag
#   * Once those have been established, create columns for loss period start and end, create a column
#       for development period (based on lubridate Period class), compute the evaluation date.
Triangle = function(TriangleData
                    , TriangleName
                    , LossPeriodType = "accident"
                    , LossPeriodInterval = years(1)
                    , DevelopmentInterval = years(1)
                    , LossPeriodColumn
                    , DevelopmentColumn)
{  
  
  # confirm that the loss period column exists. If not, we throw an error.
  if (!ColumnExists(TriangleData, LossPeriodColumn))
  {
    stop ("The specified column for the loss period does not exist. Unable to create the triangle.")
  }
  
  require(lubridate)
  
  # if the loss period column is not a date, attempt to convert it.
  if (!is.POSIXt(TriangleData[,LossPeriodColumn])){
    attemptConvert = ymd(TriangleData[,LossPeriodColumn])
    if (sum(is.Date(attemptConvert)) ==0 ){
      attemptConvert = ydm(TriangleData[,LossPeriodColumn])
      if (sum(is.Date(attemptConvert)) ==0 ){
        attemptConvert = mdy(TriangleData[,LossPeriodColumn])
        if (sum(is.Date(attemptConvert)) ==0 ){
          attemptConvert = myd(TriangleData[,LossPeriodColumn])
          if (sum(is.Date(attemptConvert)) ==0 ){
            attemptConvert = dmy(TriangleData[,LossPeriodColumn])
            if (sum(is.Date(attemptConvert)) ==0 ){
              attemptConvert = dym(TriangleData[,LossPeriodColumn])
              if (sum(is.Date(attemptConvert)) ==0 ){
                stop ("Loss period column is not in date form and all values cannot be converted to a date. Unable to create the triangle")
              }
            }
          }
        }
      }
    }
  }
  
  # If we've made it this far, we can safely convert the loss period start date
  if (!is.POSIXt(TriangleData[, LossPeriodColumn])){
    TriangleData$LossPeriodStart = attemptConvert
  } else {
    TriangleData$LossPeriodStart = TriangleData[, LossPeriodColumn]
  }
  
  # Now add the period to create the end date
  TriangleData$LossPeriodEnd = TriangleData$LossPeriodStart + LossPeriodInterval - days(1)
  
  # it's possible that the user has fed data with overlap. 
  # Annual data with two start dates in the same year and annual development period, zB.
  # I might decide to check for this and throw a warning, but for now, I'll just blame the user.
  
  # Now confirm that the development lag column exists. If not, we throw an error.
  if (!ColumnExists(TriangleData, DevelopmentColumn))
  {
    stop ("The specified column for the development does not exist. Unable to create the triangle.")
  }
  
  # For now, development periods must be integer.
  if (!is.integer(TriangleData[,DevelopmentColumn])){
    TriangleData[,DevelopmentColumn] = as.integer(TriangleData[,DevelopmentColumn])
  }
  
  TriangleData$Development = TriangleData[,DevelopmentColumn] * DevelopmentInterval
  
  # if we can, construct an evaluation date based on loss period start and development interval
  TriangleData$EvaluationDate = TriangleData$LossPeriodStart + TriangleData$Development - days(1)
  
  tri = new("Triangle", TriangleData = TriangleData
            , TriangleName = TriangleName
            , LossPeriodType = LossPeriodType
            , LossPeriodInterval = LossPeriodInterval
            , DevelopmentInterval = DevelopmentInterval)
  
  return (tri)
}
#== End Triangle constructor ======================================================================

is.Triangle = function(object)
{
  is(object, "Triangle")
}

# setGeneric("ProjectTriangle", function(x){
#   standardGeneric("ProjectTriangle")
# })
# 
# setMethod("ProjectTriangle", "Triangle", function(Triangle, NumberOfPeriods){
#   df = LatestDiagonal(Triangle)
#   return(df)
# })

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