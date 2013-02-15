#' @include Utils.R
#' 
library(lubridate)

checkTriangle = function(object)
{
  return (TRUE)
}

#' Triangle class
#' 
#' Triangle is an S4 class used to store aggregated loss data.
#' 
#' @name Triangle-class
#' @rdname Triangle-class
#' @exportClass Triangle
#' 
setClass("Triangle"
         , validity = checkTriangle
         , representation(TriangleData = "data.frame"
                        , TriangleName = "character"
                        , OriginPeriodType = "character"
                        , OriginPeriodInterval = "Period"
                        , DevelopmentInterval = "Period"))

#' Create a Triangle object
#' Triangle creates a new Triangle
#' @param TriangleData A dataframe
#' @param TriangleName The name of the triangle
#' @param OriginPeriodType A character string which describes the type of origin period. 
#' @param OriginPeriodInterval
#' @export Triangle
# User-friendly constructor
# This is a giant pile of code which basically does the following:
#   * Ensure that we have a proper date for the loss period start
#   * Ensure that we have a column for the development lag
#   * Once those have been established, create columns for loss period start and end, create a column
#       for development period (based on lubridate Period class), compute the evaluation date.
Triangle = function(TriangleData
                    , TriangleName
                    , OriginPeriodColumn
                    , OriginPeriodType = "accident"
                    , OriginPeriodInterval = years(1)
                    , DevelopmentColumn
                    , DevelopmentInterval = years(1)
                    , MeasureMeta)
{  
  
  # confirm that the loss period column exists. If not, we throw an error.
  if (!ColumnExists(TriangleData, OriginPeriodColumn))
  {
    stop ("The specified column for the origin period does not exist. Unable to create the triangle.")
  }
  
  require(lubridate)
  
  # if the loss period column is not a date, attempt to convert it.
  TriangleData$OriginPeriodStart = GetValidDate(TriangleData[, OriginPeriodColumn])
  
  # Now add the period to create the end date
  TriangleData$OriginPeriodEnd = TriangleData$OriginPeriodStart + OriginPeriodInterval - days(1)
  
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
    TriangleData$DevelopmentMultiplier = as.integer(TriangleData[,DevelopmentColumn])
  }
  
  TriangleData$DevelopmentMultiplier = TriangleData[,DevelopmentColumn]
  
  TriangleData$DevelopmentLag = TriangleData$DevelopmentMultiplier * DevelopmentInterval
  
  # if we can, construct an evaluation date based on loss period start and development interval
  TriangleData$EvaluationDate = TriangleData$OriginPeriodStart + TriangleData$DevelopmentLag - days(1)
  
  # Reorder the data
  # TODO: sort out the case when there are different grouping levels
  TriangleData = TriangleData[order(TriangleData$OriginPeriodStart, TriangleData$DevelopmentLag),]
  
  # Add a stem so that column names for newly created incremental/cumulative may be formed 
  MeasureStem = gsub("*Cumulative*", "", MeasureMeta$MeasureName)
  MeasureStem = gsub("*Incremental*", "", MeasureStem)
  MeasureMeta = cbind(MeasureMeta, MeasureStem = as.character(MeasureStem))
  
  # Create new columns so that every measure has both incremental and cumulative
  # I can alter this code so that premium and other such measures are not included
  whichRecords = subset(MeasureMeta, Cumulative != "Neither")
  NewCols = apply(as.matrix(whichRecords), 1, AdjustTriangleMeasures, TriangleData)
  NewCols = as.data.frame(do.call("cbind", NewCols))
  TriangleData = cbind(TriangleData, NewCols)
  
  # Now create columns for prior measures
  whichRecords = subset(MeasureMeta, Cumulative != "Neither")
  NewCols = apply(as.matrix(whichRecords), 1, ConstructPriorMeasures, TriangleData, DevelopmentInterval)
  NewCols = as.data.frame(do.call("cbind", NewCols))
  TriangleData = cbind(TriangleData, NewCols)
  
  row.names(TriangleData) = NULL
  
  tri = new("Triangle", TriangleData = TriangleData
            , TriangleName = TriangleName
            , OriginPeriodType = OriginPeriodType
            , OriginPeriodInterval = OriginPeriodInterval
            , DevelopmentInterval = DevelopmentInterval)
  
  return (tri)
}

AdjustTriangleMeasures = function(MetaRow, df)
{
  MeasureName = as.character(MetaRow[1])
  Cumulative = as.character(MetaRow[2])
  MeasureStem = as.character(MetaRow[3])
  
  #TODO: Adjust the split so that it will account for other grouping elements
  alist = split(df, as.factor(df$OriginPeriodStart))
  alist = lapply(alist, "[[", MeasureName)
  
  if (Cumulative == "Cumulative"){
    ColName = paste0("Incremental", MeasureStem)
    NewCol = lapply(alist, function(x){
      incr = c(x[1], diff(x))
      NewCol = data.frame(NewColumn = incr)})
  } else {
    ColName = paste0("Cumultive", MeasureStem)
    NewCol = lapply(alist, function(x){
      cumul = cumsum(x)
      NewCol = data.frame(NewColumn = incr)})
  }
  
  NewCol = as.data.frame(do.call("rbind", NewCol))
  NewCol = RenameColumn(NewCol, "NewColumn", ColName)
  
  return(NewCol)
}

ConstructPriorMeasures = function(MetaRow, df, DevelopmentInterval)
{
  MeasureStem = as.character(MetaRow[3])
  CumulativeCol = paste0("Cumulative", MeasureStem)
  IncrementalCol = paste0("Incremental", MeasureStem)
  NewColumnVal = df[,CumulativeCol] - df[,IncrementalCol]
  
  firstDevs = df$DevelopmentLag == DevelopmentInterval
  NewColumnVal[firstDevs] = NA
  df = data.frame(NewColumn = NewColumnVal)
  ColName = paste0("PriorCumulative", MeasureStem)
  df = RenameColumn(df, "NewColumn", ColName)
}