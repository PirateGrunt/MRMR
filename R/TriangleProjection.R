#' @include Triangle.R
NULL

setClass("TriangleProjection"
         , representation(TriangleModel = "TriangleModel"
                          , ProjectToDev = "logical"
                          , MaxDev = "numeric"
                          , AsOfDate = "POSIXct"
                          , ProjectionData = "data.frame"))

TriangleProjection = function(objTriangleModel
                              , ProjectToDev = TRUE
                              , MaxDev = 10
                              , AsOfDate = mdy("12/31/2010"))
{
  dfLatest = LatestDiagonal(objTriangleModel@ModelData)
  
  lOriginYear = dlply(dfLatest, "OriginPeriodStart")
  
  if (ProjectToDev){
    dfProjection = ProjectToDev(objTriangleModel, lOriginYear, MaxDev)
  } else {
    dfProjection = ProjectToDate(objTriangleModel, lOriginYear, AsOfDate)
  }
  
  if (objTriangleModel@FitCategory == "DevInteger"){
    dfProjection$FitCategory = TailFunction(dfProjection$DevInteger, objTriangleModel@Tail)
    dfProjection$FitCategory = factor(dfProjection$FitCategory)
  }
  
  dfProjection[objTriangleModel@Response] = ProjectValues(objTriangleModel, dfProjection)
  
  row.names(dfProjection) = NULL
  
  proj = new("TriangleProjection"
             , TriangleModel = objTriangleModel
             , ProjectToDev = ProjectToDev
             , MaxDev = MaxDev
             , AsOfDate = AsOfDate
             , ProjectionData = dfProjection)
  
  proj
}