#' CompleteTriangle
#' 
#' This function will bind the projected values to the base triangle data for a "complete" triangle. This facilitates comparison of ultimates between multiple TriangleModels.
#' 
#' @export CompleteTriangle
#' @param objProjection
#' @return A data frame
#' 
CompleteTriangle = function(objProjection)
{
  objTriangle = objProjection@TriangleModel@Triangle
  dfBase = objTriangle@TriangleData
  
  dfProjection = objProjection@ProjectionData
  
  commonCols = intersect(colnames(dfBase), colnames(dfProjection))
  
  dfAll = rbind(dfBase[,commonCols], dfProjection[,commonCols]) 
  
  dfAll
}
