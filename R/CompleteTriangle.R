
CompleteTriangle = function(objProjection)
{
  objTriangle = objProjection@TriangleModel@Triangle
  dfBase = objTriangle@TriangleData
  
  dfProjection = objProjection@ProjectionData
  
  commonCols = intersect(colnames(dfBase), colnames(dfProjection))
  
  dfAll = rbind(dfBase[,commonCols], dfProjection[,commonCols]) 
  
  dfAll
}
