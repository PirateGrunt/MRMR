PivotTriangle = function(objTriangle, whichMeasure, whichRows, whichColumns)
{
  require(reshape2)
  
  df = objTriangle@TriangleData[,c(whichMeasure, whichRows, whichColumns)]
  melt
  
}