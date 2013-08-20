#' plot.Triangle
#' 
#' @export 
#' @include Triangle.R
#' 
#' @param 
#' x Triangle
#' y The measure being plotted
#' Predictor The variable used to predict the response
#' Group The name of the group column used to group the data. The default is OriginPeriodStart
#' Lines Draw lines to connect the observations?
#' FitLines Draw a line of best fit? Note that fit lines will have an intercept
#' 
plotTriangle = function(x, y
                        , Predictor, Response, Group = "OriginPeriodStart"
                        , Lines = TRUE, FitLines = FALSE, ...)
{
  require(ggplot2)
  
  # TODO: allow for null parameters
  df = x@TriangleData

  df$OriginGroup = factor(df[, Group])
  df$WhichMeasure = df[, Response]
  df$WhichTime = df[,Predictor]
  
  plotTitle = paste0(Response, " by ", Predictor, " grouped by ", Group)
  
  plt = ggplot(df, aes(x = WhichTime, y = WhichMeasure, group = OriginGroup, colour = OriginGroup)) 
  plt = plt + geom_point() + labs(title = plotTitle) + xlab(Predictor) + ylab(Response)
  
  if (Lines) plt = plt + geom_line() 
  if (FitLines) plt = plt + stat_smooth(method = lm, se = FALSE)
  
  plt
  return (plt)  
}

setMethod(f = "plot", signature = "Triangle", definition = plotTriangle)