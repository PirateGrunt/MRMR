#' Plot
#' 
#' @export Plot
#' @param OriginPeriod Vector of intervals
#' @return A vector of intervals
#' @seealso \code{\link{CreateCumulative}}, \code{\link{CreatePriors}}
#' @examples
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

setMethod("plot", "Triangle", definition = plotTriangle)