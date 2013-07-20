#' Plot
#' 
#' @export Plot
#' @param OriginPeriod Vector of intervals
#' @return A vector of intervals
#' @seealso \code{\link{CreateCumulative}}, \code{\link{CreatePriors}}
#' @examples
#' 
plotTriangleModel = function(x, y,  ...)
{
  require(ggplot2)
  
  objModel = x
  plot(objModel@Fit$coefficients)
  
}

setMethod("plot", c("TriangleModel"), definition = plotTriangleModel)