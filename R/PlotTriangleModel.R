#' plotTriangleModel
#' 
#' @export plotTriangleModel
#' @include TriangleModel.R
#' @param 
#' x A TriangleModel object
#' y Nuisance parameter
#' @return None
#' @seealso \code{\link{PlotModelGoF}}, \code{\link{PlotModelFactors}}
#' 
plotTriangleModel = function(x, y,  ...)
{
  require(ggplot2)
  
  objModel = x
  qplot(objModel@Fit$coefficients)
  
}

setMethod("plot", c("TriangleModel"), definition = plotTriangleModel)