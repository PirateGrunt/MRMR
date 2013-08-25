#' plotTriangleModel
#' 
#' @export plotTriangleModel
#' @include TriangleModel.R
#' @param objTriangleModel A TriangleModel object
#' 
#' @return None
#' 
#' @seealso \code{\link{PlotModelGoF}}, \code{\link{PlotModelFactors}}
#' 
plotTriangleModel = function(objTriangleModel)
{
  require(ggplot2)
  
  qplot(objTriangleModel@Fit$coefficients)
  
}