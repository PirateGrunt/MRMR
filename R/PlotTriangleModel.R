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
#' @importFrom ggplot2 qplot
#' 
plotTriangleModel = function(objTriangleModel)
{
  qplot(objTriangleModel@Fit$coefficients)
  
}