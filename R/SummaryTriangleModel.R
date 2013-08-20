#' summaryTriangleModel
#' 
#' @export summaryTriangleModel
#' @include TriangleModel.R
#' 
#' @param x TriangleModel object
#' 
#' @return A vector of intervals
#' 
#' @seealso \code{\link{CreateCumulative}}, \code{\link{CreatePriors}}
#' 
summaryTriangleModel = function(x, ...)
{
  summary(x@Fit)
  
}

#setMethod("summary", signature = "TriangleModel", definition = summaryTriangleModel)
setMethod("summary", definition = summaryTriangleModel)