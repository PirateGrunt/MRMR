#' summaryTriangleModel
#' 
#' @export summaryTriangleModel
#' @param OriginPeriod Vector of intervals
#' @return A vector of intervals
#' @seealso \code{\link{CreateCumulative}}, \code{\link{CreatePriors}}
#' 
summaryTriangleModel = function(object, ...)
{
  summary(object@Fit)
  
}

setMethod("summary", signature = "TriangleModel", definition = summaryTriangleModel)