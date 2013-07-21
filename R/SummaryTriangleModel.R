#' summaryTriangleModel
#' 
#' @export summary
#' @param OriginPeriod Vector of intervals
#' @return A vector of intervals
#' @seealso \code{\link{CreateCumulative}}, \code{\link{CreatePriors}}
#' @examples
#' 
summaryTriangleModel = function(object, ...)
{
  summary(object@Fit)
  
}

setMethod("summary", signature = "TriangleModel", definition = summaryTriangleModel)