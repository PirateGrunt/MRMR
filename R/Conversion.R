#' 
#' @param x A Triangle object
#' @aliases as.data.frame
setGeneric("as.data.frame")

#' @export
setMethod("as.data.frame", signature("Triangle"), function(x, ...){
  x@TriangleData
})

setMethod("as.data.frame", signature("OriginPeriod"), function(x, ...){
  y = data.frame(StartDate = x@StartDate
                 , EndDate = x@EndDate
                 , Moniker = x@Moniker)
  y
})

setMethod("as.data.frame", signature("StaticMeasure"), function(x, ...){
  g = x@Group
  op = as.data.frame(x@OriginPeriod)
  df = merge(op, g, all=TRUE, sort=FALSE)
  df = cbind(df, x@Measure)
  df
})

