#'
#' StaticMeasure class
#' 
#' @doctype class
#' 
#' @seealso \code{\link{StaticMeasureConstructor}}
#' 
#' @name StaticMeasure-class
#' @rdname StaticMeasure-class
#' @exportClass StaticMeasure
#' 
#' @description
#' 
#' @details
#' 
#' A static measure is effectively a time series, or set of time series. Note that because each value of a StaticMeasure
#' corresponds to an element of an OriginPeriod- which represents an interval of time- the StaticMeasure does not represent
#' an observation at a single point in time. In an insurance context, common static measures would include written or earned
#' premium, written or earned exposure, number of insured locations and the like. In-force premium or exposure would not 
#' constitute a StaticMeasure as this is a measure at a single point in time.
#' 
#' A static measure is so called to distinguish it from a stochastic measure. A stochastic measure also has a correspondence
#' to a single element of an OriginPeriod, but its value changes from one observation to the next. This would be the case for
#' number of claims, paid losses and the like. This would also be true for loss-sensitive premium such as exists in a retrospectively
#' rated policy.
#' 
#' \strong{Dimensions}
#' Each static measure may be assigned dimensional parameters which enable multiple sets of measures to be stored in the 
#' same object. This will, in turn, facilitate construction of a hierarchical analysis model. The dimensions may be
#' of any arbitraty complexity, irrespective of the number of elements of the StaticMeasure object. For example, one
#' may have a single StaticMeasure which uses more than one dimensional element; GL, prem/ops, California, for example. 
#' 
#' \strong{StaticMeasure construction}
#' \strong{1. Construct an empty StaticMeasure}
#' myOP = OriginPeriod()
#' 
#' startDates = seq(as.Date("2002/01/01"), as.Date("2013/12/31"), by="6 months")
#' x = OriginPeriod(startDates, Type="AccidentYear")

#' endDates = startDates + as.period(6, "months") - days(1)
#' y = MonthsBetween(startDates, endDates)
#' z = MeanMonths(y)

#' x = OriginPeriod(startDates, endDates, Type="AccidentYear")

is.StaticMeasure = function(object)
{
  is(object, "StaticMeasure")
}

checkStaticMeasure = function(object)
{
  errors = character()
  if (length(errors) == 0) TRUE else errors
}

setClass("StaticMeasure"
         , representation(OriginPeriod = "OriginPeriod"
                          , Measure = "data.frame"
                          , Dimension = "data.frame")
)

StaticMeasure = function(OriginPeriod, Measure, Dimension){
  stop("You must use a specific constructor method to create a StaticMeasure object. 
       Is it possible you forgot to provide a formal argument name? 
       For example, monikers and type must be explicity named in the function signature:
       OriginPeriod(start, end, Moniker=yourMoniker, Type=yourType")
}

setGeneric("StaticMeasure")

setMethod("StaticMeasure", signature=c(OriginPeriod = "OriginPeriod", Measure="data.frame", Dimension="data.frame")
          , definition=function(OriginPeriod, Measure, Dimension){
            
            if (nrow(Measure) / length(OriginPeriod) != nrow(Dimension)){
              stop("Measure must be the product of length(OriginPeriod) and nrow(Dimension)")
            }
            op = new("StaticMeasure"
                     , OriginPeriod = OriginPeriod
                     , Measure = Measure
                     , Dimension)
            op
})


AddGroup = function(StaticMeasure, df){
  
}

AddMeasure = function(StaticMeasure, df){
  
}

# setMethod("length", signature=c(x="StaticMeasure"), definition=function(x){
#   length(x@OriginPeriod)
# })
# 
# setMethod(f = "plot"
#           , signature(x="StaticMeasure", y="missing")
#           , definition = function(x, ...){
#             op = x@OriginPeriod
#             plot(op@StartDate, x@Measure, ...)
# })

