#'
#' Triangle class
#' 
#' @doctype class
#' 
#' @seealso \code{\link{Triangle}}
#' 
#' @name Triangle-class
#' @rdname Triangle-class
#' @exportClass Triangle
#' 
#' @description
#' Triangle is an S4 class used to store aggregated loss data. All triangles must have a defined set of OriginPeriods
#' , a defined set of DevelopmentIntervals and data along those axes. A triangle may carry additional dimensional 
#' information such as line of business, geographic region and so on.
#' 
#' @details
#' 
#' \strong{Triangle construction}
#' \strong{1. Construct an empty triangle}
#' myTriangle = Triangle()
#' 
#' One may then add temporal dimensions
#' 
#' OriginPeriodStart(myTriangle) = someVector
#' 
#' OriginPeriodStart(myTriangle, OriginLength) = someVector
#' 
#' Groups(myTriangle) 
#' StaticMeasures(myTriangle)
#' StochasticMeasures(myTriangle)
#' 
#' The above will return nothing 
#' 
#' \strong{2. Construct a triangle without any groups or measures}
#' myTriangle = Triangle(OriginPeriods, DevelopmentPeriods)
#' 
#' \strong{3. Construct a triangle without measures}
#' myTriangle = Triangle(OriginPeriods, DevelopmentPeriod, Groups)
#' 
#' \strong{4. Construct a triangle with all information needed}
#' myTriangle = Triangle(OriginPeriods, DevelopmentPeriod, Groups, StaticMeasures, StochasticMeasures)
#' 
#' In each case, we may supply a dataframe as the first argument using column references to pull appropriate data.
#' 
#' myTriangle = Triangle(df)
#' 
#' \strong{Triangle properties}
#' 
#' Accessor functions to get/set metainformation
#' data(Friedland)
#' TriangleName(Friedland)
#' TriangleName(Friedland) = "Friedland"
#' TriangleName(Friedland)
#' 
#' mojo = StaticMeasure(Friedland, "EP")
#' View(mojo)
#' 
#' mojo$EP_EUR = 1.37 * mojo$EP
#' 
#' StaticMeasure(Friedland) = mojo
#' View(Friedland@TriangleData)
#' 
#' data(Friedland)
#' mojo = StochasticMeasure(Friedland, "CumulativePaid")
#' mojo$PaidEUR = mojo$CumulativePaid * 1.37
#' StochasticMeasure(Friedland, TRUE) = mojo
#' myTriangle$MeasureOne = x
#' OriginPeriod(myTriangle) = someData
#' StocasticMeasure(myTriangle) = someData
#' StaticMeasure(myTriangle) = someData
#' StaticMeasure(myTriangle)
#' 
#' \strong{Triangle operations}
#' myTriangle = Collapse()
#' myTS = TriangleTimeSeries
#' 
#' @name MRMR
#' @aliases MRMR MRMR-package



#' is.Triangle
#' @description
#' Tests whether the object is a triangle
#' @return 
#' TRUE if the object is a triangle, FALSE if it is not
#' @export
#' @param object The object to be tested
is.Triangle = function(object)
{
  is(object, "Triangle")
}

checkTriangle = function(object)
{
  errors = character()
  if (length(errors) == 0) TRUE else errors
}

setClass("Triangle"
         , representation(TriangleData = "data.frame"
                          , TriangleName = "character"
                          , OriginPeriodType = "character"
                          , DevelopmentInterval = "Period"
                          , StaticMeasures = "character"
                          , StochasticMeasures = "character"
                          , Groups = "character")
#          , sealed = TRUE
         , validity = checkTriangle
)


#=================================
# Test data frame
#
# This data frame is used for testthat
TestDataFrame = function(){
  dfTest = data.frame(AccidentYear = c(2002, 2002, 2002, 2003, 2003, 2004)
             , Month = c(12, 24, 36, 12, 24, 12)
             , Paid = c(2318,  7932, 13822, 1743,  6240, 2221)
             , Reported = c(12811, 20370, 26656, 9651, 16995, 16995)
             , EP = c( 61183,  61183,  61183,  69175,  69175,  99322))
  
  dfTest
}