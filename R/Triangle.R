#'
#' Triangle class
#' @include NewGenerics.R
#' @include OriginPeriod.R
#' @include StaticMeasure.R
#' @include StochasticMeasure.R
#' 
#' @docType class
#' 
#' @seealso \code{\link{Triangle}}
#' 
#' @name Triangle-class
#' @rdname Triangle-class
#' @exportClass Triangle
#' 
#' @description
#' Triangle is an S4 class used to store aggregated loss data. It is composed of a StochasticMeasure and 
#' a StaticMeasure object. 
#' 
#' @details
#' 
#' \strong{Triangle construction}
NULL

#**********************************************************************************************
# 0. Helper functions ====


#************************************************************************************************************************
# 1. Class Definition ====
#' @export
# is.Triangle
# @description
# Tests whether the object is a triangle
# @return 
# TRUE if the object is a triangle, FALSE if it is not
# @param object The object to be tested
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
         , representation(StaticMeasure="StaticMeasure"
                          , StochasticMeasure="StochasticMeasure"
                          , Name="character"
                          , Data="data.frame")
         , validity = checkTriangle
)

#************************************************************************************************************************
# 2. Construction ====

setGeneric("Triangle", function(StaticMeasure, StochasticMeasure, Name, ...) {
  standardGeneric("Triangle")
})

#' @export
setMethod("Triangle", signature=c(StaticMeasure="StaticMeasure"
                                  , StochasticMeasure="StochasticMeasure"
                                  , Name="character")
          , definition=function(StaticMeasure="StaticMeasure"
                                , StochasticMeasure="StochasticMeasure"
                                , Name="character"){
            
            # TODO: Add checks to ensure that Static and Stochastic Measures are compatible
            
            df = merge(as.data.frame(StochasticMeasure), as.data.frame(StaticMeasure))
            tri = new("Triangle"
                      , StaticMeasure=StaticMeasure
                      , StochasticMeasure=StochasticMeasure
                      , Name=Name
                      , Data=df)
            
})
#************************************************************************************************************************
# 3. Properties ====
#' @export
setMethod("MeasureNames", signature(x="Triangle"), definition=function(x, Stem=TRUE){
  if (Stem){
    strNames = c(MeasureNames(x@StaticMeasure), MeasureNames(x@StochasticMeasure, Stem=TRUE))
  } else {
    strNames = c(MeasureNames(x@StaticMeasure), MeasureNames(x@StochasticMeasure, Stem=FALSE))
  }
  
  strNames
  
})

#' @export
setMethod("LevelNames", signature(x="Triangle"), definition=function(x){
  LevelNames(x@StaticMeasure)
})


#************************************************************************************************************************
# 4. Accessors ====

#************************************************************************************************************************
# 5. Comparison ====

#************************************************************************************************************************
# 6. Conversion ====
#' @export
setMethod("as.data.frame", signature("Triangle"), function(x, ...){
  x@Data
})

#' @export
melt.Triangle = function(data){
  df = data@Data
#  measure = MeasureNames(data, Stem=FALSE)
  mdf = melt(df, id.vars=c("StartDate", "EndDate", "Moniker", "EvaluationDate", "DevIntervals", LevelNames(data))
             #             , measure.vars=c(IncrementalMeasureNames(measure), CumulativeMeasureNames(measure), PriorMeasureNames(measure))
             , variable.name="Measure")
  mdf
}

# PivotTriangleByFormula = function(df, Formula, Measure, Function = sum){
#   z = dcast(df, theFormula, fun=Function, value.var=Measure, fill=NA_real_)
# }
# 
# PivotTriangle = function(Triangle, Row, Column, Measure, Function=sum){
#   df = as.data.frame(Triangle)
#   
#   myFormula = paste0(Row, " ~ ", Column)
#   myFormula = as.formula(myFormula)
#   z = PivotTriangleByFormula(df, myFormula, Measure, Function)
#   z
# }

#************************************************************************************************************************
# 7. Concatenate ====


#************************************************************************************************************************
# 8. Persistence ====


#************************************************************************************************************************
# 9. Display ====

#' @export
setMethod("plot", signature(x="Triangle", y="missing")
          , definition = function(x
                                  , Response
                                  , Predictor
                                  , Group = "Moniker"
                                  , FitLines = FALSE
                                  , IncludeIntercept = FALSE
                                  , FacetFormula
                                  , plotTitle
                                  , xlab
                                  , ylab){
            
            if (missing(Group)) Group = "Moniker"
            
            mdf = melt(x)
            
            #plotTitle = paste0(Response, " by ", Predictor, " grouped by ", Group)
            
            plt = ggplot(mdf, aes_string(x = Predictor, y = Response, group = Group, colour = Group)) 
            plt = plt + geom_point() + xlab(Predictor) + ylab(Response)
            
            if (FitLines){
              if (IncludeIntercept){
                plt = plt + stat_smooth(method = lm, se = FALSE, formula=y~1+x)
              } else {
                plt = plt + stat_smooth(method = lm, se = FALSE, formula=y~0+x)
              }
            } 
            
            plt
})