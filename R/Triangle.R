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
                          , Name="character")
#                          , Data="data.frame")
         , validity = checkTriangle
)

#************************************************************************************************************************
# 2. Construction ====

setGeneric("Triangle", function(StaticMeasure, StochasticMeasure, ...) {
  standardGeneric("Triangle")
})

#' @export
setMethod("Triangle", signature=c(StaticMeasure="StaticMeasure"
                                  , StochasticMeasure="StochasticMeasure")
          , definition=function(StaticMeasure
                                , StochasticMeasure
                                , Name){
            
            if (missing(Name)) Name = "Please give me a name"
            
            # TODO: Add checks to ensure that Static and Stochastic Measures are compatible
            
#            if (nrow(df) == 0) warning("No data was matched between the Static and Stochastic measures.")
            
            tri = new("Triangle"
                      , StaticMeasure=StaticMeasure
                      , StochasticMeasure=StochasticMeasure
                      , Name=Name)
            
})
#************************************************************************************************************************
# 3. Properties ====

#' @export
StochasticMeasureNames = function(x, Stem=TRUE){
  MeasureNames(x@StochasticMeasure, Stem=Stem)
}

#' @export
StaticMeasureNames = function(x){
  MeasureNames(x@StaticMeasure)
}

#' @export
setMethod("MeasureNames", signature(x="Triangle"), definition=function(x, Stem=TRUE){
  strNames = c(MeasureNames(x@StaticMeasure), MeasureNames(x@StochasticMeasure, Stem=Stem))
  
  strNames
  
})

#' @export
setMethod("LevelNames", signature(x="Triangle"), definition=function(x){
  LevelNames(x@StaticMeasure)
})


#************************************************************************************************************************
# 4. Accessors ====
#' @export
setMethod("$", signature(x = "Triangle"), function(x, name) {
  if (length(name) > 1) stop("Cannot access more than one value at a time. Use the `[` operator for multiple access.")
  
  scm = x@StochasticMeasure
  
  # 1. Is it a slot?
  if (name %in% slotNames(x)){
    return (slot(x, name))
  }
  
  # 2. Is it a LevelName?
  if (name %in% LevelNames(x)) {
    return (scm@Data[, name])
  } 
  
  # 3. Is it a StochasticMeasure column?
  dfSCM = scm@Data
  if (name %in% colnames(dfSCM)){
    return (dfSCM[, name])
  }
  
})

#' @export
setMethod("[", signature(x="Triangle"), definition=function(x, i, j, drop=TRUE){
  
  scm = x@StochasticMeasure
  scm = scm[i, ]
  
  sm = x@StaticMeasure
  scmCore = scm@Data
  scmCore = scmCore[, c("StartDate", LevelNames(scm))]
  scmCore = unique(scmCore)
  dfSM = merge(sm@Data, scmCore)
  sm = StaticMeasure(scm$OriginPeriod, MeasureNames(sm), LevelNames(sm), dfSM)

  tri = Triangle(sm, scm, x@Name)
})

#' @export 
setMethod("UpperTriangle", signature(object="Triangle"), definition=function(object){
  # The upper triangle is all observations on or before the first evaluation for the most recent element of the OriginPeriod 
  #scm = UpperTriangle(x@StochasticMeasure)
  evalDate = object$EvaluationDate[object$StartDate == max(object$StartDate)]
  tri = object[object$EvaluationDate <= min(evalDate)]
  tri
})

#' @export
setMethod("LatestDiagonal", signature(object="Triangle"), definition=function(object){
  tri = object[object$EvaluationDate == max(object$EvaluationDate)]
  tri
})

setMethod("Diagonal", signature(object="Triangle", EvaluationDate="ANY")
          , definition=function(object, EvaluationDate){
            tri = object[object$EvaluationDate == EvaluationDate]
            tri
})
#************************************************************************************************************************
# 5. Comparison ====

#************************************************************************************************************************
# 6. Conversion ====
#' @export
setMethod("as.data.frame", signature("Triangle"), function(x, ...){
  df = merge(as.data.frame(x@StochasticMeasure), as.data.frame(x@StaticMeasure), sort=FALSE)
  df
})

#' @export
melt.Triangle = function(data){
#  df = data@Data
  df = as.data.frame(data)
  mdf = melt(df, id.vars=c("StartDate", "EndDate", "Moniker", "EvaluationDate", "Lag", LevelNames(data))
             , variable.name="Measure")
  mdf
}

#' @export 
setMethod("LongToWide", signature("Triangle"), function(object, TimeAxis="Lag"){
  mdf = melt(object)
  theFormula = paste(LevelNames(object), collapse="+")
  theFormula = paste(theFormula, "Measure", "StartDate", "Moniker", sep="+")
  theFormula = paste(theFormula, "~", TimeAxis)
  df = dcast(mdf, as.formula(theFormula), sum)
  df
})
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
                                  , Group = "Lag"
                                  , FitLines = FALSE
                                  , IncludeIntercept = FALSE
                                  , FacetFormula){
            df = as.data.frame(x)
            df = df[!is.na(df[Predictor]), ]
            
            if (missing(Group)){
              Group = "Lag"
              df$Lag = as.factor(df$Lag)
            } 
            
            plt = ggplot(df, aes_string(x = Predictor, y = Response, group = Group, colour = Group)) 
            plt = plt + geom_point()
            
            if (FitLines){
              if (IncludeIntercept){
                plt = plt + stat_smooth(method = lm, se = FALSE, formula=y~1+x)
              } else {
                plt = plt + stat_smooth(method = lm, se = FALSE, formula=y~0+x)
              }
            }
            
            if (missing(FacetFormula)){
              facetCols = LevelNames(x)
              
              FacetFormula = paste(facetCols, collapse="+")
              FacetFormula = as.formula(paste("~", FacetFormula))
              
              facetRow = length(unique(df[, facetCols]))
              facetRow = floor(sqrt(facetRow)) + 1
              
              plt = plt + facet_wrap(FacetFormula, facetRow, scales="free")  
            } else {
              if (FacetFormula != "none") plt = plt + facet_grid(FacetFormula, scales="free")
            }
            
            plt
})