is.TriangleModel = function(object)
{
  is(object, "TriangleModel")
}

checkTriangleModel = function(object)
{
  errors = character()

  if (length(errors) == 0) TRUE else errors
}

#' TriangleModel class
#' 
#' Triangle is an S4 class used to store a model fit to a Triangle object.
#' 
#' @name TriangleModel-class
#' @rdname TriangleModel-class
#' @exportClass TriangleModel
#' 
setClass("TriangleModel"
         , representation(ModelData = "data.frame"
                          , Response = "character"
                          , Predictor = "character"
                          , FitCategory = "character"
                          , Alpha = "numeric"
                          , Tail = "numeric"
                          , Fit = "lm"
                          , Formula = "formula"
                          , TailFunction = "function")
         #          , sealed = TRUE
         # , validity = #some function
)

TailFunction = function(x, Tail)
{
  y = ifelse(x > Tail, "Tail", x)
  if (!"Tail" %in% y){
    y[y == max(y)] = "Tail"
  }
  y
}

#' Create a new TriangleModel object
newTriangleModel = function(Triangle
                            , Response
                            , Predictor
                            , FitCategory
                            , Intercept = FALSE
                            , Alpha = NULL
                            , Tail = NULL)
{
  dfTriangleData = Triangle@TriangleData
  df = dfTriangleData[,c("OriginPeriod", "DevelopmentLag", "EvaluationDate", "DevInteger"
                         , "OriginPeriodStart", "OriginPeriodEnd", "CalendarPeriodStart"
                         , "CalendarPeriodEnd", "CalendarPeriod")]
  df = cbind(df, dfTriangleData[Response])
  df = cbind(df, dfTriangleData[Predictor])
  
  if (length(FitCategory) > 1){
    # do something
    error("Not yet configured for multiple groups.")
  }
  
  df$FitCategory = df[,FitCategory]
  
  if (is.null(Tail)) Tail = max(df$DevInteger)
  
  if (FitCategory == "DevInteger"){
    df$FitCategory = TailFunction(df$FitCategory, Tail)
  }
  
  df$FitCategory = as.factor(df$FitCategory)
  
  strFormula = paste0(Response, " ~ ", Predictor, ":FitCategory")
  
  if (Intercept){
    strFormula = paste0(strFormula, " + 1")
  } else {
    strFormula = paste0(strFormula, " + 0")
  }
  
  theFormula = as.formula(strFormula)
  
  Fit = lm(theFormula, data = df)
  
  TriangleModel = new("TriangleModel"
                      , ModelData = df
                      , Response = Response
                      , Predictor = Predictor
                      , FitCategory = FitCategory
                      , Alpha = 0
                      , Tail = Tail
                      , Fit = Fit
                      , Formula = theFormula)
  
  TriangleModel
}

# as.TriangleModel <- function(x, unit, ...) standardGeneric("as.TriangleModel")
# 
# setGeneric("as.TriangleModel")
# 
# setMethod("as.TriangleModel", signature("TriangleModel"), function(x, ...) x)
# 
# #' @export
# setMethod("c", signature(x = "TriangleModel"), function(x, ...){
#   elements <- lapply(list(...), as.TriangleModel)
#   
#   Triangles = c(x@Triangle, unlist(lapply(elements, slot, "Triangle")))
#   Responses <- c(x@Response, unlist(lapply(elements, slot, "Response")))
#   Predictors <- c(x@Predictor, unlist(lapply(elements, slot, "Predictor")))
#   Groups <- c(x@Group, unlist(lapply(elements, slot, "Group"))) 
#   Fits <- c(x@Fit, unlist(lapply(elements, slot, "Fit")))
#   
#   new("TriangleModel"
#       , Triangle = Triangles
#       , Response = Responses
#       , Predictor = Predictors
#       , Group = Groups
#       , Fit = Fits)
# })

# #' @export
# setMethod("rep", signature(x = "Period"), function(x, ...){
#   new("Period", rep(x@.Data, ...), year = rep(x@year, ...), 
#       month = rep(x@month, ...), day = rep(x@day, ...), 
#       hour = rep(x@hour, ...), minute = rep(x@minute, ...))
# })
# 
# 
# #' @export
# setMethod("[", signature(x = "TriangleModel"), 
#           function(x, i, j, ..., drop = TRUE) {
#             newTriangleModel(x@.Data[i], year = x@year[i], month = x@month[i], 
#                 day = x@day[i], hour = x@hour[i], minute = x@minute[i])
#           })
# 
# #' @export
# setMethod("[[", signature(x = "TriangleModel"), 
#           function(x, i, j, ..., exact = TRUE) {
#             new("Period", x@.Data[i], year = x@year[i], month = x@month[i], 
#                 day = x@day[i], hour = x@hour[i], minute = x@minute[i])
#           })
# 
# #' @export
# setMethod("[<-", signature(x = "TriangleModel", value = "TriangleModel"), 
#           function(x, i, j, ..., value) {
#             x@.Data[i] <- value@.Data
#             x@year[i] <- value@year
#             x@month[i] <- value@month
#             x@day[i] <- value@day 
#             x@hour[i] <- value@hour
#             x@minute[i] <- value@minute
#             x
#           })
# 
# #' @export
# setMethod("[[<-", signature(x = "TriangleModel", value = "TriangleModel"), 
#           function(x, i, j, ..., value) {
#             x@.Data[i] <- value@.Data
#             x@year[i] <- value@year
#             x@month[i] <- value@month
#             x@day[i] <- value@day 
#             x@hour[i] <- value@hour
#             x@minute[i] <- value@minute
#             x
#           })
# 
# #' @export
# setMethod("$", signature(x = "TriangleModel"), function(x, name) {
#   slot(x, name)
# })
# 
# #' @export
# setMethod("$<-", signature(x = "TriangleModel"), function(x, name, value) {
#   slot(x, name) <- value
#   x
# })
