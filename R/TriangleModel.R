#' @include Triangle.R
NULL

setClass("TriangleModel", 
         representation(TriangleModelName = "character"
                        , BaseTriangle = "Triangle"
                        , Response = "character"
                        , Predictor = "character"
                        , Category = "character"
                        , MinimumCategoryFrequency = "numeric"
                        , delta = "numeric"
                        , LinearFit = "lm"))

# setValidity("TriangleModel"
#             , function(object){
#               if (is.Triangle(object@BaseTriangle)){
#                 return (TRUE)
#               }else{
#                 return ("Triangle projection must be based on a valid triangle.")
#               }
#             })

#============================
# Constructor
TriangleModel = function(ModelName
                         , BaseTriangle
                         , ResponseName
                         , PredictorName
                         , CategoryName = "none"
                         , MinimumCategoryFrequency = 1
                         , delta = 0)
{  
  # Check that the various columns exist. We'll shorthand the Triangle dataframe.
  df = BaseTriangle@TriangleData
  
  if (!ColumnExists(df, ResponseName))
  {
    stop ("The specified column for the response variable does not exist. Unable to create the triangle model.")
  }
  
  if (!ColumnExists(df, PredictorName))
  {
    stop ("The specified column for the predictor variable does not exist. Unable to create the triangle model.")
  }
  
  if (!ColumnExists(df, CategoryName) && CategoryName != "none")
  {
    stop ("The specified column for the categorical variable does not exist. Unable to create the triangle model.")
  }
  
  # Now calibrate the model
  theFormula = as.formula(paste0(ResponseName, " ~ ", PredictorName, ":", CategoryName, " + 0"))
  if (tolower(CategoryName) == "none"){
    theFormula = as.formula(paste0(ResponseName, " ~ ", PredictorName, " + 0"))
  } 
  
  fit = lm(theFormula, data = df)
  
  triModel = new("TriangleModel"
                 , TriangleModelName = ModelName
                 , BaseTriangle = BaseTriangle
                 , Response = ResponseName 
                 , Predictor = PredictorName
                 , Category = CategoryName
                 , MinimumCategoryFrequency = MinimumCategoryFrequency
                 , delta = delta
                 , LinearFit = fit)
  
  return (triModel)
}

# setGeneric("CalibrateTriangleModel", function(x, ...){
#   standardGeneric("CalibrateTriangleModel")
# })
# 
# setMethod("CalibrateTriangleModel", "Triangle", function(x, Response, Predictor, Category, delta, MinimumCategoryFrequency){
#   
#   df = BaseTriangle@TriangleData
#   
#   
#   return(fit)
# })
