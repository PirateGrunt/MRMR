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
         , representation(Triangle = "Triangle"
                          , Response = "character"
                          , Predictor = "character"
                          , Group = "character"
                          , Fit = "lm")
         #          , sealed = TRUE
         , validity = #some function
)

newTriangleModel = function(Triangle
                          , Response
                          , Predictor
                          , Group
                          , Intercept = FALSE
                          , Weights = NULL)
{
  df = Triangle@TriangleData
  
  strFormula = paste0(Response, " ~ ", Predictor, ":as.factor(", Group, ")")
  
  if (Intercept){
    strFormula = paste0(strFormula, " + 1")
  } else {
    strFormula = paste0(strFormula, " + 0")
  }
  
  theFormula = as.formula(strFormula)
  
  Fit = lm(theFormula, data = df)
  fit = new("TriangleModel"
            , Response = Response
            , Predictor = Predictor
            , Group = Group
            , Fit = Fit)
  fit
}