#' adf
#' 

ProjectValues = function(objTriangleModel, dfProjection)
{
  fit = objTriangleModel@Fit
  dfProjection = predict(fit, newdata = dfProjection)
  
  dfProjection
}


# if (CategoryName == "none"){
#   newX = df[, colnames(df) %in% PredictorName]
# } else {
#   newX = df[, colnames(df) %in% c(PredictorName, CategoryName)]
# }
# print(summary(newX))
# mojo = predict(fit, newX)
# return (mojo)