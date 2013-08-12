#' adf
#' 

ProjectStaticValues = function(objTriangleModel, dfProjection)
{
  fit = objTriangleModel@Fit
  dfProjection = predict(fit, newdata = dfProjection)
  
  dfProjection
}

ProjectStochasticValues = function(objTriangleModel, dfProjection)
{
  fit = objTriangleModel@Fit
  
#   baseName = CleanMeasureNames(objTriangleModel@Response)
#   cumulName = paste0("Cumulative", baseName)
#   incrName = paste0("Incremental", baseName)
#   priorName = paste0("Prior", baseName)
#   
#   evalDates = unique(dfProjection$EvaluationDate)
#   evalDates = sort(evalDates)
#   
#   for (i in seq_along(evalDates)) {
#     whichRows = (dfProjection$EvaluationDate == evalDates[i])
#     dfProjection[whichRows,] = predict(fit, newdata = dfProjection[whichRows,])
#     dfProjection[whichRows, cumulName] = dfProjection[whichRows, incrName] + dfProjection[whichRows, priorName]
#   }
#   
  dfProjection = predict(fit, newdata = dfProjection)
  
  dfProjection
}

mojo = unique(Friedland@TriangleData$EvaluationDate)
mojo = sort(mojo)

for (i in seq_along(mojo)) print(mojo[i])
# if (CategoryName == "none"){
#   newX = df[, colnames(df) %in% PredictorName]
# } else {
#   newX = df[, colnames(df) %in% c(PredictorName, CategoryName)]
# }
# print(summary(newX))
# mojo = predict(fit, newX)
# return (mojo)

# slotNames(PaidAM1)
# mojo = Friedland@TriangleData
# whichMeasure = CleanMeasureNames(PaidAM1@Response)
# whichMeasure = GetStochasticColumnNames(whichMeasure)
# mojo = mojo[, whichMeasure]
# 
# is.StochasticMeasure(Friedland, "EP")