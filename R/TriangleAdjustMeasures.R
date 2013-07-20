#' Create incrementals
#' 
#' @export CreateIncrementals
#' @param OriginPeriod Vector of intervals
#' @param Group A vector of groups
#' @param Variable A list of 
#' @param StartDay If OriginStart and OriginEnd are supplied, this argument is ignored.
#' @param StartMonth If OriginStart and OriginEnd are supplied, this argument is ignored.
#' @return A vector of intervals
#' @seealso \code{\link{CreateCumulative}}, \code{\link{CreatePriors}}
#' @examples
#' 
CreateIncrementals = function(dfTriangleData, measureCols, Groups)
{
  lOriginYear = dlply(dfTriangleData, c(Groups, "OriginPeriodStart"))
  
  lOriginYear = lapply(lOriginYear, function(x) {
    x = x[order(x$OriginPeriodStart, x$EvaluationDate),]
    theMeasures = x[, measureCols]
    incrementals = apply(theMeasures, 2, diff)
    incrementals = rbind(theMeasures[1,], incrementals)
  })
  dfMeasures = do.call("rbind", lOriginYear)
  row.names(dfMeasures) = NULL
  colnames(dfMeasures) = gsub("Cumulative", "Incremental", measureCols)
  
  dfTriangleData = cbind(dfTriangleData, dfMeasures)
  
}

#' Create cumulative
#' 
#' @export CreateCumulative
#' @param OriginPeriod Vector of intervals
#' @param Group A vector of groups
#' @param Variable A list of 
#' @return Adjusted triangle
#' @seealso \code{\link{CreateIncremental}}, \code{\link{CreatePrior}}
#' @examples
#' 
CreateCumulative = function(dfTriangleData, measureCols, Groups)
{
  
  lOriginYear = dlply(dfTriangleData, c(Groups, "OriginPeriodStart"))
  
  lOriginYear = lapply(lOriginYear, function(x) {
    x = x[order(x$OriginPeriodStart, x$EvaluationDate),]
    theMeasures = x[, measureCols]
    cumulatives = apply(theMeasures, 2, cumsum)
  })
  dfMeasures = do.call("rbind", lOriginYear)
  row.names(dfMeasures) = NULL
  colnames(dfMeasures) = gsub("Incremental", "Cumulative", measureCols)
  
  dfTriangleData = cbind(dfTriangleData, dfMeasures)
  
}

#' Create priors
#' 
#' @export CreateCumulative
#' @param OriginPeriod Vector of intervals
#' @param Group A vector of groups
#' @param Variable A list of 
#' @return Adjusted triangle
#' @seealso \code{\link{CreateIncremental}}, \code{\link{CreateCumulative}}
#' @examples
#' 
CreatePriors = function(dfTriangleData, measureCols, Groups)
{
#   dfTriangleData = mojo
#   measureCols = names(mojo)
#   measureCols = measureCols[- (1:10)]
  
  cumulCols = grep("*Cumulative*", measureCols)
  cumulCols = measureCols[cumulCols]
  incrCols = grep("*Incremental*", measureCols)
  incrCols = measureCols[incrCols]
  
  lOriginYear = dlply(dfTriangleData, c(Groups, "OriginPeriodStart"))
  
  lOriginYear = lapply(lOriginYear, function(x) {
    priors = x[, cumulCols] - x[, incrCols]
    if (nrow(x) > 1 )
    {
      priors = priors[-1,]
      priors = rbind(rep(NA, ncol(priors)), priors)
    } else {
      priors[1,] = rep(NA, ncol(priors))
      priors
    }
  })
  dfMeasures = do.call("rbind", lOriginYear)
  row.names(dfMeasures) = NULL
  colnames(dfMeasures) = gsub("Incremental", "Prior", incrCols)
  
  dfTriangleData = cbind(dfTriangleData, dfMeasures)
  
}

#' Form measures
#' 
#' @export FormMeasures
#' @param 
#' 
FormMeasureNames = function(Measures, Cumulative = TRUE)
{
  if (Cumulative){
    missingCumul = grep("*cumulative*", tolower(names(Measures)), invert = TRUE)
    names(Measures)[missingCumul] = paste0("Cumulative", names(Measures[missingCumul]))
  } else {
    missingIncr = grep("*incremental*", tolower(names(Measures)), invert = TRUE)
    names(Measures)[missingIncr] = paste0("Incremental", names(Measures[missingIncr]))
  }
 
  names(Measures)
  
}

CleanMeasureNames = function(MeasureNames){
  MeasureNames = gsub("*cumulative*", "", MeasureNames, ignore.case = TRUE)
  MeasureNames = gsub("*incremental*", "", MeasureNames, ignore.case = TRUE)
  MeasureNames = unique(MeasureNames)
}