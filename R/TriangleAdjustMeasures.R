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
#' 
CreateIncrementals = function(dfTriangleData, measureCols, Groups)
{
  require(plyr)
  lOriginYear = dlply(dfTriangleData, c(Groups, "OriginPeriodStart"))
  
  lOriginYear = lapply(lOriginYear, function(x) {
    x = x[order(x$OriginPeriodStart, x$EvaluationDate),]
    theMeasures = x[, measureCols]
    incrementals = apply(theMeasures, 2, diff)
    incrementals = rbind(theMeasures[1,], incrementals)
    colnames(incrementals) = gsub("Cumulative", "Incremental", measureCols)
    x = cbind(x, incrementals)
  })
  dfMeasures = do.call("rbind", lOriginYear)
  row.names(dfMeasures) = NULL
  
  dfMeasures
  
}

#' Create cumulative
#' 
#' @export CreateCumulative
#' @param OriginPeriod Vector of intervals
#' @param Group A vector of groups
#' @param Variable A list of 
#' @return Adjusted triangle
#' @seealso \code{\link{CreateIncrementals}}, \code{\link{CreatePriors}}
#' 
CreateCumulative = function(dfTriangleData, measureCols, Groups)
{
  require(plyr)
  
  cumulColNames = gsub("Incremental", "Cumulative", measureCols)
  
  lOriginYear = dlply(dfTriangleData, c(Groups, "OriginPeriodStart"))
  
  lOriginYear = lapply(lOriginYear, function(x) {
    x = x[order(x$OriginPeriodStart, x$EvaluationDate),]
    theMeasures = x[measureCols]
    if (nrow(theMeasures) == 1){
      cumulatives = theMeasures
    } else {
      cumulatives = as.data.frame(apply(theMeasures, 2, cumsum))  
    }
    names(cumulatives) = cumulColNames
    x = cbind(x, cumulatives)
  })
  dfMeasures = do.call("rbind", lOriginYear)
  row.names(dfMeasures) = NULL
  
  dfMeasures
  
}

#' Create priors
#' 
#' @export CreatePriors
#' @param OriginPeriod Vector of intervals
#' @param Group A vector of groups
#' @param Variable A list of 
#' @return Adjusted triangle
#' @seealso \code{\link{CreateIncremental}}, \code{\link{CreateCumulative}}
#' 
CreatePriors = function(dfTriangleData, measureCols, Groups)
{
  require(plyr)
  cumulCols = grep("*Cumulative*", measureCols)
  cumulCols = measureCols[cumulCols]
  incrCols = grep("*Incremental*", measureCols)
  incrCols = measureCols[incrCols]
  
  numMeasures = length(incrCols)
  
  lOriginYear = dlply(dfTriangleData, c(Groups, "OriginPeriodStart"))
  
  lOriginYear = lapply(lOriginYear, function(x) {
    if (nrow(x) == 1 )
    {
      priors = as.data.frame(x[cumulCols])
    } else {
      priors = x[cumulCols] - x[incrCols]
    }
    priors[1, ] = rep(NA, numMeasures)
    priors
  })
  
  dfMeasures = do.call("rbind", lOriginYear)
  row.names(dfMeasures) = NULL
  colnames(dfMeasures) = gsub("Incremental", "Prior", incrCols)
  
  dfTriangleData = cbind(dfTriangleData, dfMeasures)
  
}

#' Form measures
#' 
#' @export FormMeasureNames
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

#' GetStochasticColumnNames
#' 
#' @export GetStochasticColumnNames
#' 
GetStochasticColumnNames = function(MeasureNames){
  baseNames = CleanMeasureNames(MeasureNames)
  incrNames = paste0("Incremental", baseNames)
  cumulNames = paste0("Cumulative", baseNames)
  priorNames = paste0("Prior", baseNames)
  
  theNames = c(priorNames, incrNames, cumulNames)
  theNames
}

GetPriorNames = function(MeasureNames){
  baseNames = CleanMeasureNames(MeasureNames)
  priorNames = paste0("Prior", baseNames)
  
  priorNames 
}

GetCumulativeNames = function(MeasureNames){
  baseNames = CleanMeasureNames(MeasureNames)
  cumulNames = paste0("Cumulative", baseNames)
  
  cumulNames 
}