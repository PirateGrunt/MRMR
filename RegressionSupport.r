#==================================================================================================
# RegressionSupport.r
# This code will create a design matrix and weights to be passed to  the 
# lm function. This will also support projection of results based on sample
# data and calibrated parameters. This will show basic diagnostics of the 
# sample data and show diagnostics of the fit
#
# Contains the following functions:
#   CreateDesignMatrixByCategory (Predictor, PaymentLag, DesignWidth)
#   CreateRegressionWeights (DesignMatrix, delta)
#   FitModel(Response, Predictor, Category, DesignWidth, delta)
#   GetDiagonal(SomeVector, CalendarPeriod, WhichCalendarPeriod)
#   GetFactors(Fit)
#   ProjectValues (Predictor, Factors, Lags, OriginYear, MaximumDev, Autoregressive)
#   GetUltimatesbyAY (dfHistory, dfProjection)
#
# TODO: Add stochastics to the projection
# TODO: Format x axes
# TODO: Increase amount of regression diagnostic output
#==================================================================================================

#==================================================================================================
# function CreateDesignMatrixByCategory
# Returns a matrix of predictors, categorized
#==================================================================================================
CreateDesignMatrixByCategory = function(Predictor, Category, MinFrequency)
{
    # This function will create a design matrix by category
    # At present, this will only work with payment lags.
    # TODO: Determine how to query the categories and allocate on that basis
    # TODO: Must be able to establish an "other" category. For payment lags, this
    # is the tail. For other types of category, we may differ.
    # TODO: Develop a way to include an intercept term
  
    z = as.data.frame(table(factor(Category)))
    nCols = length(which(z$Freq >= MinFrequency)) + 1
    # TODO: put in a check for nCols = 0
    
    nRows = length(Predictor)
    DesignMatrix <- as.data.frame(matrix(0, nRows, nCols))
    for (i in 1:(nCols-1))
    {
        idx = array(FALSE, nRows)
        idx[which(Category==z[i,1])] = TRUE
        DesignMatrix[idx, i] = Predictor[idx]
    }
    
    idx = array(FALSE,nRows)
    tail = z[which(z$Freq < MinFrequency), 1]
    idx[which(Category %in% tail)] = TRUE
    DesignMatrix[idx, nCols] = Predictor[idx]
    
    dmNames = as.character(z[which(z$Freq >= MinFrequency), 1])
    dmNames = c(dmNames, "tail")
    colnames(DesignMatrix) = dmNames
    return (DesignMatrix)
}
#== end CreateDesignMatrixByCategory ==============================================================

#==================================================================================================
# function CreateRegressionWeights
#==================================================================================================
CreateRegressionWeights = function(DesignMatrix, delta)
{
    RegressionWeights = rowSums(DesignMatrix)
    RegressionWeights = pmax(RegressionWeights,1)

    RegressionWeights=1/(RegressionWeights^delta)
    return (RegressionWeights)
}
#= end CreateRegressionWeights ====================================================================

#==================================================================================================
# function CreateDevelopmentArray
#==================================================================================================
CreateDevelopmentArray = function(DesignMatrix, Fit, IntervalLength, MaxInterval)
{
  df = as.data.frame(names(DesignMatrix))
  factors = as.numeric(coef(Fit))
  df = cbind(df, factors)
  colnames(df)[1] = "FactorName"
  colnames(df)[2] = "Factor"
  
  dfDevFactor = as.data.frame(IntervalLength * 1:MaxInterval)
  colnames(dfDevFactor)[1] = "Devs"
  
  dfMojo = match(as.vector(dfDevFactor$Devs), df$FactorName, nomatch = match("tail", df$FactorName))
  dfDevFactor = cbind(dfDevFactor, df[dfMojo, "FactorName"])
  colnames(dfDevFactor)[2] = "Categories"
  
  dfMojo = match(as.vector(dfDevFactor$Categories), df$FactorName, nomatch = match("tail", df$FactorName))
  dfDevFactor = cbind(dfDevFactor, df[dfMojo, "Factor"])
  colnames(dfDevFactor)[3] = "Factors"
  
  return (dfDevFactor)
}
#= end CreateDevelopmentArray =====================================================================

#==================================================================================================
# function FitModel
#==================================================================================================
FitModel = function(Response, Predictor, Category, MinFrequency, delta, IntervalWidth, MaxInterval)
{
  DesignMatrix <- CreateDesignMatrixByCategory(Predictor, Category, MinFrequency)
  RegressionWeights <- CreateRegressionWeights(DesignMatrix, delta)
  dm = as.matrix(DesignMatrix)
  Fit = lm(Response ~ dm + 0, weights=RegressionWeights)
  
  df = CreateDevelopmentArray(DesignMatrix, Fit, IntervalWidth, MaxInterval)
  
  z = list(DevelopmentArray = df, Fit = Fit)
  return (z)
}
#= end FitModel ===================================================================================

#==================================================================================================
# function GetDiagonal
#==================================================================================================
GetDiagonal = function(SomeVector, CalendarPeriod, WhichCalendarPeriod)
{
  ReturnValue = SomeVector[which(CalendarPeriod==WhichCalendarPeriod)]
  ReturnValue[which(is.na(ReturnValue))] = 0
  
  return (ReturnValue)
}
#= end GetDiagonal ================================================================================

#==================================================================================================
# function GetFactors
#==================================================================================================
GetFactors = function(ModelFit)
{
  Factors = as.numeric(coef(Fit))
  
  return (Factors)
}
#= end GetFactors =================================================================================

#==================================================================================================
# function ProjectValues
# This will project values iteratively.
# Each iteration represents a calendar period.
# Each iteration stores a set of origin periods, lags and calendar periods in a dataframe
# Some methods are recursive (i.e. multiplicative chain ladder) and will use a response as the predictor for the next iteration.
# OriginPeriod may be a vector of indices which map to calendar periods.
#==================================================================================================
ProjectValues = function(Predictor, Category, OriginPeriod, DevelopmentArray, Recursive)
{
  # TODO: We must be able to extrapolate through the bottom and also interpolate between factors.
  
  # Establish a data frame with a dummy row. This means that we don't have to keep checking
  # to see if we set up the data fram or merely bind a new row.
  dfReturnValue = data.frame(OriginPeriod = OriginPeriod[1], Category=Category[1], Incremental=0.0, row.names=NULL)
  nRows = length(Predictor)
  maxDevs = length(DevelopmentArray$Devs)
  for (iRow in 1:nRows)
  {
    thePredictor = Predictor[iRow]
    theOriginPeriod = OriginPeriod[iRow]
    
    firstDev = match(Category[iRow], DevelopmentArray$Categories) + 1
    if (is.na(firstDev) == TRUE) { firstDev = match("tail", DevelopmentArray$Categories)}
    for (iDev in firstDev:maxDevs)
    {
      theCategory = DevelopmentArray$Devs[iDev]
      theProjection =  thePredictor * DevelopmentArray$Factors[iDev]
      dfReturnValue = rbind(dfReturnValue, data.frame(OriginPeriod = theOriginPeriod
                                                      , Category = theCategory
                                                      , Incremental = theProjection
                                                      , row.names=NULL))
      if (Recursive == TRUE)
      {thePredictor = thePredictor + theProjection}
    }
  }

  dfReturnValue = dfReturnValue[-1,]
  row.names(dfReturnValue) = seq(nrow(dfReturnValue))
  
  dfReturnValue$OriginYear = as.POSIXlt(dfReturnValue$OriginPeriod)$year + 1900
  return(dfReturnValue)
}
#= end ProjectValues ==============================================================================

#==================================================================================================
# function GetUltimatesbyAY
#==================================================================================================
GetUltimatesbyAY = function(dfHistory, dfProjection)
{
  dfComplete = data.frame(OriginYear = dfHistory$AY, Age=dfHistory$Lag, Incremental=dfHistory$IncrementalPaid, row.names=NULL)
  dfComplete = rbind(dfComplete, dfProjection)
  OriginYearFactors = factor(dfComplete$OriginYear)
  Ultimates = tapply(dfComplete$Incremental, OriginYearFactors, sum)
  
  return (Ultimates)
}
#= end GetUltimatesbyAY============================================================================

#==================================================================================================
# function ResidualDiagnostics
# This function will take the results of a fit which used lm and calculate several standard 
# diagnostic statistics. They are:
#     AD: The Anderson-Darling test for normality
#     KS: The Kolmogorov-Smirnov test for normality
#     DW: The Durbin-Watson test for correlation
#     BG: The Breusch-Godfrey test for correlation
# In each case, we store the value of the statistic and the associated p-value. We return this as 
# a dataframe.
#==================================================================================================
ResidualDiagnostics = function(Fit)
{
  residuals = residuals(Fit)
  AD = ad.test(residuals)
  KS = lillie.test(residuals)
  DW = dwtest(Fit)
  BG = bgtest(Fit)
  BP = bptest(Fit)
  
  df = data.frame(row.names=NULL
                  , ADStat = AD$statistic, ADp = AD$p.value
                  , KSStat= KS$statistic, KSp = KS$p.value
                  , DWStat = DW$statistic, DWp = DW$p.value
                  , BGStat = BG$statistic, BGp = BG$p.value
                  , BPStat = BP$statistic, BPp = BP$p.value)
  return (df)
}
#= end ResidualDiagnostics ========================================================================

#==================================================================================================
# function CalculateIncrementals
# This function will calculate incremental developments
#==================================================================================================
CalculateIncrementals = function(df)
{
  df = df[order(df$LossPeriod, df$DevelopmentLag),]
  
  rownames(df) = NULL
  
  df$IncrementalPaid = df$CumulativePaid
  df$IncrementalIncurred = df$CumulativeIncurred
  df$PriorCumulativePaid = df$CumulativePaid
  df$PriorCumulativeIncurred = df$PriorCumulativeIncurred
  
  nrows = length(df$CumulativePaid)
  
  df$PriorCumulativePaid[2:nrows] = df$CumulativePaid[1:nrows-1]
  df$PriorCumulativeIncurred[2:nrows] = df$CumulativeIncurred[1:nrows-1]
  df$IncrementalPaid[2:nrows] = df$CumulativePaid[2:nrows] - df$CumulativePaid[1:nrows-1]
  df$IncrementalIncurred[2:nrows] = df$CumulativeIncurred[2:nrows] - df$CumulativeIncurred[1:nrows-1]
  
  firstLag = which(df$DevelopmentLag == 1)
  df$IncrementalPaid[firstLag] = df$CumulativePaid[firstLag]
  df$IncrementalIncurred[firstLag] = df$CumulativeIncurred[firstLag]
  df$PriorCumulativePaid[firstLag] = 0
  df$PriorCumulativeIncurred[firstLag] = 0
  
  return(df)
}
#= end CalculateIncrementals ========================================================================
