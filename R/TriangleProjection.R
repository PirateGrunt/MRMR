#'
#' TriangleProjection class
#' 
#' @include NewGenerics.R
#' @include OriginPeriod.R
#' @include StaticMeasure.R
#' @include StochasticMeasure.R
#' 
#' @docType class
#' 
#' @name TriangleProjection-class
#' @rdname TriangleProjection-class
#' @exportClass TriangleProjection
#' 
NULL

#**********************************************************************************************
# 0. Helper functions ====

FilterForProjection = function(df, criteria){
  for (i in seq_along(criteria)){
    colname = names(criteria)[[i]]
    testval = criteria[[i]]
    df = df[df[, colname] == testval, ]
  }
  df
}

ProjectDiagonal = function(df, TriangleModel){
  
  fits = TriangleModel@Fit
  filter = TriangleModel@FitFilter
  response = TriangleModel@Response
  lstDF = list()
  j = 1
  for (i in seq_along(fits)){
    dfAtom = FilterForProjection(df, filter[[i]])
    if (nrow(dfAtom) != 0){
      dfAtom[, response] = predict(fits[[i]], newdata=dfAtom)
      lstDF[[j]] = dfAtom
      j = j+1
    }
  }
  
  df = do.call(rbind, lstDF)
  df
}

ProjectTriangle = function(TriangleModel, EvaluationDates, MaxLag){
  
  tri = TriangleModel@Triangle
  scm = tri@StochasticMeasure
  sm = tri@StaticMeasure
  op = sm@OriginPeriod
  
  predictor = TriangleModel@Predictor
  response = TriangleModel@Response
  Level = scm@Level
  DevPeriod = scm@DevPeriod
  
  Lags = unique(scm$Lag)
  Tail = TriangleModel@Tail
  
  priorCols = PriorMeasureNames(MeasureNames(scm))
  cumulativeCols = CumulativeMeasureNames(priorCols)
  incrementalCols = IncrementalMeasureNames(priorCols)
  
  CumulativeResponse = (response %in% cumulativeCols)
  
  df = as.data.frame(tri)
  dfLatest = LatestDiagonal(tri)
  dfLatest = as.data.frame(dfLatest)
  
  for (i in seq_along(EvaluationDates)){
    
    # Create a data frame equal to the most recent evaluation date
    dfNext = dfLatest
    
    # Alter the columns
    dfNext[, priorCols] = dfNext[, cumulativeCols]
    
    dfNext$EvaluationDate = EvaluationDates[i]
    
    dfNext$Lag = dfNext$Lag + 1
    
    dfNext$AdjustedLag = dfNext$Lag
    dfNext$AdjustedLag[dfNext$AdjustedLag > Tail] = Tail
    
    # 2. Predict the observations
    # How do we match the Fit object to the lag?
    dfNext = ProjectDiagonal(dfNext, TriangleModel)
    #dfNext[, response] = 1
    
    # 3. Update the StochasticMeasure
    if (CumulativeResponse){
      dfNext[, incrementalCols] = dfNext[, cumulativeCols] - dfNext[, priorCols]
    } else {
      dfNext[, cumulativeCols] = dfNext[, incrementalCols] + dfNext[, priorCols]
    }
    
    # 4. Bind to the df and increment
    dfNext$AdjustedLag = NULL
    
    df = rbind(df, dfNext)
    
    dfLatest = dfNext
  }
  
  df = subset(df, Lag <= MaxLag)
  
  scm = StochasticMeasure(OriginPeriod = op
                          , Level = Level
                          , Measure = CumulativeMeasureNames(response)
                          , DevPeriod = scm@DevPeriod
                          , EvaluationDates = df$EvaluationDate
                          , Data = df
                          , Lags = df$Lag)
  scm
  
  tri = Triangle(StaticMeasure=sm, StochasticMeasure=scm)
  tri
}

#************************************************************************************************************************
# 1. Class Definition ====
#' @export
is.TriangleProjection = function(object)
{
  is(object, "TriangleProjection")
}

checkTriangleProjection = function(object)
{
  errors = character()
  if (length(errors) == 0) TRUE else errors
}

setClass("TriangleProjection"
         , representation(TriangleModel = "TriangleModel"
                          , Projection = "Triangle")
         , validity = checkTriangleProjection
)

#************************************************************************************************************************
# 2. Construction ====

setGeneric("TriangleProjection", function(TriangleModel, ...) {
  standardGeneric("TriangleProjection")
})

#' @export
setMethod("TriangleProjection"
          , signature=c(TriangleModel="TriangleModel")
          , definition=function(TriangleModel, AsOfDate, MaxLag) {
            
            # 1. Forecast EvaluationDates
            scm = TriangleModel@Triangle@StochasticMeasure
            EvaluationDates = DateSequence(max(scm$EvaluationDate), AsOfDate, scm@DevPeriod)
            EvaluationDates = EvaluationDates[-1]
            
            # 2. Construct iterative forecast
            triProjection = ProjectTriangle(TriangleModel, EvaluationDates, MaxLag)
            
            proj = new("TriangleProjection"
                       , TriangleModel = TriangleModel
                       , Projection = triProjection)
            
            proj
            
})

#************************************************************************************************************************
# 3. Properties ====

#************************************************************************************************************************
# 4. Accessors ====

#************************************************************************************************************************
# 5. Comparison ====

#************************************************************************************************************************
# 6. Conversion ====

#************************************************************************************************************************
# 7. Concatenate ====

#************************************************************************************************************************
# 8. Persistence ====
#' @export
setMethod("write.excel", signature=c(object = "TriangleProjection", file="character", overwrite="logical")
          , definition=function(object, file, overwrite=FALSE, TimeAxis="Lag", Wide=TRUE){
            
            if (file.exists(file) & !overwrite){
              stop("Excel file already exists. Either enter a new filename or set the overwrite parameter to TRUE.")
            }
            
            write.excel(object@Projection, file, overwrite, TimeAxis, Wide)
            
})

#************************************************************************************************************************
# 9. Display ====
