#' StochasticMeasure class
#'
#' @include NewGenerics.R
#' @include OriginPeriod.R
#' 
#' 
#' @docType class
#' 
#' @seealso \code{\link{StochasticMeasureConstructor}}
#' 
#' @name StochasticMeasure-class
#' @rdname StochasticMeasure-class
#' @exportClass StochasticMeasure
#' 
#' @description
#' A StochasticMeasure holds multilevel/multivariate data associated with an OriginPeriod.
#' 
#' @details
#' 
#' A StochasticMeasure resembles longitudinal data. 
#' 
#' A StochasticMeasure is so called to distinguish it from a StaticMeasure. A StochasticMeasure has a correspondence
#' to a single element of an OriginPeriod, but its value changes from one observation to the next. This would be the case for
#' number of claims, paid losses and the like. This would also be true for loss-sensitive premium such as exists in a retrospectively
#' rated policy.
#' 
#' 
#' \strong{Levels}
#' Each StochasticMeasure may be assigned level parameters which enable multiple sets of measures to be stored in the 
#' same object. This will, in turn, facilitate construction of a hierarchical analysis model. The levels may be
#' of any arbitraty complexity, irrespective of the number of elements of the StochasticMeasure object. For example, one
#' may have a single StochasticMeasure which uses more than one level element: GL, prem/ops, California, for example. 
#' The object is constrained in that there must be enough levels to describe each element of the object. The constructor examples
#' should make this clear.
#' 
#' If the user does not specify level attributes, the default value of "All" is used.
#' 
#' \strong{StochasticMeasure construction}
#' To construct
#' 
#' \strong{Member access}
#' Elements of a StochasticMeasure object are accessed in a manner similar to those for data frames. However, a StochasticMeasure has two, not
#' three dimensions. One way of thinking of this is that a StochasticMeasure is a list of data frames. The indexing convention is as follows:
#' Because the "data frame" may be thought of as having rows specific by OriginPeriod and columns specified by the names of measured 
#' values, the i and i indices are used to specify these two dimensions. The third dimension is used to identify the Level. For a 
#' StochasticMeasure object, a fourth dimension is used to identify the evalution date, or development lag.
NULL

#************************************************************************************************************************
# 0. Helpers ====
StochasticKernelDataFrame = function(op, levels, devIntervals){
  op = as.data.frame(op)
  op = op[, c("StartDate", "EndDate", "Moniker")]
  
  levels$DevIntervals = devIntervals
  df = merge(op, expand.grid(levels, stringsAsFactors=FALSE))
  df = OrderStochasticMeasureData(df, "Moniker", "DevIntervals", levels)
  df
}

OrderStochasticMeasureData = function(df, opSort, devSort, levels){
  df = df[ do.call(order, df[, c(names(levels), devSort, opSort)]), ]
  row.names(df) = NULL
  df
}

GetEvaluationDates = function(df, period){
  EvaluationDate = df$StartDate + df$DevIntervals * period - days(1)
  EvaluationDate
}

GetDevIntervals = function(df, EvalDates, period){
  devIntervals = (EvalDates + days(1) - df$StartDate) / period
  devIntervals
}

CumulativeMeasureNames = function(Measure){
  Measure = CleanStochasticMeasure(Measure)
  paste0("Cumulative", Measure)
}

IncrementalMeasureNames = function(Measure){
  Measure = CleanStochasticMeasure(Measure)
  paste0("Incremental", Measure)
}

PriorMeasureNames = function(Measure){
  paste0("PriorCumulative", Measure)
}

CleanStochasticMeasure = function(Measure){
  Measure = gsub("*cumulative*", "", Measure, ignore.case = TRUE)
  Measure = gsub("*incremental*", "", Measure, ignore.case = TRUE)
  Measure = unique(Measure)
}

CreatePriors = function(df, Measure)
{
  cumulCols = CumulativeMeasureNames(Measure)
  incrCols = IncrementalMeasureNames(Measure)
  
  priors = df[cumulCols] - df[incrCols]
  
  priors[df$DevIntervals == min(df$DevIntervals), ] = NA
  
  colnames(priors) = PriorMeasureNames(Measure)
  
  priors

}

CreateCumulative = function(df, Level, Measure)
{
  incrCols = IncrementalMeasureNames(Measure)
  cumulCols = CumulativeMeasureNames(Measure)
  
  lstDF = split(df, f = df[, c("StartDate", names(Level))], drop=TRUE)
  
  lstDF = lapply(lstDF, function(x) {
    x = x[order(x$EvaluationDate), ]
    theMeasures = x[incrCols]
    if (nrow(theMeasures) == 1){
      cumulatives = theMeasures
    } else {
      cumulatives = as.data.frame(apply(theMeasures, 2, cumsum))  
    }
    names(cumulatives) = cumulCols
    x = cbind(x, cumulatives)
  })
  df = do.call("rbind", lOriginYear)
  row.names(df) = NULL
  
  df
  
}

CreateIncrementals = function(df, Level, Measure)
{
  incrCols = IncrementalMeasureNames(Measure)
  cumulCols = CumulativeMeasureNames(Measure)
  
  lstDF = split(df, f = df[, c("StartDate", names(Level))], drop=TRUE)
  
  lstDF = lapply(lstDF, function(x) {
    x = x[order(x$EvaluationDate), ]
    theMeasures = x[cumulCols]
    
    if (nrow(theMeasures) == 1) {
      incrementals = theMeasures
    } else {
      if (nrow(theMeasures) == 2) {
        incrementals = apply(theMeasures, 2, diff)
        incrementals = as.data.frame(t(incrementals))
      } else {
        incrementals = as.data.frame(apply(theMeasures, 2, diff))
      }
      incrementals = rbind(theMeasures[1,], incrementals)
    }
    names(incrementals) = incrCols
    x = cbind(x, incrementals)
  })
  
  df = do.call("rbind", lstDF)
  row.names(df) = NULL
  
  df
  
}

#************************************************************************************************************************
# 1. Class Definition ====
#' @export
is.StochasticMeasure = function(object)
{
  is(object, "StochasticMeasure")
}

checkStochasticMeasure = function(object)
{
  errors = character()
  if (length(errors) == 0) TRUE else errors
}

setClass("StochasticMeasure"
         , representation(OriginPeriod = "OriginPeriod"
                          , Measure = "character"
                          , Level = "list"
                          , DevPeriod = "Period"
                          , DevIntervals = "integer"
                          , Data = "data.frame")
)
#************************************************************************************************************************
# 2. Construction ====
setGeneric("StochasticMeasure", function(OriginPeriod, Measure, Level, DevPeriod, DevIntervals, Data, ...) {
  standardGeneric("StochasticMeasure")
})

#' @export
setMethod("StochasticMeasure", signature=c(OriginPeriod = "OriginPeriod")
          , definition=function(OriginPeriod, Measure, Level, DevPeriod, DevIntervals, Data
                                , FirstEvaluationDate, LastEvaluationDate
                                , OriginPeriodSort, DevIntervalSort
                                , Cumulative=TRUE){
            
            # 0. See if we can run
            if (missing(Level)) {
              stop("Cannot create a StochasticMeasure object without knowing the levels.")
            }
            
            if (missing(DevPeriod)) {
              stop("Cannot create a StochasticMeasure object without knowing the development period.")
            }
            
            # 1. Clean up Level attributes
            if (!missing(Data) & class(Level) == "character") {
              Level = Data[, Level, drop=FALSE]
            }
            
            if (class(Level) == "data.frame") {
              Level = lapply(Level, function(x){
                x = as.character(x)
                x = unique(x)
                x
              })
            } else if (class(Level) != "list") {
              Level = as.list(Level)
            } 
            
            if (is.null(names(Level))) {
              names(Level) = paste0("Level", seq(1, length(Level)))
            }
            
            # 2. Clean up measures
            
            if (missing(Measure)) {
              Measure = character()
            } else if(class(Measure) != "character"){
              Measure = as.character(Measure)
            }
            
            # Strip cumulative and incremental from names
            #Measure = CleanStochasticMeasure(Measure)
            
            if (length(intersect(Measure, names(Level)) != 0)){
              warning("You're using the same name(s) for both measures and levels.")
            }
            
            # 3. Build the kernel data frame
            dfKernel = StochasticKernelDataFrame(OriginPeriod, Level, DevIntervals)
            
            # 4. Add the measures
            if (length(Measure) == 0){
              if (!missing(Data)){
                warning ("Data was passed into the function, but no measures were specified. Data will be ignored.")
              }
              Data = dfKernel
            } else {
              if (missing(Data)) {
                Data = cbind(dfKernel, EmptyMeasureColumns(nrow(dfKernel), Measure))
              } else {
                missingMeasure = setdiff(Measure, colnames(Data))
                missingMeasure = intersect(missingMeasure, Measure)
                if (length(missingMeasure) != 0) {
                  missingMeasure = EmptyMeasureColumns(nrow(dfKernel), missingMeasure)
                  dfKernel = cbind(dfKernel, missingMeasure)
                }
                if (!missing(OriginPeriodSort) & !missing(DevIntervalSort)){
                  Data = Data[, c(Measure, names(Level), OriginPeriodSort, DevIntervalSort), drop=FALSE]
                  Data = OrderStochasticMeasureData(Data, OriginPeriodSort, DevIntervalSort, Level)
                } else {
                  Data = Data[, Measure, drop=FALSE]
                }
                Data = cbind(dfKernel, Data[, Measure, drop=FALSE])
              }
            }
            
            # 5. Add the evaluation dates
            Data$EvaluationDate = GetEvaluationDates(Data, DevPeriod)
            
            # Subset to the appropriate portion of the triangle
            if (missing(LastEvaluationDate)) LastEvaluationDate = max(OriginPeriod$EndDate)
            if (missing(FirstEvaluationDate)) FirstEvaluationDate = min(OriginPeriod$EndDate)
            
            Data = subset(Data, EvaluationDate >= FirstEvaluationDate)
            Data = subset(Data, EvaluationDate <= LastEvaluationDate)
            
            # Create required cumulative and incurred
            # Must normalize the meaure names accordingly
            whichCols = colnames(Data) %in% Measure
            if (Cumulative) {
              colnames(Data)[whichCols] = CumulativeMeasureNames(Measure)
              Data = merge(Data, CreateIncrementals(Data, Level, Measure))
            } else {
              colnames(Data)[whichCols] = IncrementalMeasureNames(Measure)
              Data = merge(Data, CreateCumulative(Data, Level, Measure))
            }
            
            # Form the prior
            Measure = CleanStochasticMeasure(Measure)
            Data = cbind(Data, CreatePriors(Data, Measure))
            
            # Add a factor column for the DevInterval. We'll need this when plotting
            Data$DevIntervalFactor = as.factor(Data$DevIntervals)
            
            row.names(Data) = NULL
            
            scm = new("StochasticMeasure"
                      , OriginPeriod = OriginPeriod
                      , Measure = Measure
                      , Level = Level
                      , DevPeriod = DevPeriod
                      , DevIntervals = DevIntervals
                      , Data = Data)
})

#************************************************************************************************************************
# 3. Properties ====
#' @export
setMethod("length", signature(x="StochasticMeasure"), definition=function(x){
  if (length(x@Level) < 1) {
    return (0)
  }
  
  iLength = 1
  for (i in 1:length(x@Level)){
    iLength = iLength * length(x@Level[[i]])
  }
  
  iLength
})

#' @export
setMethod("MeasureNames", signature(x="StochasticMeasure"), definition=function(x, Stem=TRUE){
  if (Stem){
    strNames = x@Measure
  } else {
    strNames = c(IncrementalMeasureNames(measure), CumulativeMeasureNames(measure), PriorMeasureNames(measure))
  }
  
  strNames
  
})

#' @export
setMethod("LevelNames", signature(x="StochasticMeasure"), definition=function(x){
  names(x@Level)
})

#' @export 
setMethod("show", signature(object="StochasticMeasure"), definition=function(object){
  cat("StochasticMeasure object \n")
  cat("Measures:\t", paste(names(object@Measure), collapse=", "), "\n")
  cat("Levels:\t", paste(names(object@Level), collapse=", "), "\n")
  cat("Length:\t", length(object), "\n")
  cat("\nThe OriginPeriod slot is:\n")
  cat("\t\t",show(object@OriginPeriod), "\n")
})

#************************************************************************************************************************
# 4. Accessors ====
#' @export 
setMethod("[", signature(x="StochasticMeasure"), definition=function(x, i, j, k){
  
})

setMethod("[<-", signature(x = "StochasticMeasure"), definition=function(x, i, j, k, ..., value) {
})

setMethod("$", signature(x = "StochasticMeasure"), function(x, name) {
})

setMethod("$<-", signature(x = "StochasticMeasure"), function(x, name, value) {
  })

#************************************************************************************************************************
# 5. Comparison ====

#************************************************************************************************************************
# 6. Conversion ====
#' @export
setMethod("as.data.frame", signature("StochasticMeasure"), function(x, ...){
  x@Data
})

#' @export
melt.StochasticMeasure = function(data){
  df = data@Data
  measure = MeasureNames(data)
  mdf = melt(df, id.vars=c("StartDate", "EndDate", "Moniker", "EvaluationDate", "DevIntervals", LevelNames(data))
#             , measure.vars=
             , variable.name="Measure")
  mdf
}

#************************************************************************************************************************
# 7. Concatenate ====
JoinStochaticMeasureElements = function(elements){
}

#' @export
rbind.StochaticMeasure = function(..., deparse.level=1){
}

setMethod("c", signature(x="StochasticMeasure"), definition=function(x, ...){
})

#************************************************************************************************************************
# 8. Persistence ====
# setMethod("write.csv", signature=c(x="StochaticMeasure"), definition=function(x, ...){
# })

setMethod("write.excel", signature=c(object = "StochasticMeasure", file="character", overwrite="logical")
          , definition=function(object, file, overwrite=FALSE, byGroup=TRUE, sheetName){
            
})

#************************************************************************************************************************
# 9. Display ====
#' @export
setMethod("plot", signature(x="StochasticMeasure", y="missing")
          , definition = function(x, TimeAxis="DevInterval", Measure, Group="Moniker", FacetFormula){
            
            mdf = melt(x)
            
            if (missing(Measure)) Measure = IncrementalMeasureNames(MeasureNames(x))
            if (missing(Group)) Group = "Moniker"
            if (missing(TimeAxis)) TimeAxis="DevIntervals"
            
            if (sum(mdf$Measure %in% Measure) == 0) stop("Improper Measure specification. Please specify Measures for this object.")
            
            mdf = mdf[mdf$Measure %in% Measure, ]
            
            plt = ggplot(mdf, aes_string(x=TimeAxis, y="value", group=Group, color=GroupParameter)) + geom_line()
            
            if (missing(FacetFormula)){
              facetCols = colnames(mdf)[!colnames(mdf) %in% c("StartDate", "EndDate", "Moniker", "EvaluationDate", "DevIntervals", Group, "value")]
              
              FacetFormula = paste(facetCols, collapse="+")
              FacetFormula = as.formula(paste("~", FacetFormula))
              
              facetRow = length(unique(mdf[, facetCols]))
              FacetRow = floor(sqrt(facetRow)) + 1
              
              plt = plt + facet_wrap(FacetFormula, facetRow)  
            } else {
              plt = plt + facet_grid(FacetFormula)
            }
            
            plt
            
})
