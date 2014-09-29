#' StochasticMeasure class
#'
#' @include HelperFunctions.R
#' @include NewGenerics.R
#' @include OriginPeriod.R
#' 
#' @docType class
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
StochasticKernelDataFrame = function(op, levels, EvalDates, Lags, DevPeriod){
  op = as.data.frame(op)
  op = op[, c("StartDate", "EndDate", "Moniker")]
  
  levels$EvaluationDate = EvalDates
#  levels$Lag = Lags
  df = merge(op, expand.grid(levels, stringsAsFactors=FALSE))
  
  df = subset(df, EvaluationDate > StartDate)
  
  df$Lag = GetLags(df, DevPeriod)
  df = subset(df, Lag %in% Lags)
  df = subset(df, Lag >= 1)
  
#   df = OrderStochasticMeasureData(df, "Moniker", "EvaluationDate", levels)
  df
}

OrderStochasticMeasureData = function(df, opSort, devSort, levels){
  df = df[ do.call(order, df[, c(names(levels), devSort, opSort)]), ]
  row.names(df) = NULL
  df
}

# GetEvaluationDates = function(df, period){
#   EvaluationDate = df$StartDate + df$Lag * period - days(1)
#   EvaluationDate
# }
# 
GetLags = function(df, period){
  Lags = difftime(df$EvaluationDate, df$StartDate, units="days")
  Lags = suppressMessages(Lags / (period / days(1)))
  Lags = round(Lags)
  Lags = as.integer(Lags)
  Lags
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
  Measure = CleanStochasticMeasure(Measure)
  paste0("PriorCumulative", Measure)
}

CleanStochasticMeasure = function(Measure){
  Measure = gsub("cumulative", "", Measure, ignore.case = TRUE)
  Measure = gsub("incremental", "", Measure, ignore.case = TRUE)
  Measure = gsub("prior", "", Measure, ignore.case = TRUE)
  Measure = unique(Measure)
}

CreatePriors = function(df, Level, Measure)
{
  cumulCols = CumulativeMeasureNames(Measure)
  incrCols = IncrementalMeasureNames(Measure)
  priorCols = PriorMeasureNames(Measure)
  
  priors = df[cumulCols] - df[incrCols]
  colnames(priors) = priorCols
  df = cbind(df, priors)
  
  lstDF = split(df, f = df[, c("StartDate", names(Level))], drop=FALSE)
  lstDF = lapply(lstDF, function(x){
    x[x$EvaluationDate == min(x$EvaluationDate), priorCols] = NA
    x
  })
  df = do.call("rbind", lstDF)
  
  df = OrderStochasticMeasureData(df, "StartDate", "Lag", Level)
  priors = df[priorCols]
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
  df = do.call("rbind", lstDF)
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

SingleAccessStochasticMeasure = function(x, name, UniqueAttribute=FALSE){
  
  if (length(name) > 1) stop("Cannot access more than one value at a time. Use the `[` operator for multiple access.")
  
  # 1. Is it a slot?
  if (name %in% slotNames(x)){
    return (slot(x, name))
  }
  
  # 2. Is it a LevelName?
  if (name %in% LevelNames(x)) {
    if (UniqueAttribute) {
      return (x@Level[[name]]) 
    } else {
      return (x@Data[, name])
    }
  } 
  
  # 3. Is it a data column?
  # First check to see if this corresponds to the name of a measure
  if (name %in% colnames(x@Data)){
    return (x@Data[, name])
  } 
  
  # 4. Is it an element of one of the levels?
  # The name is not a measure or level name. Check to see if it corresponds to one of the values in the Level list.
  lst = x@Level
  for (i in 1:length(lst)){
    level = lst[[i]]
    if (name %in% level){
      df = x@Data[[names(lst)[i]]]
      whichRows = (df == name)
      df = x@Data[whichRows, ]
      row.names(df) = NULL
      lst[[i]] = name
      sm = StochasticMeasure(OriginPeriod = x@OriginPeriod
                             , Level = lst
                             , Measure = x@Measure
                             , DevPeriod = x@DevPeriod
                             , EvaluationDates = df$EvaluationDate
                             , Data = df)
      return(sm)
    }
  }
  
  # 5. Are we looking for an OriginPeriod?
  if (name %in% x@OriginPeriod@Moniker){
    df = x@Data[x@Data$Moniker == name, ]
    row.names(df) = NULL
    op = x@OriginPeriod[name]
    sm = StochasticMeasure(OriginPeriod = op
                           , Level = lst
                           , Measure = CumulativeMeasureNames(x@Measure)
                           , DevPeriod = x@DevPeriod
                           , EvaluationDates = df$EvaluationDate
                           , Data = df)
    return(sm)
  }
  
  stop("Name does not correspond to any slot, level, level attribute, data column or OriginPeriod moniker.")
  
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
                          , Level = "list"
                          , Measure = "character"
                          , DevPeriod = "Period"
#                          , EvaluationDates = "Date"
                          , Data = "data.frame")
)
#************************************************************************************************************************
# 2. Construction ====
setGeneric("StochasticMeasure", function(OriginPeriod, Level, Measure, DevPeriod, EvaluationDates, Data, ...) {
  standardGeneric("StochasticMeasure")
})

# 

#' @export
setMethod("StochasticMeasure", signature=c(OriginPeriod = "OriginPeriod")
          , definition=function(OriginPeriod, Level, Measure, DevPeriod, EvaluationDates, Data
                                , Lags
                                , Cumulative=TRUE
                                , FirstEvaluationDate, LastEvaluationDate
                                , OriginPeriodSort="StartDate"
                                , LagSort="Lag"){
            
            # 0. See if we can run
            if (missing(Level)) {
              stop("Cannot create a StochasticMeasure object without knowing the levels.")
            }
            
            if (missing(DevPeriod)) {
              stop("Cannot create a StochasticMeasure object without knowing the development period.")
            }
            
            if (missing(OriginPeriodSort)) OriginPeriodSort = "StartDate"
            if (missing(LagSort)) LagSort = "Lag"
            
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
            if(missing(EvaluationDates)){
              if (missing(FirstEvaluationDate)) FirstEvaluationDate = min(OriginPeriod$EndDate)
              if (missing(LastEvaluationDate)) LastEvaluationDate = max(OriginPeriod$EndDate)
              EvaluationDates = DateSequence(FirstEvaluationDate, LastEvaluationDate, DevPeriod)
            }
            
            EvaluationDates = unique(EvaluationDates)
            Lags = unique(Lags)
            
            dfKernel = StochasticKernelDataFrame(OriginPeriod, Level, EvaluationDates, Lags, DevPeriod)
#            dfKernel$Lag = GetLags(dfKernel, DevPeriod)
            
#             if (missing(MaxLag)){
#               MaxLag = dfKernel$Lag[dfKernel$StartDate == min(dfKernel$StartDate)]
#               MaxLag = max(MaxLag)
#             }

#            dfKernel = subset(dfKernel, Lag <= MaxLag)
            
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
                  Data = Data[, c(Measure, names(Level), OriginPeriodSort, LagSort), drop=FALSE]
                if (nrow(Data) != nrow(dfKernel)) {
#                   warning("Length of 'Data' argument is not consistent with OriginPeriod and EvaluationDates. Did you forget to pass in the Evaluation Dates?") 
#                   return(dfKernel)
                  stop("Length of 'Data' argument is not consistent with OriginPeriod and EvaluationDates. Did you forget to pass in the Evaluation Dates?")
                }
                Data = OrderStochasticMeasureData(Data, OriginPeriodSort, LagSort, Level)
                dfKernel = OrderStochasticMeasureData(dfKernel, "StartDate", "Lag", Level)
                Data = cbind(dfKernel, Data[, Measure, drop=FALSE])
              }
            }
            
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
            dfPrior = CreatePriors(Data, Level, Measure)
            Data = OrderStochasticMeasureData(Data, "StartDate", "Lag", Level)
            Data = cbind(Data, dfPrior)
            
            row.names(Data) = NULL
            
            scm = new("StochasticMeasure"
                      , OriginPeriod = OriginPeriod
                      , Measure = Measure
                      , Level = Level
                      , DevPeriod = DevPeriod
#                       , EvaluationDates = EvaluationDates
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
  
  strNames = x@Measure
  
  if (!Stem) {
    strNames = c(IncrementalMeasureNames(strNames), CumulativeMeasureNames(strNames), PriorMeasureNames(strNames))
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
setMethod("$", signature(x = "StochasticMeasure"), function(x, name) {
  
  SingleAccessStochasticMeasure(x, name, FALSE)
  
})

#' @export
setMethod("[[", signature(x = "StochasticMeasure"), function(x, i, UniqueAttribute=TRUE) {
  
  if (class(i) == "numeric"){
    i = x@Level[[i]]  
  } 
  
  SingleAccessStochasticMeasure(x, i, UniqueAttribute)
  
})

#' @export 
setMethod("[", signature(x="StochasticMeasure"), definition=function(x, i, j, ..., OriginPeriod, EvaluationDate, Lag, drop=TRUE){
  
  df = x@Data
  op = x@OriginPeriod
  
  if(missing(i)) {
    i = rep(TRUE, nrow(df))
  }
  
  if (missing(j)) {
    j = CumulativeMeasureNames(x@Measure)
  } else {
    whichCols = intersect(colnames(df), j)
    if (length(whichCols) == 0){
      stop ("Improper Measure specification. No columns returned.")
    }
    j = CumulativeMeasureNames(j)
    whichCols = c(whichCols, LevelNames(x))
    whichCols = c("StartDate", "EndDate", "Moniker", whichCols)
    whichCols = c(whichCols, "Lag", "EvaluationDate")
    df = df[, whichCols]
  }
  
  if (!missing(OriginPeriod)){
    op = x@OriginPeriod[OriginPeriod]
    i = i & (df$Moniker %in% op$Moniker)
  }
  
  if (!missing(EvaluationDate)){
    i = i & (df$EvaluationDate %in% EvaluationDate)
  }
  
  if (!missing(Lag)){
    i = i & (df$Lag %in% Lag)
  }
  
  df = df[i, j, drop=drop]
  
  if (class(df) == "data.frame"){
    df2 = x@Data
    df2 = df2[i, ]
    df = cbind(df, df2[, c("StartDate", "Lag", "EvaluationDate", "Moniker", LevelNames(x))])
    sm = StochasticMeasure(op
                           , Level=LevelNames(x)
                           , Measure=j
                           , DevPeriod=x@DevPeriod
                           , EvaluationDates=df$EvaluationDate
                           , Lags=df$Lag
                           , Data=df)
    return (sm)
  } else {
    return (df)
  }
  
})

setMethod("UpperTriangle", signature(object="StochasticMeasure"), definition=function(object){
  # The upper triangle is all observations on or before the first evaluation for the most recent element of the OriginPeriod 
  evalDate = object$EvaluationDate[object$StartDate == max(object$StartDate)]
  scm = object[object$EvaluationDate <= min(evalDate)]
  scm
})

setMethod("LatestDiagonal", signature(object="StochasticMeasure"), definition=function(object){
  scm = object[object$EvaluationDate == max(object$EvaluationDates)]
  scm
})

setMethod("Diagonal", signature(object="StochasticMeasure", EvaluationDate="ANY")
          , definition=function(object, EvaluationDate){
            scm = object[object$EvaluationDate == EvaluationDate]
            scm
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
  mdf = melt(df, id.vars=c("StartDate", "EndDate", "Moniker", "EvaluationDate", "Lag", LevelNames(data))
#             , measure.vars=
             , variable.name="Measure")
  mdf
}

#' @export 
setMethod("LongToWide", signature("StochasticMeasure"), function(object, TimeAxis="Lag"){
  mdf = melt(object)
  theFormula = paste(LevelNames(object), collapse="+")
  theFormula = paste(theFormula, "Measure", "StartDate", "Moniker", sep="+")
  theFormula = paste(theFormula, "~", TimeAxis)
  df = dcast(mdf, as.formula(theFormula), sum)
  df
})

#************************************************************************************************************************
# 7. Concatenate ====
JoinStochasticMeasureElements = function(elements){
  
  # 1. Sort out OriginPeriod
  op = lapply(elements, slot, "OriginPeriod")
  op = lapply(op, as.data.frame)
  op = do.call(rbind, op)
  op = unique(op)
  OriginPeriod = OriginPeriod(StartDate=op$StartDate, Period=op$Period[1], Type=as.character(op$Type[1]), Moniker=as.character(op$Moniker))
  
  # 2. Get DevPeriod
  DevPeriod = lapply(elements, slot, "DevPeriod")
  if(length(unique(DevPeriod)) != 1) {
    stop("DevPeriods must be equal to combine StochasticMeasures.")
  }
  DevPeriod = DevPeriod[[1]]
  
  # 3. Get Measures
  Measures = lapply(elements, slot, "Measure")
  if(length(unique(Measures)) != 1) {
    stop("Measures must be equal to combine StochasticMeasures.")
  }
  Measures = Measures[[1]]
  
  # 4. Get Levels
  Levels = lapply(elements, slot, "Level")
  levelNames = lapply(Levels, names)
  if(length(unique(levelNames)) != 1) {
    stop("LevelNames must be equal to combine StochasticMeasures.")
  }
  levelNames = levelNames[[1]]
  
  # 5. Combine Data
  # When combining data, we make our lives easier by assuming cumulative measures
  Data = lapply(elements, slot, "Data")
  Data = do.call(rbind, Data)  
  Data = Data[, c("StartDate", "Moniker", "EvaluationDate", "Lag", levelNames, CumulativeMeasureNames(Measures))]
  
  x = StochasticMeasure(OriginPeriod = OriginPeriod
                        , Measure = CumulativeMeasureNames(Measures)
                        , Level = levelNames
                        , DevPeriod = DevPeriod
                        , EvaluationDates = Data$EvaluationDate
                        , Lags=Data$Lag
                        , Data = Data)
  
  x
}

#' @export
rbind.StochasticMeasure = function(..., deparse.level=1){
  elements = list(...)
  blnSCMs = sapply(elements, is.StochasticMeasure)
  elements = elements[blnSCMs]
  
  scm = JoinStochasticMeasureElements(elements)
  scm
}

#' @export
setMethod("c", signature(x="StochasticMeasure"), definition=function(x, ...){
  elements = list(...)
  blnSCMs = sapply(elements, is.StochasticMeasure)
  elements = elements[blnSCMs]
  
  if (length(elements) == 0) {
    return (x)
  } else if (length(elements) == 1){
    scm = elements[[1]]
  } else {
    scm = JoinStochasticMeasureElements(elements)
  }
  scm = rbind(x, scm)
  scm
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
            if (missing(TimeAxis)) TimeAxis="Lag"
            
            if (sum(mdf$Measure %in% Measure) == 0) stop("Improper Measure specification. Please specify Measures for this object.")
            
            mdf = mdf[mdf$Measure %in% Measure, ]
            
            plt = ggplot(mdf, aes_string(x=TimeAxis, y="value", group=Group, color=Group)) + geom_line()
            
            if (missing(FacetFormula)){
              facetCols = colnames(mdf)[!colnames(mdf) %in% c("StartDate", "EndDate", "Moniker", "EvaluationDate", "Lag", Group, "value")]
              
              FacetFormula = paste(facetCols, collapse="+")
              FacetFormula = as.formula(paste("~", FacetFormula))
              
              facetRow = length(unique(mdf[, facetCols]))
              facetRow = floor(sqrt(facetRow)) + 1
              
              plt = plt + facet_wrap(FacetFormula, facetRow, scales="free")  
            } else {
              plt = plt + facet_grid(FacetFormula, scales="free")
            }
            
            plt
            
})
