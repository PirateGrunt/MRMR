#'
#' StochasticMeasure class
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
                          , Level  "list"
                          , EvaluationDate = "Date"
                          , .data = "data.frame")
)
#************************************************************************************************************************
# 2. Construction ====
setGeneric("StochasticMeasure", function(OriginPeriod, Measure, Level, EvaluationDate, .data, ...) {
  standardGeneric("StochasticMeasure")
})

#' @export
setMethod("StochasticMeasure", signature=c(OriginPeriod = "OriginPeriod", Measure="ANY", Level="ANY", EvaluationDate="ANY")
          , definition=function(){
            
})

#************************************************************************************************************************
# 3. Properties ====
#' @export
setMethod("length", signature(x="StochasticMeasure"), definition=function(x){
  nrow(x@.data)
})

#' @export
setMethod("MeasureNames", signature(x="StochasticMeasure"), definition=function(x){
  x@Measure
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
# Accessors extract from OriginPeriod, Level and Measure
#' @export 
setMethod("[", signature(x="StochasticMeasure"), definition=function(x, i, j, k){
  
  if(missing(i)) i = NA
  if(missing(j)) j = NA
  if(missing(k)) k = NA
  
  # if the origin period is not specified, then return all of them
  if (any(is.na(i))) {
    i = 1:(length(x@OriginPeriod))
  }
  
  op = x@OriginPeriod
  if (class(i) == "character") {
    i = which(op@Moniker == i)
  } 
  
  op = op[i]
  
  if (any(is.na(j))) {
    j = 1:(length(x@Measure))
  }
  
  if (class(j) == "character") {
    j = match(j, x@Measure)
  } 
  measure = x@Measure[j]
  
  if (any(is.na(k))) {
    k = names(x@Level)
  }
  
  if (class(k) == "character") {
    k = match(k, LevelNames(x))
  }
  level = LevelNames(x)[k]
  
  df = as.data.frame(x)
  keepCols = c("StartDate", "EndDate", "Moniker", level, measure)
  df = df[df$StartDate %in% op$StartDate, keepCols]
  
  sm = StaticMeasure(op, measure, level, df)
  sm
})

setMethod("[<-", signature(x = "StochasticMeasure"), definition=function(x, i, j, k, ..., value) {
})

#' @export
setMethod("$", signature(x = "StaticMeasure"), function(x, name) {
  if (name %in% slotNames(x)){
    slot(x, name)
  } else if (name %in% MeasureNames(x)){
    x@.data[, name]
  } else if (name %in% LevelNames(x)) {
    x@Level[[name]]
  } else {
    stop("Name does not correspond to any slot, level or measure.")
  }
})

setMethod("$<-", signature(x = "StochasticMeasure"), function(x, name, value) {
  
  if (length(name) > 1) stop("Cannot assign more than one value at a time.")
  
  if (name %in% slotNames(x)){
    stop("Slot names may not be used as value names.")
  } else if (name %in% MeasureNames(x)){
    x@.data[name] = value
  } else if (name %in% LevelNames(x)) {
    x@Level[[name]] = value
    x = StochasticMeasure(OriginPeriod = x@OriginPeriod, Level = x@Level, Measure = MeasureNames(x), .data = x@.data)
  } else {
    if (length(value) == nrow(x@.data)) {
      x@.data = cbind(x@.data, value)
      colnames(x@.data)[length(colnames(x@.data))] = name  
    }
  }
  
  x
  
})

#************************************************************************************************************************
# 5. Comparison ====

#************************************************************************************************************************
# 6. Conversion ====
#' @export
setMethod("as.data.frame", signature("StochasticMeasure"), function(x, ...){
  x@.data
})

setMethod("as.formula", signature(object="StochasticMeasure"), definition=function(object){
  facetFormula = LevelNames(object)
  firstHalf = ceiling(length(facetFormula) / 2)
  secondHalf = (firstHalf+1):length(facetFormula)
  firstHalf = 1:firstHalf
  
  LHS = paste(LevelNames(object)[firstHalf], collapse="+")
  RHS = paste(LevelNames(object)[secondHalf], collapse="+")
  facetFormula = as.formula(paste(LHS, "~", RHS))
  
  facetFormula
})
#************************************************************************************************************************
# 7. Concatenate ====
JoinStochaticMeasureElements = function(elements){
  op = lapply(elements, slot, "OriginPeriod")
  op = lapply(op, as.data.frame)
  op = do.call(rbind, op)
  op = unique(op)
  OriginPeriod = OriginPeriod(StartDate=op$StartDate, Period=op$Period[1], Type=as.character(op$Type[1]), Moniker=as.character(op$Moniker))
  
  Measures = lapply(elements, slot, "Measure")
  if(length(unique(Measures)) != 1) {
    stop("Measures must be equal to combine StaticMeasures.")
  }
  Measures = Measures[[1]]
  
  Levels = lapply(elements, slot, "Level")
  levelNames = lapply(Levels, names)
  if(length(unique(levelNames)) != 1) {
    stop("LevelNames must be equal to combine StaticMeasures.")
  }
  levelNames = levelNames[[1]
                          ]
  .data = lapply(elements, slot, ".data")
  .data = do.call(rbind, .data)  
  
  x = StochaticMeasure(OriginPeriod = OriginPeriod, Measure=Measures, Level=levelNames, EvaluationDate = NULL, .data=.data)
  x
}

#' @export
rbind.StochaticMeasure = function(..., deparse.level=1){
  elements = list(...)
  blnSMs = sapply(elements, is.StochaticMeasure)
  elements = elements[blnSMs]
  
  sm = JoinStochaticMeasureElements(elements)
  sm
}

#' @export
setMethod("c", signature(x="StochaticMeasure"), definition=function(x, ...){
  elements = list(...)
  blnOPs = sapply(elements, is.StochaticMeasure)
  elements = elements[blnOPs]
  
  if (length(elements) == 0) {
    return (x)
  } else if (length(elements) == 1){
    sm = elements[[1]]
  } else {
    sm = JoinStochaticMeasureElements(elements)
  }
  sm = rbind(x, sm)
  sm
})

#************************************************************************************************************************
# 8. Persistence ====
setMethod("write.csv", signature=c(x="StochaticMeasure"), definition=function(x, ...){
  df = as.data.frame(x)
  write.csv(df, ...)
})

setMethod("write.excel", signature=c(object = "StochaticMeasure", file="character", overwrite="logical")
          , definition=function(object, file, overwrite=FALSE, byGroup=TRUE, sheetName){
            
            if (file.exists(file) & !overwrite){
              stop("Excel file already exists. Either enter a new filename or set the overwrite parameter to TRUE.")
            }
            
            wbk = loadWorkbook(file, create=TRUE)
            
            writeSheet = function(wbk, shtName, df){
              createSheet(wbk, name=shtName)
              writeWorksheet(wbk, df, shtName, startRow = 1, header=TRUE)
            }
            
            df = as.data.frame(object)
            
            if (byGroup){
              lstGroups = split(df, df[, LevelNames(object)])
              dfLevel = df[, LevelNames(object)]
              dfLevel = unique(dfLevel)
              sheetNames = do.call(paste, c(dfLevel[names(dfLevel)], sep = ""))
              for (i in 1:(length(lstGroups))){
                writeSheet(wbk, sheetNames[i], lstGroups[[i]])
              }
            } else {
              if (missing(sheetName)) sheetName = "StaticMeasure"
              
              writeSheet(wbk, sheetName, df)
            }
            
            saveWorkbook(wbk)
            
          })

#************************************************************************************************************************
# 9. Display ====
#' @export
setMethod("plot", signature(x="StochasticMeasure", y="missing"), definition = function(x, facetFormula, ...){
            
  df = as.data.frame(x)
            
  mdf = melt(df, id.vars=c("StartDate", "EndDate", "Moniker", LevelNames(x)), measure.vars=MeasureNames(x))
  
  if (missing(facetFormula)) facetFormula = as.formula(x)
  
  plt = ggplot(mdf, aes(x=Moniker, y = value, group=variable)) + geom_line() + facet_grid(facetFormula)
  plt
            
})
