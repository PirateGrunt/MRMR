#'
#' StaticMeasure class
#' 
#' @docType class
#' 
#' @seealso \code{\link{StaticMeasureConstructor}}
#' 
#' @name StaticMeasure-class
#' @rdname StaticMeasure-class
#' @exportClass StaticMeasure
#' 
#' @description
#' A StaticMeasure holds multilevel/multivariate data associated with an OriginPeriod.
#' 
#' @details
#' 
#' A StaticMeasure is effectively a time series, or set of time series. Because each value of a StaticMeasure
#' corresponds to an element of an OriginPeriod- which represents an interval of time- the StaticMeasure does not represent
#' an observation at a single point in time. In an insurance context, common StaticMeasures would include written or earned
#' premium, written or earned exposure, number of insured locations for a fixed duration and the like. In-force premium or 
#' in-force exposure would not constitute a StaticMeasure as this is a measure at a single point in time.
#' 
#' A StaticMeasure is so called to distinguish it from a StochasticMeasure. A StochasticMeasure also has a correspondence
#' to a single element of an OriginPeriod, but its value changes from one observation to the next. This would be the case for
#' number of claims, paid losses and the like. This would also be true for loss-sensitive premium such as exists in a retrospectively
#' rated policy.
#' 
#' A StaticMeasure object may hold more than one measure for each time period. Earned premium and policy count may both be stored
#' in a single object, for example. This is done to facilitate a multivariate approach to the loss reserving problem. Stochastic 
#' responses may depend on more than one variable.
#' 
#' \strong{Levels}
#' Each StaticMeasure may be assigned level parameters which enable multiple sets of measures to be stored in the 
#' same object. This will, in turn, facilitate construction of a hierarchical analysis model. The levels may be
#' of any arbitraty complexity, irrespective of the number of elements of the StaticMeasure object. For example, one
#' may have a single StaticMeasure which uses more than one level element: GL, prem/ops, California, for example. 
#' The object is constrained in that there must be enough levels to describe each element of the object. The constructor examples
#' should make this clear.
#' 
#' If the user does not specify level attributes, the default value of "All" is used.
#' 
#' \strong{StaticMeasure construction}
#' To construct
#' 
#' Factors will be converted to characters by default. This is advisable.
#' 
#' \strong{Member access}
#' Elements of a StaticMeasure object are accessed in a manner similar to those for data frames. However, a StaticMeasure has two, not
#' three dimensions. One way of thinking of this is that a StaticMeasure is a list of data frames. The indexing convention is as follows:
#' Because the "data frame" may be thought of as having rows specific by OriginPeriod and columns specified by the names of measured 
#' values, the i and i indices are used to specify these two dimensions. The third dimension is used to identify the Level. For a 
#' StochasticMeasure object, a fourth dimension is used to identify the evalution date, or development lag.
NULL

#**********************************************************************************************
# 0. Helper functions ====
FactorsToStrings = function(df){
  df = lapply(df, function(x){
    if (class(x) == "factor") {
      x = as.character(x)
    }
    x
  })
  df = as.data.frame(df, stringsAsFactors = FALSE)
  df
}

KernelDataFrame = function(op, levels){
  op = as.data.frame(op)
  op = op[, c("StartDate", "EndDate", "Moniker")]
  df = merge(op, expand.grid(levels))
  df
}

EmptyMeasureColumns = function(nrow, names){
  ncol = length(names)
  df = rep(NA, nrow * ncol)
  df = matrix(df, nrow, ncol)
  df = as.data.frame(df)
  colnames(df) = names
  df
}

#************************************************************************************************************************
# 1. Class Definition ====
#' @export
is.StaticMeasure = function(object)
{
  is(object, "StaticMeasure")
}

checkStaticMeasure = function(object)
{
  errors = character()
  if (length(errors) == 0) TRUE else errors
}

setClass("StaticMeasure"
         , representation(OriginPeriod = "OriginPeriod"
                          , Measure = "character"
                          , Level = "list"
                          , .data = "data.frame")
)
#************************************************************************************************************************
# 2. Construction ====

setGeneric("StaticMeasure", function(OriginPeriod, Measure, Level, .data, ...) {
  standardGeneric("StaticMeasure")
})

#' @export 
setMethod("StaticMeasure", signature=c(OriginPeriod="OriginPeriod", Measure="ANY", Level="ANY", .data="ANY")
          , definition=function(OriginPeriod, Measure, Level, .data) {
            
            if (missing(Level)) {
              stop("Cannot create a static measure without knowing the levels")
            }
            
            if (!missing(.data) & class(Level) == "character") {
              Level = .data[, Level, drop=FALSE]
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
            
            if (missing(Measure)) {
              Measure = character()
            } 
            
            if (length(intersect(Measure, names(Level)) != 0)){
              warning("You're using the same name(s) for both measures and levels.")
            }
            
            df = KernelDataFrame(OriginPeriod, Level)
            
            if (length(Measure) == 0){
              .data = df
            } else {
              if (missing(.data)) {
                .data = cbind(df, EmptyMeasureColumns(nrow(df), Measure))
              } else {
                .data = cbind(df, .data[, Measure, drop=FALSE])
              }
            }
            
            sm = new("StaticMeasure"
                     , OriginPeriod = OriginPeriod
                     , Measure = Measure
                     , Level = Level
                     , .data = .data)
            sm
})

#************************************************************************************************************************
# 3. Properties ====
#' @export
setMethod("length", signature(x="StaticMeasure"), definition=function(x){
  # TODO: This should reflect the total number of rows
  nrow(x@.data)
})

#' @export
setMethod("MeasureNames", signature(x="StaticMeasure"), definition=function(x){
  x@Measure
})

#' @export
setMethod("MeasureNames<-", signature(x="StaticMeasure", value="character"), definition=function(x, value){
  oldNames = x@Measure
  whichCols = colnames(x@.data) %in% oldNames
  colnames(x@.data)[whichCols] = value
  x@Measure = value
  x
})

#' @export
setMethod("LevelNames", signature(x="StaticMeasure"), definition=function(x){
  names(x@Level)
})

#' @export
setMethod("LevelNames<-", signature(x="StaticMeasure", value="character"), definition=function(x, value){
  oldNames = LevelNames(x)
  whichCols = colnames(x@.data) %in% oldNames
  colnames(x@.data)[whichCols] = value
  names(x@Level) = value
  x
})

#' @export 
setMethod("show", signature(object="StaticMeasure"), definition=function(object){
  cat("StaticMeasure object \n")
  cat("Measures:\t", paste(MeasureNames(object), collapse=", "), "\n")
  cat("Levels:\t", paste(LevelNames(object), collapse=", "), "\n")
  cat("Length:\t", length(object), "\n")
  cat("\nThe OriginPeriod slot is:\n")
  cat("\t\t",show(object@OriginPeriod), "\n")
})

#************************************************************************************************************************
# 4. Accessors ====
# Accessors extract from OriginPeriod, Level and Measure
#' @export 
setMethod("[", signature(x="StaticMeasure"), definition=function(x, i, j, k){
  
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

setMethod("[<-", signature(x = "StaticMeasure", value = "StaticMeasure"), definition=function(x, i, j, k, ..., value) {
  
  if (j %in% MeasureNames) changeMeasure = TRUE
  if (k %in% LevelNames) changeLevel = TRUE
  
  if (changeMeasure & changeLevel) {
    stop("Cannot set measures and levels simultaneously.")
  }
  
  if (!changeMeasure & !changeLevel) {
    stop("Must change either measure or level.")
  }
  
  if (changeLevel) {
    df = x@Level
    df[, ]
  }
  # if the origin period is not specified, then return all of them
  if (any(is.na(i))) {
    i = 1:(length(x@OriginPeriod))
  }
  
  op = x@OriginPeriod[i]
  
  if (any(is.na(j))) j = colnames(x@Measure)
  
  if (any(is.na(k))) {
    level = data.frame(x@Level)
    measureIndex = 1:(nrow(x@Level))
  } else {
    if (is.character(k)) {
      z = as.matrix(sapply(x@Level, function(y) y %in% k))
      k = which(rowSums(z) != 0)  
    }
    
    level = data.frame(x@Level[k, ])
    measureIndex = k
  }
  
  measureIndex = (measureIndex - 1) * length(x@OriginPeriod)
  measureIndex = c(sapply(measureIndex, function(x) x + i))
  measure = x@Measure[measureIndex, j, drop=FALSE]
  
  sm = StaticMeasure(op, measure, level)
  x
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
 
setMethod("$<-", signature(x = "StaticMeasure"), function(x, name, value) {
  
  if (length(name) > 1) stop("Cannot assign more than one value at a time.")
  
  if (name %in% slotNames(x)){
    stop("Slot names may not be used as value names.")
  } else if (name %in% MeasureNames(x)){
    x@.data[name] = value
  } else if (name %in% LevelNames(x)) {
    x@Level[[name]] = value
    x = StaticMeasure(OriginPeriod = x@OriginPeriod, Level = x@Level, Measure = MeasureNames(x), .data = x@.data)
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
setMethod("as.data.frame", signature("StaticMeasure"), function(x, ...){
  x@.data
})

setMethod("as.formula", signature(object="StaticMeasure"), definition=function(object){
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

JoinStaticMeasureElements = function(elements){
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

  x = StaticMeasure(OriginPeriod = OriginPeriod, Measure=Measures, Level=levelNames, .data=.data)
  x
}

#' @export
rbind.StaticMeasure = function(..., deparse.level=1){
  elements = list(...)
  blnSMs = sapply(elements, is.StaticMeasure)
  elements = elements[blnSMs]
  
  sm = JoinStaticMeasureElements(elements)
  sm
}

#' @export
setMethod("c", signature(x="StaticMeasure"), definition=function(x, ...){
  elements = list(...)
  blnOPs = sapply(elements, is.StaticMeasure)
  elements = elements[blnOPs]
  
  if (length(elements) == 0) {
    return (x)
  } else if (length(elements) == 1){
    sm = elements[[1]]
  } else {
    sm = JoinStaticMeasureElements(elements)
  }
  sm = rbind(x, sm)
  sm
})

# Addition of a group may be done via rbind and c()
# AddGroup = function(StaticMeasure, df){
#   
# }

# Addition of a measure may be done via cbind
# AddMeasure = function(StaticMeasure, df){
#   
# }

#************************************************************************************************************************
# 8. Persistence ====

#' @export
setMethod("write.csv", signature=c(x="StaticMeasure"), definition=function(x, file, row.names){
  df = as.data.frame(x)
  write.csv(df, ...)
})

#' @export
setMethod("write.excel", signature=c(object = "StaticMeasure", file="character", overwrite="logical")
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
setMethod("plot", signature(x="StaticMeasure", y="missing"), definition = function(x, facetFormula, ...){
            
  df = x@.data
            
  mdf = melt(df, id.vars=c("StartDate", "EndDate", "Moniker", LevelNames(x)), measure.vars=MeasureNames(x))
  
  if (missing(facetFormula)) facetFormula = as.formula(x)
  
  plt = ggplot(mdf, aes(x=Moniker, y = value, group=variable)) + geom_line() + facet_grid(facetFormula)
  plt
            
})
