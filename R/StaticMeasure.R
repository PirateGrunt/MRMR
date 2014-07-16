#' StaticMeasure class
#' 
#' @include NewGenerics.R
#' @include OriginPeriod.R
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
#' The number of levels of a StaticMeasure will determine its length. A StaticMeasure may be thought of as containing a list, wherein each
#' element of the list holds a data frame. The OriginPeriod and Measure properties ensure that each data frame will contain comparable 
#' information.
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

StaticKernelDataFrame = function(op, levels){
  op = as.data.frame(op)
  op = op[, c("StartDate", "EndDate", "Moniker")]
  df = merge(op, expand.grid(levels, stringsAsFactors=FALSE))
  df = OrderStaticMeasureData(df, "Moniker", levels)
}

OrderStaticMeasureData = function(df, opSort, levels){
  df = df[ do.call(order, df[, c(names(levels), opSort)]), ]
  row.names(df) = NULL
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

SingleAccessStaticMeasure = function(x, name, UniqueAttribute=FALSE){
  
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
  # First check to see if this corresponds to the name of a column
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
      sm = StaticMeasure(OriginPeriod = x@OriginPeriod, Level = lst, Measure = x@Measure, Data = df)
      return(sm)
    }
  }
  
  # 5. Are we looking for an OriginPeriod?
  if (name %in% x@OriginPeriod@Moniker){
    df = x@Data[x@Data$Moniker == name, ]
    row.names(df) = NULL
    op = x@OriginPeriod[name]
    sm = StaticMeasure(OriginPeriod = op, Level = x@Level, Measure = x@Measure, Data = df)
    return(sm)
  }
  
  stop("Name does not correspond to any slot, level, level attribute, data column or OriginPeriod moniker.")
  
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
                          , Data = "data.frame")
         , validity = checkStaticMeasure
)
#************************************************************************************************************************
# 2. Construction ====

setGeneric("StaticMeasure", function(OriginPeriod, Measure, Level, Data, ...) {
  standardGeneric("StaticMeasure")
})

#' @export 
setMethod("StaticMeasure", signature=c(OriginPeriod="OriginPeriod", Measure="ANY", Level="ANY", Data="ANY")
          , definition=function(OriginPeriod, Measure, Level, Data, OriginPeriodSort) {
            
            # TODO: Must handle the case when there are duplicate records. Only permit
            # one Measure observation for each OriginPeriod and Level combination.
            
            if (missing(Level)) {
              stop("Cannot create a static measure without knowing the levels")
            }
            
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
            
            if (missing(Measure)) {
              Measure = character()
            } else if(class(Measure) != "character"){
              Measure = as.character(Measure)
            }
            
            if (length(intersect(Measure, names(Level)) != 0)){
              warning("You're using the same name(s) for both measures and levels.")
            }
            
            dfKernel = StaticKernelDataFrame(OriginPeriod, Level)
            
            if (length(Measure) == 0){
              Data = dfKernel
            } else {
              if (missing(Data)) {
                Data = cbind(dfKernel, EmptyMeasureColumns(nrow(dfKernel), Measure))
              } else {
                missingMeasure = setdiff(Measure, colnames(dfKernel))
                missingMeasure = intersect(missingMeasure, Measure)
                if (length(missingMeasure) != 0) {
                  missingMeasure = EmptyMeasureColumns(nrow(dfKernel), missingMeasure)
                  Data = cbind(Data, missingMeasure)
                }
                if (!missing(OriginPeriodSort)){
                  Data = Data[, c(names(Level), Measure, OriginPeriodSort), drop=FALSE]
                  Data = OrderStaticMeasureData(Data, OriginPeriodSort, Level)
                } else {
                  Data = Data[, Measure, drop=FALSE]
                }
                Data = cbind(dfKernel, Data[, Measure, drop=FALSE])
              }
            }
            
            row.names(Data) = NULL
            sm = new("StaticMeasure"
                     , OriginPeriod = OriginPeriod
                     , Measure = Measure
                     , Level = Level
                     , Data = Data)
            sm
})

#************************************************************************************************************************
# 3. Properties ====
#' @export
setMethod("length", signature(x="StaticMeasure"), definition=function(x){
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
setMethod("MeasureNames", signature(x="StaticMeasure"), definition=function(x){
  x@Measure
})

#' @export
setMethod("MeasureNames<-", signature(x="StaticMeasure", value="character"), definition=function(x, value){
  oldNames = x@Measure
  whichCols = colnames(x@Data) %in% oldNames
  colnames(x@Data)[whichCols] = value
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
  whichCols = colnames(x@Data) %in% oldNames
  colnames(x@Data)[whichCols] = value
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

# The dollar sign will return a single element. The object returned depends on the character used with this method. MRMR will search
# through the object data as follows: it will first try to match against slot names. It will then try to match against MeasureNames. 
# If a match is found, that vector will be returned. It will then search through Level data. If the character is the name of a level, 
# the vector returned will be the attributes of that level. If the character is an attribute, then it will return a St*Measure object 
# with data for that single attribute. Finally, MRMR will attempt to match against the moniker for the OriginPeriod.
# 
# Example:
# sm$MyMeasure 
# Returns a vector of numeric data.
# 
# sm$State
# Returns a vector of the states which have been defined.
# 
# sm$CA
# Returns a St*Measure object for State == 'CA'.
# 
# sm$"AY 2004"
# Returns a St*Measure object for OriginPeriod == "AY 2004".
#' @export
setMethod("$", signature(x = "StaticMeasure"), function(x, name) {

  SingleAccessStaticMeasure(x, name, FALSE)
  
})

# [[
# Double brackets will return a single element. When a character is passed, the behavior is identical to the `$` operator.
# When passing an integer index, MRMR will return a St*Measure object which corresponds to the first element in the Level list.

#' @export
setMethod("[[", signature(x = "StaticMeasure"), function(x, i, UniqueAttribute=TRUE) {
  
  if (class(i) == "numeric"){
      i = x@Level[[i]]  
  } 
  
  SingleAccessStaticMeasure(x, i, UniqueAttribute)
  
})

# [
# Single brackets will return a set of St*Measure objects. 
# 
# At present, all Levels will be returned. There is not yet a mechanism to consolidate Levels.
#' @export 
setMethod("[", signature(x="StaticMeasure"), definition=function(x, i, j, OriginPeriod, drop=TRUE){

  df = x@Data
  op = x@OriginPeriod
  
  if(missing(i)) {
    i = rep(TRUE, nrow(df))
  }
  
  if (missing(j)) {
    j = x@Measure
  } else {
    whichCols = intersect(colnames(df), j)
    if (length(whichCols) == 0){
      stop ("Improper Measure specification. No columns returned.")
    }
    whichCols = c(whichCols, LevelNames(x))
    whichCols = c("StartDate", "EndDate", "Moniker", whichCols)
    df = df[, whichCols]
  }
  
  if (!missing(OriginPeriod)){
    op = x@OriginPeriod[OriginPeriod]
    i = i & (df$Moniker %in% op$Moniker)
  }
  
  df = df[i, drop]
  
  if (class(df) == "data.frame"){
    sm = StaticMeasure(op, Level=LevelNames(x), Measure=j, Data=df)  
    return (sm)
  } else {
    return (df)
  }
  
})

#' @export
setMethod("[<-", signature(x = "StaticMeasure"), definition=function(x, i, j, OriginPeriod, ..., value) {

  df = x@Data
  
  if(missing(i)) {
    i = rep(TRUE, nrow(df))
  }
  
  op = x@OriginPeriod
  if (!missing(OriginPeriod)){
    i = i & (df$Moniker %in% op["AY 2004"]$Moniker)
  }
  
  if (missing(j)) {
    j = c(MeasureNames(x), LevelNames(x))
  } else {
    whichCols = intersect(colnames(df), j)
    if (length(whichCols) == 0){
      stop ("Improper column specification. Please ensure that the columns exist.")
    }
  }

  df[i, whichCols] = value
  
  sm = StaticMeasure(op, Level=LevelNames(x), Measure=MeasureNames(x), Data=df)  
  
})

# The dollar sign will assign to a single element. The object assigned depends on the character used with this method. MRMR will search
# through the object data as follows: it will first try to match against slot names. It will then try to match against MeasureNames. 
# If a match is found, that vector will be returned. It will then search through Level data. If the character is the name of a level, 
# the vector returned will be the attributes of that level. If the character is an attribute, then it will return a St*Measure object 
# with data for that single attribute. Finally, MRMR will attempt to match against the moniker for the OriginPeriod.
# 
# Example:
# sm$MyMeasure = myVector
# 
# sm$State = myVector
# 
# sm$CA = myStMeasure
# 
# sm$"AY 2004" = myStMeasure

#' @export
setMethod("$<-", signature(x = "StaticMeasure"), function(x, name, value) {
  
  if (length(name) > 1) stop("Cannot assign more than one value at a time. Use the `[` operator for multiple assignment.")
  
  # 1. Check to see if we're assigning a slot
  if (name %in% slotNames(x)){
    slot(x, name) = value
    
    return(x)
  } 
  
  # 2. Next check to see if we're assigning a Measure
  if (name %in% MeasureNames(x)){
    x@Data[name] = value
    
    return(x)
  }  
  
  # 3. Check if we're assigning a Level
  if (name %in% LevelNames(x)) {
#     if (length(value) != length(x@Level[[name]])) {
#       stop ("Cannot assign Level of different length.")
#     }
    x@Data[name] = value
    # We call the constructor so that the data frame will repopulate with the proper Level attributes.
    x = StaticMeasure(OriginPeriod = x@OriginPeriod, Level = LevelNames(x), Measure = MeasureNames(x), Data = x@Data)
    
    return (x)
  }

#   # 4. Check to assign based on Level element
#   lst = x@Level
#   for (i in 1:length(lst)){
#     level = lst[[i]]
#     if (name %in% level){
#       sm = x[Level != name]
#       sm = c(sm, value)
# 
#       return(sm)
# 
#     }
#   }
#   
#   # 5. Check to assign based on OriginPeriod
#   if (name %in% x@OriginPeriod@Moniker){
#     sm = x[OriginPeriod != name]
#     sm = c(sm, value)
#     
#     return(sm)
#     
#   }
  
  # 6. This isn't a slot, it isn't a Level, it isn't a Level attribute and it isn't an OriginPeriod.
  # Must be a new Measure
  if (length(value) == nrow(x@Data)) {
    x@Data = cbind(x@Data, value)
    colnames(x@Data)[length(colnames(x@Data))] = name  
    x@Measure = c(name, x@Measure)
    
    return(x)
    
  }
  
  # If we reach this point, something has gone wrong
  stop("Unexpected assignment failure. Contact package author.")
  return (NULL)
  
})

#************************************************************************************************************************
# 5. Comparison ====

#************************************************************************************************************************
# 6. Conversion ====

#' @export
setMethod("as.data.frame", signature("StaticMeasure"), function(x, ...){
  x@Data
})

#' @export
setMethod("as.formula", signature(object="StaticMeasure"), definition=function(object){
  if (length(LevelNames) < 2){
    warning("Cannot create a formula for a StaticMeasure with only one Level.")
    return(NULL)
  }
  facetFormula = LevelNames(object)
  firstHalf = ceiling(length(facetFormula) / 2)
  secondHalf = (firstHalf+1):length(facetFormula)
  firstHalf = 1:firstHalf
  
  LHS = paste(LevelNames(object)[firstHalf], collapse="+")
  RHS = paste(LevelNames(object)[secondHalf], collapse="+")
  facetFormula = as.formula(paste(LHS, "~", RHS))
  
  facetFormula
})

#' @export
melt.StaticMeasure = function(data){
  df = data@Data
  mdf = melt(df, id.vars=c("StartDate", "EndDate", "Moniker", LevelNames(data))
             , measure.vars=MeasureNames(data)
             , variable.name="Measure")
  mdf
}
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
  Data = lapply(elements, slot, "Data")
  Data = do.call(rbind, Data)  

  x = StaticMeasure(OriginPeriod = OriginPeriod, Measure=Measures, Level=levelNames, Data=Data)
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

# setMethod("Grow", signature=c(object="StaticMeasure", Length="numeric")
#           , definition=function(object, Length, newdata, OriginPeriodSort){
#             
#             op = Grow(object@OriginPeriod, Length)
#             
#             if (missing(OriginPeriodSort)) OriginPeriodSort = "StartDate"
#             colnames(df)[colnames(df) == OriginPeriodSort] = "StartDate"
#             
#             if (missing(newdata)) {
#               
#             } else {
#               newdata = newdata[, colnames(newdata) %in% colnames(sm@Data)]
#             }
#             
#             newdata = rbind(sm@Data, newdata)
#             
#             sm = StaticMeasure(OriginPeriod = op
#                                , Measure = sm@Measure
#                                , Level = sm@Level
#                                , Data = newdata)
# })

#************************************************************************************************************************
# 8. Persistence ====

# setMethod("write.csv", signature=c(x="StaticMeasure"), definition=function(x, file, row.names){
#   df = as.data.frame(x)
#   write.csv(df, ...)
# })

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
#' @title StaticMeasure plot
#' @name plot.StaticMeasure
#' @description
#' StaticMeasure plot
#' 
#' @details
#' 
#' A StaticMeasure will always use the OriginPeriod for the x axis.
#' 
#' The GroupParameter controls which column will be used to group the results. Everything else will be used to facet 
#' the plots.
#' 
#' @export
setMethod("plot", signature(x="StaticMeasure", y="missing")
          , definition = function(x, Measure, GroupParameter="Measure", FacetFormula){
  
  mdf = melt(x)
  
  if (missing(Measure)) Measure = MeasureNames(x)
  if (missing(GroupParameter)) GroupParameter = "Measure"
  
  if (sum(mdf$Measure %in% Measure) == 0) stop("Improper Measure specification. Please specify Measures for this object.")
  
  mdf = mdf[mdf$Measure %in% Measure, ]
  
  plt = ggplot(mdf, aes_string(x="Moniker", y="value", group=GroupParameter, color=GroupParameter)) + geom_line()
  
  if (missing(FacetFormula)){
    facetCols = colnames(mdf)[!colnames(mdf) %in% c("StartDate", "EndDate", "Moniker", GroupParameter, "value")]
    
    FacetFormula = paste(facetCols, collapse="+")
    FacetFormula = as.formula(paste("~", FacetFormula))
    
    facetRow = length(unique(mdf[, facetCols]))
    FacetRow = floor(sqrt(facetRow)) + 1
    
    plt = plt + facet_wrap(FacetFormula, facetRow, scales="free")  
  } else {
    plt = plt + facet_grid(FacetFormula, scales="free")
  }
  
  plt
            
})
