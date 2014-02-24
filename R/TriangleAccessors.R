#=====================
# Triangle name
setGeneric("TriangleName")

setMethod("TriangleName", signature("Triangle"), function(x){
  slot(x, "TriangleName")
})

"TriangleName<-" <- function(x, value){
  x = value
}

setGeneric("TriangleName<-")

setMethod("TriangleName<-", signature("Triangle"), function(x, value){
  slot(x, "TriangleName") = value
  x
})

#=======================
# StaticMeasure
StaticMeasure = function(x, MeasureName){
  print(MeasureName)
}

setGeneric("StaticMeasure")

setMethod("StaticMeasure", signature("Triangle")
          , function(x, MeasureName){
            df = slot(x, "TriangleData")
            df = df[, c("OriginPeriodStart", "OriginPeriodEnd", MeasureName)]
            df = unique(df)
            row.names(df) = NULL
            df
          })

"StaticMeasure<-" = function(x, ...){
  x = 2
}

setGeneric("StaticMeasure<-")

setMethod("StaticMeasure<-", signature("Triangle"), function(x, value){
  # clean out temporal measures other than OriginPeriodStart
  # or in some other way ensure one and only one origin period measure
  
  OPNames = intersect(colnames(value), OriginPeriodNames())
  if (length(OPNames) > 0) {
    value = cbind(value[, OPNames[1]], value[, !colnames(value) %in% OPNames])
    colnames(value)[1] = OPNames[1]
  } else {
    warning("No origin periods in data frame. Will assume that they are in OP order")
  }
  
  # Check consistency with group columns. It's ok if we don't have them.
  # We may want to assign generic measures which don't vary by group
  
  # Check against existing static measures. We are either adding
  # or replacing measures. If we're replacing, we need to remove the 
  # columns from the merged data set.
  
  # We could remove from the original TriangleData data frame, but
  # if the merge doesn't work for some reason, we can fail 
  # gracefully without a loss of data
  df = slot(x, "TriangleData")
  df2 = merge(df, value, by = OPNames[1], all.x = TRUE)
  
  df2 = df2[, !grepl(".x", colnames(df2))]
  
  colnames(df2) = gsub(".y", "", colnames(df2))
  slot(x, "TriangleData") = df2
  x
})

#====================
# StochasticMeasure
StochasticMeasure = function(x, MeasureName){
  print(MeasureName)
}

setGeneric("StochasticMeasure")

setMethod("StochasticMeasure", signature("Triangle")
          , function(x, MeasureName){
            df = slot(x, "TriangleData")
            df = df[, c("OriginPeriodStart", "OriginPeriodEnd", "DevInteger", "DevelopmentLag", MeasureName)]
            df = unique(df)
            row.names(df) = NULL
            df
          })

"StochasticMeasure<-" = function(x, ...){
  x = 2
}

setGeneric("StochasticMeasure<-")

setMethod("StochasticMeasure<-", signature("Triangle"), function(x, cumulative, value){
  OPNames = intersect(colnames(value), OriginPeriodNames())
  if (length(OPNames) > 0) {
    value = cbind(value[, OPNames[1]], value[, !colnames(value) %in% OPNames])
    colnames(value)[1] = OPNames[1]
  } else {
    warning("No origin periods in data frame. Will assume that they are in OP order")
  }
  
  DevNames = intersect(colnames(value), DevelopmentPeriodNames())
  if (length(DevNames) > 0) {
    value = cbind(value[, DevNames[1]], value[, !colnames(value) %in% DevNames])
    colnames(value)[1] = DevNames[1]
  }
  
  if (cumulative) {
    whichCols = colnames(value) %in% c(OPNames[1], DevNames[1])
    value[, !whichCols] = -value[, !whichCols]
  }
  
  df = slot(x, "TriangleData")
  df2 = merge(df, value, by = c(OPNames[1], DevNames[1]), all.x = TRUE)
  
  df2 = df2[, !grepl(".x", colnames(df2))]
  
  colnames(df2) = gsub(".y", "", colnames(df2))
  slot(x, "TriangleData") = df2
  x
})