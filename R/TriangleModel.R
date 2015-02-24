#'
#' TriangleModel class
#' 
#' @import nlme
#' @import lubridate
#' 
#' @include NewGenerics.R
#' @include OriginPeriod.R
#' @include StaticMeasure.R
#' @include StochasticMeasure.R
#' 
#' @docType class
#' 
#' @name TriangleModel-class
#' @rdname TriangleModel-class
#' @exportClass TriangleModel
#' 
NULL

#**********************************************************************************************
# 0. Helper functions ====

GetFilter = function(df, Level){
    lstFilter = list()
    lstFilter[[1]] = unique(df$AdjustedLag)
    names(lstFilter)[[1]] = "AdjustedLag"
    j = 2
    for (i in 1:(length(Level))){
      colName = names(Level)[[i]]
      colValue = df[, colName, drop=TRUE]
      colValue = unique(colValue)
      if (length(colValue) == 1 & !is.null(colValue)){
        lstFilter[[j]] = colValue
        names(lstFilter)[[j]] = colName
        j = j + 1
      }
    }
    
    lstFilter
}

IndividualData = function(df, objTriangle){
  lstDF = split(df, f = df[, c("AdjustedLag", LevelNames(objTriangle))], drop=FALSE)
  lstDF
}

GroupedData = function(df){
  lstDF = split(df, df$AdjustedLag)
  lstDF
}

FitElement = function(df, theFormula, Alpha){
  
  if (class(theFormula) == "list"){
    formulaExpression = paste0("lme(fixed = ", theFormula$FixedFormula
                               , ", random = ", theFormula$RandomFormula
                               , ", data = df")
    if(Alpha != 0){
      formulaExpression = paste0(formulaExpression, ", weights = ~ Weight")
      Fit = lme(fixed = theFormula$FixedFormula, random = theFormula$RandomFormula, data = df, weights = ~ Weight)
    }
    formulaExpression = paste0(formulaExpression, ")")
  } else {
    formulaExpression = paste0("lm(", theFormula, ", data = df, weights = Weight)")
  }
  
  Fit = eval(parse(text=formulaExpression))
  Fit
  
}

PredictElement = function(Fit){
  Predicted = predict(Fit)
  if (class(Fit) == "lm"){
    Residual = residuals(Fit)
    Rstandard = rstandard(Fit)
  } else {
    Residual = residuals(Fit, type="response")
    Rstandard = residuals(Fit, type="pearson")
  }
  
  df = data.frame(Predicted, Residual, Rstandard)
}

lmFormula = function(Predictor, Response, Intercept=FALSE){
  
  strFormula = paste(Predictor, collapse="+")
  
  if (Intercept){
    strFormula = paste0("1 + ", strFormula)
  } else {
    strFormula = paste0("0 + ", strFormula)
  }
  
  strFormula = paste0(Response, " ~ ", strFormula)
  
}

lmeFormulae = function(Predictor, Response, Group, Intercept=FALSE){
  FixedFormula = lmFormula(Predictor, Response, Intercept)
  
  RandomFormulae = list()
  for (i in 1:(length(Group))) {
    iFormula = lmFormula(Predictor, "", Intercept)
    iFormula = paste(iFormula, collapse="")
    iFormula = paste(iFormula, "|", Group[i])
    RandomFormulae[[i]] = iFormula
  }
  
  lst = list(FixedFormula = FixedFormula, RandomFormula = RandomFormulae)
}

#************************************************************************************************************************
# 1. Class Definition ====
#' @export
is.TriangleModel = function(object)
{
  is(object, "TriangleModel")
}

checkTriangleModel = function(object)
{
  errors = character()
  if (length(errors) == 0) TRUE else errors
}

setClass("TriangleModel"
         , representation(ModelData="data.frame"
                          , Response="character"
                          , Predictor="character"
                          , Group = "character"
                          , ModelType = "character"
                          , Alpha = "numeric"
                          , Tail = "numeric"
                          , Fit = "list"
                          , FitFilter = "list"
                          , Triangle = "Triangle")
         , validity = checkTriangleModel
)

#************************************************************************************************************************
# 2. Construction ====

setGeneric("TriangleModel", function(Triangle,  ...) {
  standardGeneric("TriangleModel")
})

#' @export
setMethod("TriangleModel"
          , signature=c(Triangle="Triangle")
          , definition=function(Triangle, Response, Predictor, Group, ModelType
                                , Intercept, Alpha, Tail) {
            
            # 1. Check the inputs
            if (!Response %in% StochasticMeasureNames(Triangle, Stem=FALSE)){
              warning ("Use of a static response is a silly thing to do. These results may not make much sense.")
            }
            
            if (!Predictor %in% MeasureNames(Triangle, Stem=FALSE)){
              stop ("Predictor variable not found in Triangle data.")
            }
            
            if (!Response %in% MeasureNames(Triangle, Stem=FALSE)){
              stop ("Response variable not found in Triangle data.")
            }
            
            if (!ModelType %in% c("individual", "pooled", "blended")) ModelType = "individual"
            
            if (missing(Alpha)) Alpha = 0
            
            if (missing(Intercept)) Intercept = FALSE
            
            if (missing(Group)) {
              Group = "None"
              if (ModelType == "blended"){
                warning("Group parameter must be specified for blended models. Switching to individual model.")
                ModelType = "individual"
              }
            } else {
              if (ModelType != "blended") {
                warning("Group parameter is ignored for individual and pooled models.")
              }
            }
            
            # Missing tail will be dealt with below when we fetch the data frame
            
            # 2. Create the formula
            if (ModelType == "blended"){
              theFormula = lmeFormulae(Predictor, Response, Group)
            } else {
              theFormula = lmFormula(Predictor, Response)
            }
            
            # 3. Fetch and adjust the data
            df = as.data.frame(Triangle)
            df = df[!is.na(df[Predictor]), ]
            df$AdjustedLag = df$Lag
            
            # Default is to have a Tail one less than the maximum lag. This will generally give us 3 observations
            # in the penultimate and ultimate lag.
            if (missing(Tail)) {
              uniqueLags = unique(df$Lag)
              Tail = max(uniqueLags)
              if (length(uniqueLags) > 2) Tail = Tail - 1
            }
              
            df$AdjustedLag[df$Lag >= Tail] = Tail
            df$Weight = 1 / df[Predictor] ^ (Alpha/2)
            
            # 4. Repeat the fit by element
            if (ModelType == "individual"){
              lstDF = IndividualData(df, Triangle)
            } else {
              lstDF = GroupedData(df)
            }
            lstFit = list()
            lstFilter = list()
            for (i in seq_along(lstDF)){
              lstFit[[i]] = FitElement(lstDF[[i]], theFormula, Alpha)
              lstDF[[i]] = cbind(lstDF[[i]], PredictElement(lstFit[[i]]))
              lstFilter[[i]] = GetFilter(lstDF[[i]], Triangle@StochasticMeasure@Level)
            }
            df = do.call(rbind, lstDF)
            
            TriangleModel = new("TriangleModel"
                                , ModelData = df
                                , Response = Response
                                , Predictor = Predictor
                                , Group = Group
                                , ModelType = ModelType
                                , Alpha = Alpha
                                , Tail = Tail
                                , Fit = lstFit
                                , FitFilter = lstFilter
                                , Triangle = Triangle)
            
            TriangleModel
            
})
#************************************************************************************************************************
# 3. Properties ====

#' @export
RMSE = function(object){
  RMSE = sqrt(MSE(object))
  RMSE
}

#' @export
MSE = function(object){
  df = object@ModelData
  MSE = (df[object@Response] - df$Predicted) ^2
  MSE = mean(MSE)
  MSE
}

#' @export
setMethod("LevelNames", signature(x="TriangleModel"), definition=function(x){
  LevelNames(x@Triangle)
})

#************************************************************************************************************************
# 6. Conversion ====
#' @export
setMethod("as.data.frame", signature("TriangleModel"), function(x, ...){
  df = x@ModelData
  whichCols = !(colnames(df) %in% MeasureNames(x@Triangle, Stem=FALSE))
  whichCols = c(colnames(df)[whichCols], x@Predictor, x@Response)
  whichCols = unique(whichCols)
  df = df[, whichCols]
  row.names(df) = NULL
  df
})

#' @export
melt.TriangleModel = function(data){
  df = as.data.frame(data)
  # melt will often generate a warning. It appears to be no cause for concern. This should be researched and confirmed.
  suppressWarnings(mdf <- melt(df
                              , id.vars=c("StartDate", "EndDate", "Moniker", "EvaluationDate", "Lag", LevelNames(data))
                              , variable.name="Measure"))
  mdf
}

#' @export 
setMethod("LongToWide", signature("TriangleModel"), function(object, TimeAxis="Lag"){
  mdf = melt(object)
  theFormula = paste(LevelNames(object), collapse="+")
  theFormula = paste(theFormula, "Measure", "StartDate", "Moniker", sep="+")
  theFormula = paste(theFormula, "~", TimeAxis)
  df = dcast(mdf, as.formula(theFormula), sum)
  df
})
#************************************************************************************************************************
# 8. Persistence ====

#' @export
setMethod("write.excel", signature=c(object = "TriangleModel", file="character", overwrite="logical")
          , definition=function(object, file, overwrite=FALSE, TimeAxis="Lag"){
            
            if (file.exists(file) & !overwrite){
              stop("Excel file already exists. Either enter a new filename or set the overwrite parameter to TRUE.")
            }
            
            #df = as.data.frame(object)
            df = LongToWide(object, TimeAxis)
            
            lst = split(df, df$Measure)
            
            wbk = loadWorkbook(file, create=TRUE)
            
            for (i in seq_along(lst)){
              #writeSheet(wbk, sheetNames[i], lstGroups[[i]])
              df = lst[[i]]
              shtName = as.character(df$Measure[1])
              createSheet(wbk, name=shtName)
              writeWorksheet(wbk, df, shtName, startRow = 1, header=TRUE)
            }
            
            saveWorkbook(wbk)
            
})

#************************************************************************************************************************
# 9. Display ====
# setMethod("plot", signature(x="TriangleModel", y="missing"), definition = function(x, ...){
#    
# })

# PlotModelFactors = function(objTriangleModel)
# {
#   
#   fit = objTriangleModel@Fit
#   dfCoef = as.data.frame(summary(fit)$coefficients)
#   colnames(dfCoef)
#   
#   x = GetX(objTriangleModel)
#   
#   dfY = apply(dfCoef, 1, function(y){
#     ret = dnorm(x, mean = y[1], sd = y[2])
#   })
#   
#   dfY = as.data.frame(dfY)
#   colnames(dfY) = paste0("b", 1:ncol(dfY))
#   mdf = suppressMessages(melt(dfY, value.name = "y"))
#   mdf$x = rep(x, ncol(dfY))
#   
#   y = NULL
#   variable = NULL
#   plt = ggplot(mdf, aes(x, y, col=variable)) + geom_line()
#   
#   plt
#   
# }
# 
# GetX = function(objTriangleModel)
# {
#   fit = objTriangleModel@Fit
#   
#   xlow = min(confint(fit))
#   xhigh = max(confint(fit))
#   
#   x = seq(xlow, xhigh, length.out = 200)
#   
#   x
# }
# 
# PlotModelGoF = function(objTriangleModel)
# {
#   fit = objTriangleModel@Fit
#   
#   fstat = summary(fit)$fstatistic
#   df1 = fstat[2]
#   df2 = fstat[3]
#   
#   xlow = qf(0.025, df1, df2)
#   xhigh = qf(0.975, df1, df2)
#   x = seq(xlow, xhigh, length.out = 200)
#   
#   df = data.frame(x = x, y = df(x, df1, df2))
#   y = NULL
#   plt = ggplot(df, aes(x, y)) + geom_line() + geom_vline(xintercept = fstat[1])
#   plt
#   
# }
# 

#' @export
PlotResiduals = function(object){
  
  predictor = object@Predictor[1]
  df = object@ModelData
  
  op = par(mfrow=c(2,2), oma=c(0,0,2,0))
#   op = par(mfrow=c(2,2))
  
  RstandardOK <- !is.nan(df$Rstandard) & !is.infinite(df$Rstandard) & !is.na(df$Rstandard)
  ylow = min(df$Rstandard[RstandardOK], -3)
  ymax = max(df$Rstandard[RstandardOK], 3)
  
  plot(x = df$Predicted, y = df$Rstandard, col="blue", pch=16, xlab="Fitted values", ylab="Std. Resid.", xaxt="n", ylim=c(ylow, ymax))
  xTicks = pretty(df$Predicted, 8)
  axis(side=1, at=xTicks, labels=format(xTicks, scientific=FALSE, big.mark=","))
  abline(0, 0)
  abline(3, 0, lty = "dashed")
  abline(-3, 0, lty = "dashed")
  
  plot(x=df$Lag, y=df$Rstandard, col="blue", pch=16, xlab="Development Lag", ylab="Std. Resid.", ylim=c(ylow, ymax))
  abline(0,0)
  abline(3, 0, lty = "dashed")
  abline(-3, 0, lty = "dashed")
  factors = factor(df$Lag)
  means = tapply(df$Rstandard, factors, mean)
  lines(levels(factors), means, col="red", type="l")
  
  plot(x = df$StartDate, y=df$Rstandard, col="blue", pch=16, xlab="Origin Period", ylab="Std. Resid.", ylim=c(ylow, ymax))
  abline(0,0)
  abline(3, 0, lty = "dashed")
  abline(-3, 0, lty = "dashed")
  factors = factor(df$Moniker)
  means = tapply(df$Rstandard, factors, mean)
  lines(levels(factors), means, col="red", type="l")
  
  plot(x=year(df$EvaluationDate), y=df$Rstandard, col="blue", pch=16, xlab="Calendar Period", ylab="Std. Resid.", ylim=c(ylow, ymax))
  abline(0,0)
  abline(3, 0, lty = "dashed")
  abline(-3, 0, lty = "dashed")
  factors = factor(year(df$EvaluationDate))
  means = tapply(df$Rstandard, factors, mean)
  lines(levels(factors), means, col="red", type="l")
  
  par(op)
  
}
