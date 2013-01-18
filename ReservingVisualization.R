#==================================================================================================
# RegressionSupport.r
#   ShowIncrementals
#   ShowAggDiagnostics (SampleData, Name)
#   ShowReserveDiagnostics (Fit, Category1, Category2, Category3, Title)
#   WriteModelDiagnosticsToFile (Fit, DataSetName, ResponseName, PredictorName, delta, DesignWidth, Category, OriginYear, CalendarYear)
#==================================================================================================

ShowTriangle = function(df, EntityName, Cumulative = TRUE, Paid = TRUE, Show = TRUE)
{
  require(ggplot2)
  df$LossPeriod = as.factor(df$LossPeriod)
  
  PaidOrIncurred = ifelse(Paid, "Paid", "Incurred")
  CumulativeOrIncremental = ifelse(Cumulative, "Cumulative", "Incremental")
  LossValue = paste0(CumulativeOrIncremental, PaidOrIncurred)
  
  df$LossValue = df[,LossValue]
  
  PlotTitle = paste(EntityName, tolower(CumulativeOrIncremental), tolower(PaidOrIncurred), "loss by accident year")
  
  plt = ggplot(df, aes(x = DevelopmentLag, y = LossValue, group = LossPeriod, colour = LossPeriod)) 
  plt = plt + geom_line(show_guide=FALSE) + geom_point(show_guide=FALSE) + labs(title=PlotTitle)
  
  if (Show) print(plt)
  
  return (plt)
}

#==================================================================================================
# function ShowAggDiagnostics
#==================================================================================================
ShowAggDiagnostics = function(df, ResponseName, PredictorName, CategoryName)
{
  categories = levels(factor(df[,CategoryName]))
  numCategory = length(categories)
  palette = rainbow(numCategory)
  
  colors = palette[df[,CategoryName]]
  plot(x=df[,PredictorName], y=df[,ResponseName], col=colors, pch=19, xlab=PredictorName, ylab=ResponseName, xaxt="n", yaxt="n")
  xTicks = pretty(df[,PredictorName])
  yTicks = pretty(df[,ResponseName])
  axis(side=1, at=xTicks, labels=format(xTicks, scientific=FALSE, big.mark=","))
  axis(side=2, at=yTicks, labels=format(yTicks, scientific=FALSE, big.mark=","))
  for (iCategory in 1:numCategory)
  {
    dfLine = subset(df, df[,CategoryName]==categories[iCategory])
    colors = palette[dfLine[,CategoryName]]
    plot(x=dfLine[,PredictorName], y=dfLine[,ResponseName], col=colors, pch=19, xlab=PredictorName, ylab=ResponseName, xaxt="n", yaxt="n")
    xTicks = pretty(dfLine[,PredictorName])
    yTicks = pretty(dfLine[,ResponseName])
    axis(side=1, at=xTicks, labels=format(xTicks, scientific=FALSE, big.mark=","))
    axis(side=2, at=yTicks, labels=format(yTicks, scientific=FALSE, big.mark=","))
    
    y = dfLine[,ResponseName]
    x = dfLine[,PredictorName]
    fit = lm(y ~ x +0, data=dfLine)
    curve(coef(fit)[1]*x, add=TRUE, col=palette[iCategory])
  }
  
  return (0)
}
#= end ShowAggDiagnostics==========================================================================

#==================================================================================================
# function ShowReserveDiagnostics
#==================================================================================================
ShowReserveDiagnostics = function(Fit, Category1, Category2, Category3, Title)
{
  Stand.Resid = rstandard(Fit)
  FittedValues = fitted(Fit)
  
  op = par(mfrow=c(2,2), oma=c(0,0,2,0))
  
  plot(x=FittedValues, y=Stand.Resid, col="blue", pch=16, ylim=c(-3,3), main="Fitted Values", xlab="", ylab="", xaxt="n", yaxt="n")
  xTicks = pretty(FittedValues, 8)
  yTicks = pretty(Stand.Resid, 8)
  
  axis(side=1, at=xTicks, labels=format(xTicks, scientific=FALSE, big.mark=","))
  axis(side=2, at=yTicks, labels=format(yTicks, scientific=FALSE, big.mark=","))
  abline(0,0)
  
  plot(Category1, Stand.Resid, col="blue", pch=16, ylim=c(-3,3), main="Payment Lag", xlab="", ylab="")
  abline(0,0)
  factors = factor(Category1)
  means = tapply(Stand.Resid, factors, mean)
  lines(levels(factors), means, col="red", type="l")
  
  plot(Category2, Stand.Resid, col="blue", pch=16, ylim=c(-3,3), main="Origin Period", xlab="", ylab="")
  abline(0,0)
  factors = factor(Category2)
  means = tapply(Stand.Resid, factors, mean)
  lines(levels(factors), means, col="red", type="l")
  
  plot(Category3, Stand.Resid, col="blue", pch=16, ylim=c(-3,3), main="Calendar Period", xlab="", ylab="")
  abline(0,0)
  factors = factor(Category3)
  means = tapply(Stand.Resid, factors, mean)
  lines(levels(factors), means, col="red", type="l")
  
  mtext(Title, side=3, outer=TRUE, line=0, adj=0)
  
  #   par(mfrow=c(1,1))
  par(op)
  return (0)
}
#= end ShowReserveDiagnostics =====================================================================

#==================================================================================================
# function WriteModelDiagnosticsToFile
#==================================================================================================
WriteModelDiagnosticsToFile = function(Fit, DataSetName, ResponseName, PredictorName, delta, DesignWidth, Category, OriginPeriod, CalendarYear)
{
  filename = paste(DataSetName, ResponseName, PredictorName, delta, DesignWidth, ".pdf", sep="")
  filename = paste(filepath, filename, sep="")
  Title = paste("Data: ", DataSetName, " Response: ", ResponseName, " Predictor: ", PredictorName, " delta = ", delta, sep="")
  
  pdf(width=11, height=8.5, onefile=TRUE, file=filename)
  
  ShowReserveDiagnostics(Fit, Category, OriginPeriod, CalendarPeriod, Title)
  
  dev.off()
}
#= end WriteModelDiagnosticsToFile ================================================================
