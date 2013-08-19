#' Plot
#' 
#' @export PlotModelGoF
#' @param OriginPeriod Vector of intervals
#' @return A vector of intervals
#' 
PlotModelGoF = function(objTriangleModel,  ...)
{
  require(ggplot2)
  require(reshape2)
  
  fit = objTriangleModel@Fit
  
  fstat = summary(fit)$fstatistic
  df1 = fstat[2]
  df2 = fstat[3]
  
  xlow = qf(0.025, df1, df2)
  xhigh = qf(0.975, df1, df2)
  x = seq(xlow, xhigh, length.out = 200)
   
  df = data.frame(x = x, y = df(x, df1, df2))
  plt = ggplot(df, aes(x, y)) + geom_line() + geom_vline(xintercept = fstat[1])
  plt
  
}