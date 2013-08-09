#' Plot
#' 
#' @export PlotModelFactors
#' @param OriginPeriod Vector of intervals
#' @return A vector of intervals
#' @seealso \code{\link{CreateCumulative}}, \code{\link{CreatePriors}}
#' @examples
#' 
PlotModelFactors = function(objTriangleModel,  ...)
{
  require(ggplot2)
  require(reshape2)
  
  fit = objTriangleModel@Fit
  dfCoef = as.data.frame(summary(fit)$coefficients)
  colnames(dfCoef)
  
  x = GetX(objTriangleModel)
  
  dfY = apply(dfCoef, 1, function(y){
    ret = dnorm(x, mean = y[1], sd = y[2])
  })
  
  dfY = as.data.frame(dfY)
  colnames(dfY) = paste0("b", 1:ncol(dfY))
  mdf = suppressMessages(melt(dfY, value.name = "y"))
  mdf$x = rep(x, ncol(dfY))
  
  plt = ggplot(mdf, aes(x, y, col=variable)) + geom_line()
  
  plt
  
}

#setMethod("PlotModelFactors", c("TriangleModel"), definition = plotTriangleModel)
GetX = function(objTriangleModel)
{
  fit = objTriangleModel@Fit
  
  xlow = min(confint(fit))
  xhigh = max(confint(fit))
  
  x = seq(xlow, xhigh, length.out = 200)
  
  x
}

# cazart = c(EPModel, PaidCL)
# mojo = c(1,2)
# mojo = GetX()
# PlotModelFactors(PaidCL)
