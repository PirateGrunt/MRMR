#' 

SimulateLosses = function(AccidentYear, FrequencyMean, FrequencyVariance, ReportLag, PaymentLag, SeverityMean, SeverityVar)
{
  numClaims = rnorm(1, mean = FrequencyMean, sd = sqrt(FrequencyVariance))
  
  ReportLag = rexp(numClaims, rate = 1/ReportLag)
  PaymentLag = rexp(numClaims, rate = 1/ PaymentLag)
  UntrendedClaim = rlnorm(numClaims, meanlog = log(SeverityMean), sdlog = log(SeverityVar^0.5))
  UntrendedClaim = ifelse(UntrendedClaim >1000000,1000000,UntrendedClaim)
  LossMonth = runif(numClaims, min=0, max=11)
  
  dfAY = data.frame(LossMonth = LossMonth
                    , ReportLag = ReportLag
                    , PaymentLag = PaymentLag)
  
  dfAY$LossMonth = dfAY$LossMonth + 12 * AccidentYear 
  dfAY$ReportMonth = dfAY$LossMonth + dfAY$ReportLag
  dfAY$PaymentMonth = with(dfAY, 12 * AccidentYear + LossMonth + ReportLag + PaymentLag)
  
  dfAY$AccidentYear = AccidentYear
  dfAY$ReportYear = ceiling(dfAY$ReportMonth / 12) - 1
  dfAY$PaymentYear = ceiling(dfAY$PaymentMonth / 12) - 1
  
  dfAY$AnnualPaymentLag = with(dfAY, PaymentYear - AccidentYear) + 1
  dfAY$AnnualReportLag = with(dfAY, ReportYear - AccidentYear) + 1
  
  dfAY$UntrendedClaim = UntrendedClaim
  
  return (dfAY)
}

InflateLosses = function(dfLosses, Inflation, alpha = 0)
{
  InflationFactor = with(dfLosses, (Inflation[PaymentMonth] / Inflation[AccidentYear * 12])^alpha
                         * (Inflation[LossMonth] / Inflation[AccidentYear * 12]) ^(1-alpha))
  dfLosses$TrendedClaim = dfLosses$UntrendedClaim * InflationFactor
}

set.seed(12341234)
numTrials = 1000

FrequencyMean = 40
FrequencyVariance = 60
ReportLag = 18
PaymentLag = 12
SeverityMean = 10400
SeverityVar = 34800^2
aDF = SimulateLosses(0, FrequencyMean, FrequencyVariance, ReportLag, PaymentLag, SeverityMean, SeverityVar)

AccidentYears = 0:5

SimLosses = lapply(AccidentYears, SimulateLosses, FrequencyMean, FrequencyVariance, ReportLag, PaymentLag, SeverityMean, SeverityVar)

dfSimLosses = do.call("rbind", SimLosses)

# hist(dfSimLosses$UntrendedClaim)
# hist(dfSimLosses$ReportLag)
# hist(dfSimLosses$LossMonth)

AggregateLosses = function(dfByClaim)
{
  require(doBy)
  
  myFun = function(x) c(mean=mean(x), count=length(x), sum=sum(x))
  
  dfAgg = summaryBy(UntrendedClaim ~ AccidentYear + ReportYear + AnnualReportLag, data=dfByClaim, FUN=myFun)
  
  return (dfAgg)
}

dfAgg = AggregateLosses(dfSimLosses)

TriangleLong2Short = function(dfTriangle, OriginPeriod, Lag, Value)
{
  require(reshape2)
  
  dfMelt = melt(dfTriangle, id=c(OriginPeriod, Lag))
  
  dfMelt = dfMelt[dfMelt$variable == Value,]
  
  dfTri = dcast(dfMelt, dfMelt[,OriginPeriod] ~ dfMelt[,Lag], sum)
  colnames(dfTri)[1] = OriginPeriod
  
  return(dfTri)
  
}

TriangleShort2Long = function(dfTriangle){}

dfTop = dfAgg[dfAgg$ReportYear <= 4,]
dfShort = TriangleLong2Short(dfTop, "AccidentYear", "AnnualReportLag", "UntrendedClaim.sum")
dfShort = TriangleLong2Short(dfTop, "AccidentYear", "AnnualReportLag", "UntrendedClaim.count")
dfShort = incr2cum(dfShort)

