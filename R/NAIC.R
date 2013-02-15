#====================================================================================
# Code to fetch a triangle from CAS research site.
#====================================================================================

GetNAICData = function (dataSetName = "ppauto_pos.csv")
{
  URL.stem = "http://www.casact.org/research/reserve_data/"
  URL = paste(URL.stem, dataSetName, sep="")
  
  df = read.csv(URL)
  colnames(df) = c("GroupCode"
                   , "GroupName"
                   , "LossPeriod"
                   , "DevelopmentYear"
                   , "DevelopmentLag"
                   , "CumulativeIncurred"
                   , "CumulativePaid"
                   , "IBNR"
                   , "DirectEP"
                   , "CededEP"
                   , "NetEP"
                   , "Single"
                   , "Reserve1997")
  
  require(lubridate)
  df$OriginPeriodStart = ymd("2000-01-01")
  year(df$OriginPeriodStart) = df$LossPeriod
  df$LossPeriod = NULL
  
  return(df)
}