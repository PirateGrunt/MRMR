#' @include Triangle.R
NULL

# library(RODBC)
# 
# channel = odbcConnect(dsn = "BAFSQL")
# 
# df = sqlQuery(channel, "SELECT * FROM qryCumulativeOccurrenceTotalByEvalDateWC")
# odbcClose(channel)
# 
# df$LossPeriodStartAnnual = NULL
# df$LossPeriodStartQuarter = NULL
# 
# df$LagAnnual = NULL
# df$LagQuarter = NULL
# 
# MeasureNames = colnames(df)[4:26]
# MeasureMeta = data.frame(MeasureNames = MeasureNames, Cumulative = rep(TRUE, 23))
# # MeasureMeta should also include gross/net/ceded, paid/reported/outstanding
# #   , claims, exposure, policy countetc.
# 
# tri = Triangle(TriangleData = df
#                , TriangleName = "WC"
#                , OriginPeriodColumn = "LossPeriodStartSemi"
#                , OriginPeriodType = "accident"
#                , OriginPeriodInterval = months(6)
#                , DevelopmentColumn = "LagAnnual"
#                , DevelopmentInterval = months(6)
#                , MeasureMeta)
# 
# chainLadder = TriangleModel("CumulPaid"
#                             , BaseTriangle = tri
#                             , ResponseName = "CumulativePaid"
#                             , PredictorName = "DirectEP"
#                             , CategoryName = "DevelopmentLag"
#                             , MinimumCategoryFrequency = 1
#                             , delta = 0)
# 
# summary(chainLadder@LinearFit)