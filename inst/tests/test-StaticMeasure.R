context("StaticMeasure")

# Sample data
op = OriginPeriod(StartDate = as.Date("2001-01-01"), Period=as.period(1, "years"), NumPeriods=10)
op$Moniker = paste0("AY ", as.character(year(op$StartDate)))
op$Type = "Accident Year"

EarnedPremium.CA = seq(from=10000, to=19000, by=1000)
IncurredLoss.CA = EarnedPremium.CA * 0.75

EarnedPremium.NY = EarnedPremium.CA * .65
IncurredLoss.NY = IncurredLoss.CA * .76

df.CA = data.frame(EarnedPremium = EarnedPremium.CA, IncurredLoss = IncurredLoss.CA)
df.NY = data.frame(EarnedPremium = EarnedPremium.NY, IncurredLoss = IncurredLoss.NY)

dfLevel.CA = data.frame(Line = "GL", ClassGroup = "PremOps", State = "CA", stringsAsFactors = FALSE)
dfLevel.NY = data.frame(Line = "GL", ClassGroup = "PremOps", State = "NY")
dfLevel.PR = data.frame(Line = "GL", ClassGroup = "PremOps", Territory = "PR")


test_that("Construction", {

  # This will produce garbage. Must think of a way to address this.
  x = new("StaticMeasure", OriginPeriod = op, Measure = names(df.CA), Level = dfLevel.CA)
  expect_true(is.StaticMeasure(x))
  
  x = StaticMeasure(OriginPeriod = op, Measure = names(df.NY), Level = dfLevel.NY)
  expect_true(is.StaticMeasure(x))
  
  x = StaticMeasure(op, names(df.NY), dfLevel.NY)
  expect_true(is.StaticMeasure(x))
  
  x = StaticMeasure(op, Measure=names(df.CA), rbind(dfLevel.CA, dfLevel.NY))
  expect_true(is.StaticMeasure(x))
  
  x = StaticMeasure(OriginPeriod=op, Level=c("GL", "PremOps", "TX"))
  LevelNames(x) = c("Line", "ClassGroup", "State")
  
  x = StaticMeasure(op, Level=c(Line = "GL", ClassGroup = "PremOps", State = "TX"))
  
  x = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio"), Level=c("GL", "PremOps", "TX"))
  x = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio"), Level=c(Line = "GL", ClassGroup = "PremOps", State = "TX"))
  MeasureNames(x) = c("EP", "IL", "LR")
  
  x = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio"), Level = dfLevel.CA)
  
  dfData = merge(dfLevel.CA, df.CA)
  
  x = StaticMeasure(op, .data = dfData
                    , Measure = c("EarnedPremium", "IncurredLoss", "LossRatio")
                    , Level=c("Line", "ClassGroup", "State"))
})

test_that("Accessors", {
  sm.CA = x = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio")
                            , Level=c(Line = "GL", ClassGroup = "PremOps", State = "CA"))

  x = sm.CA[1]
  expect_true(is.StaticMeasure(x))
  expect_true(length(x) == 1)
  
  x = sm.CA[1:3]
  expect_true(length(x) == 3)
  
  x = sm.CA[c(1, 5)]
  expect_true(length(x) == 2)
  
  x = sm.CA["AY 2004"]
  expect_true(is.StaticMeasure(x))
  expect_true(length(x) == 1)

  x = sm.CA[c("AY 2001", "AY 2004")]
  expect_true(is.StaticMeasure(x))
  expect_true(length(x) == 2)
  
  x = sm.CA[, "EarnedPremium"]
  expect_true(is.StaticMeasure(x))
  expect_true(length(x) == 10)
  
  x = sm.CA[, , "Line"]
  x = sm.CA[, , c("Line", "State")]
  
  sm.Multi = StaticMeasure(OriginPeriod = op, Measure = names(df.CA), Level = rbind(dfLevel.CA, dfLevel.NY))
  x = sm.Multi[1]
  
  x = sm.Multi[, , "NY"]
  expect_true(is.StaticMeasure(x))
  expect_true(length(x) == 10)
  
  x = sm.Multi[, , 2]
  expect_true(is.StaticMeasure(x))
  expect_true(length(x) == 10)
  
  x = sm.Multi[, , c("NY", "GL")]
  expect_true(is.StaticMeasure(x))
  expect_true(length(x) == 20)
 
  x = sm.Multi[, , c("NY", "CA")]
  expect_true(is.StaticMeasure(x))
  expect_true(length(x) == 20)
  
  x = sm.Multi[sm.Multi@OriginPeriod$StartDate <= as.Date("2004/1/1"), "IncurredLoss", "CA"]
  expect_true(is.StaticMeasure(x))
  expect_true(length(x) == 4)
  
  sm.Multi[, 1]
  sm.Multi[, 2]
  sm.Multi[, 1:2]
  sm.Multi[, c(1,2)]
  sm.Multi[1, c(1,2)]
  sm.Multi[1:2, c(1,2)]

  sm.Multi["AY 2004", , ]
  sm.Multi["AY 2004", , "CA"]
  sm.Multi["AY 2004", c(1,2)]

  sm.CA[1:5, 1, k=1]
  x = sm.CA[, "EarnedPremium", ]
  sm.CA[, 2, ]
  sm.CA["AY 2004", "EarnedPremium", ]
  sm.CA["AY 2004", "EarnedPremium", "CA"]
  
  sm.CA$LossRatio = sm.CA$IncurredLoss / sm.CA$EarnedPremium
  y = MeasureNames(sm.CA)
  expect_true(length(y) == 3)
  expect_true("EarnedPremium" %in% y)
  expect_true("IncurredLoss" %in% y)
  expect_true("LossRatio" %in% y)
  
  sm.CA$EarnedPremium[1] = 20000
  sm.CA$EarnedPremium = EarnedPremium.CA
  expect_true(sm.CA$EarnedPremium[1] == 20000)
  sm.CA$EarnedPremium = sm.CA$EarnedPremium * 1.37
  
  sm.CA$State[1] = "TX"
  expect_true(sm.CA$State[1] == "TX")
  
  sm.CA$State = "CA"
  
  sm.Multi$State
  sm.Multi$State[1] = "TX"
  sm.Multi$Line[1] = "Liability"
  
#  sm.CA[1, "EarnedPremium"] = 20000
#   sm.CA[sm.CA$]
#   
#   sm.CA[1, "State"] = "TX"
#   
#   sm.CA[]
})

# Comparison

# Properties

test_that("Conversion", {
  sm.CA = StaticMeasure(OriginPeriod = op, Measure = names(df.CA), Level = dfLevel.CA)
  df = as.data.frame(sm.CA)
  #df = as.data.frame(sm.Multi)
  
})

test_that("Concatenate", {
  sm.CA = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio")
                            , Level=c(Line = "GL", ClassGroup = "PremOps", State = "CA"))
  sm.NY = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio")
                            , Level=c(Line = "GL", ClassGroup = "PremOps", State = "NY"))
  sm.TX = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio")
                        , Level=c(Line = "GL", ClassGroup = "PremOps", State = "TX"))
  z = rbind(sm.CA, sm.NY)
  expect_true(length(z) == length(sm.CA))
  expect_true(length(z) == length(sm.NY))
  
  z = c(sm.CA, sm.NY)
  z = c(sm.CA, sm.NY, sm.TX)
  
})

test_that("Persistence", {
  sm.CA = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio")
                        , Level=c(Line = "GL", ClassGroup = "PremOps", State = "CA"))
  sm.NY = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio")
                        , Level=c(Line = "GL", ClassGroup = "PremOps", State = "NY"))
  sm.TX = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio")
                        , Level=c(Line = "GL", ClassGroup = "PremOps", State = "TX"))
  z = c(sm.CA, sm.NY, sm.TX)
  
  write.excel(z, "StaticMeasure.xlsx", overwrite=TRUE)
  
})
# sm.CA = new("StaticMeasure", OriginPeriod = op, Measure = df.CA, Level = dfLevel.CA)
# sm.NY = StaticMeasure(OriginPeriod = op, Measure = df.NY, Level = dfLevel.NY)
# sm.TX = StaticMeasure(op, df.TX, dfLevel.TX)
# sm.Multi = StaticMeasure(op, rbind(df.CA, df.NY, df.TX), rbind(dfLevel.CA, dfLevel.NY, dfLevel.TX))

# rm(EarnedPremium.CA, EarnedPremium.NY, EarnedPremium.TX)
# rm(IncurredLoss.CA, IncurredLoss.NY, IncurredLoss.TX)
# rm(dfLevel.CA, dfLevel.NY, dfLevel.PR, dfLevel.TX)
# rm(df.CA, df.NY, df.PR, df.TX)

# op = OriginPeriod(StartDate = as.Date("2002-01-01")
#                   , Period=as.period(6, "months"), EndDate=as.Date("2014-01-01"), Type="Accident Period")
# op$Moniker = paste0("H", ifelse(month(op$StartDate) == 1, "1", "2"), " ", year(op$StartDate))
