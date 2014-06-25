#' StaticMeasure
#'     +---Level
#'         +----Measure
#'         +----OriginPeriod
#'         
#' StochasticMeasure
#'     +---Level
#'         +----Measure
#'         +----OriginPeriod
#'         +----EvaluationDate
#'         
context("StaticMeasure")

# Sample data
op = OriginPeriod(StartDate = as.Date("2001-01-01"), Period=as.period(1, "years"), NumPeriods=10)
op$Moniker = paste0("AY ", as.character(year(op$StartDate)))
op$Type = "Accident Year"

sm = StaticMeasure(OriginPeriod = op
                   , Level=list(Line = "GL", Subline = c("PremOps", "Products"), State = c("CA", "TX"))
                   , Measure = c("EarnedPremium", "IncurredLoss")
                   , Data = data.frame(EarnedPremium=seq(from=10000, to=19000, by=1000)
                                       , IncurredLoss = 0.75*seq(from=10000, to=19000, by=1000)))

EarnedPremium.CA = seq(from=10000, to=19000, by=1000)
IncurredLoss.CA = EarnedPremium.CA * 0.75

EarnedPremium.NY = EarnedPremium.CA * .65
IncurredLoss.NY = IncurredLoss.CA * .76

df.CA = data.frame(EarnedPremium = EarnedPremium.CA, IncurredLoss = IncurredLoss.CA)
df.NY = data.frame(EarnedPremium = EarnedPremium.NY, IncurredLoss = IncurredLoss.NY)

dfLevel.CA = data.frame(Line = "GL", ClassGroup = "PremOps", State = "CA", stringsAsFactors = FALSE)
dfLevel.NY = data.frame(Line = "GL", ClassGroup = "PremOps", State = "NY")
dfLevel.PR = data.frame(Line = "GL", ClassGroup = "PremOps", Territory = "PR")

GenericStaticMeasure = function(){
  op = OriginPeriod(StartDate = as.Date("2001-01-01"), Period=as.period(1, "years"), NumPeriods=10)
  op$Moniker = paste0("AY ", as.character(year(op$StartDate)))
  op$Type = "Accident Year"
  
  dfLevel.CA = data.frame(Line = "GL", ClassGroup = "PremOps", State = "CA", stringsAsFactors = FALSE)
  
  sm = StaticMeasure(OriginPeriod = op, Measure = c("EarnedPremium", "IncurredLoss"), Level = dfLevel.CA)
  sm$EarnedPremium = seq(from=10000, to=19000, by=1000)
  sm$IncurredLoss = sm$EarnedPremium * 0.75
  sm
}

test_that("Construction", {

  # This will produce garbage. Must think of a way to address this.
  # Why does this produce garbage? Because we're not actually calling one of our constructors.
#   x = new("StaticMeasure", OriginPeriod = op, Measure = names(df.CA), Level = dfLevel.CA)
#   expect_true(is.StaticMeasure(x))
  
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
  
  x = StaticMeasure(op, Data = dfData
                    , Measure = c("EarnedPremium", "IncurredLoss", "LossRatio")
                    , Level=c("Line", "ClassGroup", "State"))
})

# Properties

test_that("Accessors", {

  sm = StaticMeasure(OriginPeriod = op
                     , Level=list(Line = "GL", Subline = c("PremOps", "Products"), State = c("CA", "TX"))
                     , Measure = c("EarnedPremium", "IncurredLoss")
                     , Data = data.frame(EarnedPremium=seq(from=10000, to=19000, by=1000)
                                         , IncurredLoss = 0.75*seq(from=10000, to=19000, by=1000)))
  
  # Test $ accessors
  x = sm$EarnedPremium
  expect_true(length(x) == 40)
  
  x = sm$State
  expect_true(length(x) == nrow(sm@Data))
  
  x = sm$Line
  expect_true(length(x) == nrow(sm@Data))
  
  x = sm$CA
  expect_true(is.StaticMeasure(x))
  expect_true(length(setdiff(LevelNames(x), c("Line", "Subline", "State"))) == 0)
  expect_true(unique(x$State) == "CA")
  
  x = sm$"AY 2004"
  expect_true(is.StaticMeasure(x))
  
  x = sm$EarnedPremium[sm$State == "CA"]
  # This is a TODO
  # State(sm)
  
  # Test [[. This is more or less the same. However, when we use integer indexing, we will likely return an error, except in the unlikely event that
  # the user has supplied a vector, which return a single Level attribute.
  x = sm[["State", , FALSE]]
  expect_true(length(x) == nrow(sm@Data))
  x = sm[["State", UniqueAttribute=TRUE]]
  expect_true(length(setdiff(x, c("CA", "TX"))) == 0)
  x = sm[["State", UniqueAttribute=FALSE]]
  
  x = sm[["CA"]]
  expect_true(is.StaticMeasure(x))
  expect_true(length(setdiff(LevelNames(x), c("Line", "Subline", "State"))) == 0)
  expect_true(unique(x$State) == "CA")
  
#   x = sm[[c(3,2)]]
#   expect_true(is.StaticMeasure(x))
#   expect_true(length(x) == 1)
#   
  x = sm[[1]]
  expect_true(length(x) != 1)
  
  # Test [
  x = sm[OriginPeriod = "AY 2004"]
  expect_true(is.StaticMeasure(x))
  
  x = sm[OriginPeriod="AY 2004", sm$State=="CA"]
  expect_true(is.StaticMeasure(x))
  x = sm[sm$State=="CA", OriginPeriod="AY 2004"]
  
  x = sm[sm$State == "CA", OriginPeriod = "AY 2004"]
  expect_true(is.StaticMeasure(x))
  
  x = sm[sm$State == "CA", OriginPeriod = sm$OriginPeriod$Moniker[4]]
  
  # This won't work. The 4th element of the OriginPeriod object is an OriginPeriod object
  expect_error(x <- sm[sm$State == "CA", OriginPeriod = sm$OriginPeriod[4]])
  
  x = sm[sm$State == "CA", OriginPeriod = c("AY 2004", "AY 2005")]
  
  x = sm[sm$State == "CA", "EarnedPremium", OriginPeriod = "AY 2004"]
  
  x = sm[sm$State == "CA", "EarnedPremium"]
  
  # This will basically produce BS
  x = sm[1]
  
})

test_that("Assignment", {

  # Highly experimental. This will be used when I figure out how and if to create dynamic functions.
  # State(sm)[1] = "TX"
  
  # Test $ assignment
  sm$State[sm$State == "CA"] = "NY"
  
  sm$State[sm$State == "NY"] = "CA"
  
  # This should produce an error
  sm$State = "WV"
  
  sm$State[1:20] = "NY"
  
  LevelNames(sm)[LevelNames(sm) == "State"] = "Territory"
  sm$Territory[1:20] = "BC"
  
  LevelNames(sm)[LevelNames(sm) == "Territory"] = "State"
  sm$State[1:20] = "NY"
  
  sm$EarnedPremium[sm$State == "NY"] = sm$EarnedPremium[sm$State == "NY"] * 1.05
  
  #sm$OriginPeriod = #something
    
  sm$LossRatio = sm$IncurredLoss / sm$EarnedPremium
  y = MeasureNames(sm)
  expect_true(length(y) == 3)
  expect_true("EarnedPremium" %in% y)
  expect_true("IncurredLoss" %in% y)
  expect_true("LossRatio" %in% y)
  
  sm$EarnedPremium[1] = 20000
  expect_true(sm$EarnedPremium[1] == 20000)

  sm[, "EarnedPremium"] = 4
  sm[, "EarnedPremium", OriginPeriod = "AY 2004"] = 400
  
  sm[sm$State=="CA" & sm$Subline=="PremOps", c("IncurredLoss", "EarnedPremium"), OriginPeriod = "AY 2004"] = c(4, 6)
  sm[sm$State=="CA" & sm$Subline=="PremOps"
     , c("IncurredLoss", "EarnedPremium")] = c(seq(1000, by=1000, length.out=10), seq(2000, by=500, length.out=10))
  
  LevelNames(sm) = c("Bereich", "Abteiling", "Bundesstaat")
  
})

# Comparison

test_that("Conversion", {
  df = as.data.frame(sm)
  expect_true(class(df) == "data.frame")
})

test_that("Concatenate", {
  sm.CA = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio")
                            , Level=c(Line = "GL", ClassGroup = "PremOps", State = "CA"))
  sm.NY = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio")
                            , Level=c(Line = "GL", ClassGroup = "PremOps", State = "NY"))
  sm.TX = StaticMeasure(op, Measure = c("EarnedPremium", "IncurredLoss", "LossRatio")
                        , Level=c(Line = "GL", ClassGroup = "PremOps", State = "TX"))

  z = rbind(sm.CA, sm.NY)
  expect_true(length(z) == 2)
  
  z = c(sm.CA, sm.NY)
  expect_true(length(z) == 2)
  
  z = c(sm.CA, sm.NY, sm.TX)
  expect_true(length(z) == 3)
  
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
