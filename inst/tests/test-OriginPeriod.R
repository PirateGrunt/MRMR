context("OriginPeriod")

dummyOriginPeriod = function{
  startDates = seq(as.Date("2002/01/01"), as.Date("2013/12/31"), by="1 year")
  endDates = startDates + as.period(1, "year") - days(1)
  length = as.period(1, "year")
  moniker = paste0("AY ", as.character(year(startDates)))
  type = "Accident"
}

test_that("Multiple stochastic measures", {
  myTriangle = newTriangle(TriangleData = TestDataFrame()
                           , OriginPeriods = AccidentYear
                           , DevelopmentLags = Month
                           , Cumulative = FALSE
                           , StochasticMeasures = c("Paid", "Reported")
                           , StaticMeasures = c("EP")
                           , Verbose = FALSE)
  
  expect_true("IncrementalReported" %in% colnames(df))
})


x = OriginPeriod(startDates)
x = OriginPeriod(startDates, endDates)
x = OriginPeriod(startDates, Length=length)
x = OriginPeriod(startDates, length)
x = OriginPeriod(startDates, endDates, Moniker=moniker)
x = OriginPeriod(startDates, endDates, Type=type)
x = OriginPeriod(startDates, endDates, Moniker=moniker, Type=type)
x = OriginPeriod(startDates, Length=length, Moniker=moniker, Type=type)
# This won't work! R assumes that the unnamed argument is meant to be length, so
# it will dispatch the default method
x = OriginPeriod(startDates, endDates, moniker, type)

startDates.y = seq(as.Date("2014/01/01"), as.Date("2015/12/31"), by="1 year")
endDates.y = startDates.y + as.period(1, "year") - days(1)
moniker.y = paste0("AY ", as.character(year(startDates.y)))
type = "Accident"
y = OriginPeriod(startDates.y, Length=length, Moniker=moniker.y, Type="Accident")

z = rbind(x, y)
z = c(x, y)

accidentYears = seq(2002:2013)
x = OriginPeriod(accidentYears, Type=type, Moniker=moniker)
x = OriginPeriod(accidentYears, StartMonth = 7, StartDay = 1)

x = OriginPeriod(startDates, Type="Accident")

startDates = seq(as.Date("2002/01/01"), as.Date("2013/12/31"), by="6 months")
endDates = startDates + as.period(6, "months") - days(1)
moniker = paste0("H", ifelse(month(startDates) == 1, "1", "2"), as.character(year(startDates)))

x = OriginPeriod(startDates, endDates, Moniker=moniker)
