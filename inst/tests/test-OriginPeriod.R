context("OriginPeriod")

startDates = seq(as.Date("2001/01/01"), as.Date("2010/12/31"), by="1 year")
endDates = startDates + as.period(1, "year") - days(1)
period = as.period(1, "year")
moniker =  paste0("AY ", as.character(year(startDates)))
type = "Accident"

# Dummy data
GenericTestOP = function(){
  op = OriginPeriod(startDates, endDates, period, moniker, type)
  op
}

test_that("Construction", {
  x = OriginPeriod(startDates)
  
  expect_true(is.OriginPeriod(x))
  
  x = OriginPeriod(startDates, endDates)
  expect_true(is.OriginPeriod(x))
  
  x = OriginPeriod(startDates, Period=period)
  expect_true(is.OriginPeriod(x))
  
#   x = OriginPeriod(startDates, period)
#   expect_true(is.OriginPeriod(x))
  
  x = OriginPeriod(startDates, endDates, Moniker=moniker)
  expect_true(is.OriginPeriod(x))
  
  x = OriginPeriod(startDates, endDates, Type=type)
  expect_true(is.OriginPeriod(x))
  
  x = OriginPeriod(startDates, endDates, Moniker=moniker, Type=type)
  expect_true(is.OriginPeriod(x))
  
  x = OriginPeriod(startDates, Period=period, Moniker=moniker, Type=type)
  expect_true(is.OriginPeriod(x))
  
  x = OriginPeriod(startDates, Type="Accident")
  expect_true(is.OriginPeriod(x))
  
  # This won't work! R assumes that the unnamed argument is meant to be Period, so
  # it will dispatch the default method
  expect_error(OriginPeriod(startDates, endDates, moniker, type))
  
  # integers
  accidentYears = seq(2001:2010)
  x = OriginPeriod(accidentYears, StartMonth = 7, StartDay = 1, Moniker=moniker)
  expect_true(is.OriginPeriod(x))
  
  expect_true(month(x$StartDate[1]) == 7)
  
  x = OriginPeriod(accidentYears, Type=type, Moniker=moniker)
  expect_true(is.OriginPeriod(x))
  expect_true(month(x$StartDate[1]) == 1)
  
  x = OriginPeriod(accidentYears)
  expect_true(is.OriginPeriod(x))
  
  # semi-annual
  startDates = seq(as.Date("2001/01/01"), as.Date("2005/12/31"), by="6 months")
  endDates = startDates + as.period(6, "months") - days(1)
  x = OriginPeriod(startDates, endDates)
  expect_true(is.OriginPeriod(x))
  
  expect_true(length(x) == 10)
  
  op = OriginPeriod(StartDate = as.Date("2001-01-01"), EndDate = as.Date("2010-07-01")
                    , Period=as.period(6, "months"))
  expect_true(is.OriginPeriod(op))
  
  op = OriginPeriod(StartDate = as.Date("2001-01-01"), Period=as.period(6, "months")
                    , NumPeriods=20)
  expect_true(is.OriginPeriod(op))
})

test_that("Accessors", {
  x = OriginPeriod(2001:2010)
  
  y = x[1]
  expect_true(is.OriginPeriod(y))
  expect_true(length(y) == 1)

  y = x[2:3]
  expect_true(is.OriginPeriod(y))
  expect_true(length(y) == 2)
  
  y = x["2004-01-01"]
  expect_true(is.OriginPeriod(y))
  expect_true(length(y) == 1)
  
  y = x[c("2004-01-01", "2005-01-01")]
  expect_true(is.OriginPeriod(y))
  expect_true(length(y) == 2)
  
  y = x[c(1, 8)]
  expect_true(is.OriginPeriod(y))
  expect_true(length(y) == 2)
  
  y = x$StartDate
  expect_true(is.Date(y))
  
  y = x$Type
  expect_true(is.character(y))
  
  y = x$Moniker[3]
  expect_true(is.character(y))
  
})

test_that("Assignment", {
  x = OriginPeriod(seq(2001:2010))
  
  x$Type = "Report"
  expect_true(x$Type == "Report")
  
  x$Moniker[3] ="blah"
  expect_true(x$Moniker[3] == "blah")

  expect_error(x$Moniker[5:6] <- "blah")
  
  x$Moniker[5:6] = c("AY 2005", "AY 2006")
  
  expect_error(x$Moniker[] <- "blah")
  expect_error(x$Moniker <- seq(2001:2010))
  
  x$Moniker[] = as.character(seq(1, length(x)))
  expect_true(x$Moniker[1] == 1)
})

test_that("Comparison", {
  
  x = OriginPeriod(seq(2001, 2005))
  y = OriginPeriod(seq(2002, 2006))
  expect_true(x != y)
  expect_true(x == x)
})

test_that("Conversion", {
  x = OriginPeriod(seq(2001, 2005))
  z = as.data.frame(x)
  expect_true(is.data.frame(z))
})

test_that("Concatenate", {
  x = OriginPeriod(startDates)
  y = OriginPeriod(max(startDates) + as.period(1, "year"))
  
  z = rbind(x, y)
  expect_true(length(z) == length(x) + length(y))
  
  z = c(x, y)
  expect_true(length(z) == length(x) + length(y))
  
  expect_error(z <- rbind(x, x))
  
  x = Grow(x, Length=2)
  
  expect_true(length(x@Moniker) == 12)
  expect_true(x@Moniker[12] == "New moniker 2")
})
 
test_that("Persistence", {
  startDates = seq(as.Date("2001/01/01"), as.Date("2010/12/31"), by="1 year")
  endDates = startDates + as.period(1, "year") - days(1)
  period = as.period(1, "year")
  moniker =  paste0("AY ", as.character(year(startDates)))
  type = "Accident"
  
  op = OriginPeriod(startDates, Period=as.period(1, "years"), Moniker=moniker, Type=type)
  
  write.excel(op, "OriginPeriod.xlsx", overwrite=TRUE)
  
})

#=============================================
# rep, subset, arithmetic ?
