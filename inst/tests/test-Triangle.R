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
context("Triangle")

test_that("Construction", {
  op = OriginPeriod(StartDate = as.Date("2001-01-01"), Period=as.period(1, "year"), NumPeriods=10)
  
  df = LongToWide(scm)
})

# Properties

test_that("Accessors", {
  
})

test_that("Assignment", {

})

# Comparison

test_that("Conversion", {
  
})

test_that("Concatenate", {
  
})

test_that("Persistence", {
  
})
