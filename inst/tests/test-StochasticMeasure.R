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
context("StochasticMeasure")

test_that("Construction", {
  op = OriginPeriod(StartDate = as.Date("2001-01-01"), Period=as.period(1, "year"), NumPeriods=10)
  
  scm = StochasticMeasure(OriginPeriod = op
                        , Level=list(Line = "GL", Subline = c("PremOps", "Products"))
                        , Measure = c("PaidLoss", "IncurredLoss")
                        , DevPeriod = as.period(1, "year")
                        , Lags = 1:10
                        , LastEvaluationDate = as.Date("2010-12-31"))
  
  df = LongToWide(scm)
})

# Properties

test_that("Accessors", {

  
})

test_that("Assignment", {

  
})

# Comparison

test_that("Conversion", {
  df = as.data.frame(sm)
  expect_true(class(df) == "data.frame")
})

test_that("Concatenate", {
})

test_that("Persistence", {
  
})
