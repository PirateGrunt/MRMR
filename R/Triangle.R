#' @include Utils.R
#' 
library(lubridate)

is.Triangle = function(object)
{
  is(object, "Triangle")
}

checkTriangle = function(object)
{
  errors = character()
#   numOPs = length(unique(object@OriginPeriod))
#   numDevs = length(unique(object@DevelopmentLag))
#   numGroups = length(unique(object@Group))
#   maxRows = numDevs * numOPs * numGroups
#   if ((nrow(object@TriangleData)) > maxRows){
#     paste0("The number of origin periods and lags suggests that you forgot to add a grouping field.")
#     paste0("Number of origin periods: ", numOPs)
#     paste0("Number of development lags: ", numDevs)
#     paste0("Number of groups: ", numGroups)
#     paste0("OP * Dev * Groups = ", maxRows)
#     paste0("Number of records in TriangleData: ", nrow(object@TriangleData))
#   }
  if (length(errors) == 0) TRUE else errors
}

#' Triangle class
#' 
#' Triangle is an S4 class used to store aggregated loss data.
#' 
#' @name Triangle-class
#' @rdname Triangle-class
#' @exportClass Triangle
#' 
setClass("Triangle"
         , representation(TriangleData = "data.frame"
                          , TriangleName = "character"
                          , OriginPeriodType = "character"
                          , StaticMeasures = "character"
                          , StochasticMeasures = "character"
                          , Groups = "character")
#          , sealed = TRUE
#         , validity = #some function
         )

#' Create a Triangle object.
#' @param TriangleData A dataframe 
#' @param TriangleName The name of the triangle
#' @param OriginPeriodType A character string which describes the type of origin period. 
#' @param OriginPeriodInterval
#' @export Triangle
# User-friendly constructor
newTriangle = function(OriginPeriods = NULL
                    , OriginEnd = NULL
                    , OriginLength = years(1)
                    , StartDay = 1
                    , StartMonth = 1
                    , DevelopmentLags = NULL
                    , DevelopmentPeriod = months(1)
                    , EvaluationDates = NULL
                    , OriginPeriodType = "Accident Year"
                    , TriangleName = NULL
                    , TriangleData = NULL
                    , StaticMeasures
                    , StochasticMeasures
                    , Groups = NULL
                    , Cumulative = TRUE)
{
  arguments <- as.list(match.call())
  
  OriginPeriods = eval(arguments$OriginPeriods, TriangleData)
  if(is.null(OriginPeriods)) stop ("No origin period was specified.")
  
  if (!is.interval(OriginPeriods)) {
    OriginPeriods = CreateOriginPeriods(OriginPeriods, OriginEnd, OriginLength, StartDay, StartMonth)
  }
  
  DevelopmentLags = eval(arguments$DevelopmentLags, TriangleData)
  if(is.null(DevelopmentLags)) stop ("No development lag information was provided.")
  
  if (!is.period(DevelopmentLags)){
    DevelopmentLags = CreateDevelopmentLags(DevelopmentLags, DevelopmentPeriod, EvaluationDates, OriginPeriods)
  }
  
  CommonDevInterval = DevelopmentLags[order(DevelopmentLags)]
  CommonDevInterval = CommonDevInterval[1]
  DevInteger = DevelopmentLags / CommonDevInterval

  if(is.null(EvaluationDates)) {
    EvaluationDates = CreateEvaluationDates(OriginPeriods, DevelopmentLags)
  } else {
    # Throw a warning if the evaluation dates which were passed in are not consistent with what they ought to be.
  }
  
  # It's possible that the user has fed data with overlap. 
  # Annual data with two start dates in the same year and annual development period, zB.
  # I might decide to check for this and throw a warning, but for now, I'll just blame the user.
  
  dfNewTriangleData = data.frame(OriginPeriod = OriginPeriods
                                 , DevelopmentLag = DevelopmentLags
                                 , EvaluationDate = EvaluationDates
                                 , DevInteger = DevInteger)
  
  dfNewTriangleData$OriginPeriodStart = int_start(dfNewTriangleData$OriginPeriod)
  dfNewTriangleData$OriginPeriodEnd = int_end(dfNewTriangleData$OriginPeriod)
  
  dfNewTriangleData$CalendarPeriodStart = dfNewTriangleData$EvaluationDate - CommonDevInterval + days(1)
  dfNewTriangleData$CalendarPeriodEnd = dfNewTriangleData$EvaluationDate 
  dfNewTriangleData$CalendarPeriod = with(dfNewTriangleData, new_interval(CalendarPeriodStart, CalendarPeriodEnd))
  
  if (is.null(Groups))
  {
    dfNewTriangleData$Group = "All"
    Groups = "Group"
  } else {
    dfNewTriangleData = cbind(dfNewTriangleData, TriangleData[Groups])
  }
  
  if (!is.null(StaticMeasures)) dfNewTriangleData = cbind(dfNewTriangleData, TriangleData[StaticMeasures])
  
  if (is.null(StochasticMeasures)) stop ("You've not supplied any stochastic measures for this triangle. Idiot.")

  dfStochasticMeasures = TriangleData[StochasticMeasures]
  stochasticMeasureNames = FormMeasureNames(dfStochasticMeasures, Cumulative)
  names(dfStochasticMeasures) = stochasticMeasureNames

  dfNewTriangleData = cbind(dfNewTriangleData, dfStochasticMeasures) 
  
  if(Cumulative) {
    dfNewTriangleData = CreateIncrementals(dfNewTriangleData, stochasticMeasureNames, Groups)
    print(colnames(dfNewTriangleData))
    stochasticMeasureNames = c(stochasticMeasureNames, gsub("Cumulative", "Incremental", stochasticMeasureNames))
  } else {
    dfNewTriangleData = CreateCumulative(dfNewTriangleData, stochasticMeasureNames, Groups)
    stochasticMeasureNames = c(stochasticMeasureNames, gsub("Incremental","Cumulative", stochasticMeasureNames))
  }
  
  dfNewTriangleData = CreatePriors(dfNewTriangleData, stochasticMeasureNames, Groups)
  
  if (is.null(TriangleName)) TriangleName = ""
  
  row.names(dfNewTriangleData) = NULL
  
  tri = new("Triangle"
            , TriangleData = dfNewTriangleData
            , TriangleName = TriangleName
            , OriginPeriodType = OriginPeriodType
            , StaticMeasures = CleanMeasureNames(stochasticMeasureNames)
            , StochasticMeasures = StaticMeasures
            , Groups = Groups)
  
  tri
}

# setMethod("c", signature(x = "Triangle"), function(x,  ...){
#   
#   elements = list(...)
# #  TriangleDatas = lapply(elements, slot, "TriangleData"))
#   TriangleNames = c(x@TriangleName, unlist(lapply(elements, slot, "TriangleName")))
#   OriginPeriodTypes = c(x@OriginPeriodType, unlist(lapply(elements, slot, "OriginPeriodType")))
#   Measures = c(x@Measures, unlist(lapply(elements, slot, "Measures")))
#   Groups = c(x@Groups, unlist(lapply(elements, slot, "Groups")))
#   
#   new("Triangle"
# #      , TriangleData = TriangleDatas
#       , TriangleName = TriangleNames
#       , OriginPeriodType = OriginPeriodTypes
#       , Measures = Measures
#       , Groups = Groups)
# })
# 
# # elements = list(tri1, tri2)
# # TriangleDatas = lapply(elements, slot, "TriangleData")
# # TriangleNames = unlist(lapply(elements, slot, "TriangleName"))
# # OriginPeriodTypes = unlist(lapply(elements, slot, "OriginPeriodType"))
# # Measures = unlist(lapply(elements, slot, "Measures"))
# # Groups = unlist(lapply(elements, slot, "Groups"))
# # 
# # mojo = as.data.frame(TriangleDatas[[1]])
# 
# mojo = new("Triangle"
# #    , TriangleData = as.data.frame(TriangleDatas)
#     , TriangleName = "tri1"
#     , OriginPeriodType = "AY"
#     , Measures = c("1", "2", "3")
#     , Groups = c("monkey", "zebra"))
# 
# mojo1 = mojo
# mojo2 = mojo
# 
# monkey = c(mojo1, mojo2)