# #' @export
# setMethod("show", "Triangle"
#           , function(object){
#             cat("This is a loss triangle\n")
#             cat("Its name is", object@TriangleName, "\n")
#             cat("Its columns are", colnames(object@TriangleData), "\n")
#             print(head(object@TriangleData))
#           })
# 
# #' @export
# setMethod("summary", "Triangle",
#           function(object, ...) {
#             print(paste("Loss period type = ", object@OriginPeriodType))
#             print(paste("Loss period interval = ", object@OriginPeriodInterval))
#           })
# 
# 
# .plotTriangle = function(tri)
# {
#   require(ggplot2)
#   df = tri@TriangleData
#   df$OriginPeriod = as.factor(df$OriginPeriod)
#   
#   #PaidOrIncurred = ifelse(Paid, "Paid", "Incurred")
#   PaidOrIncurred = "Paid"
#   #CumulativeOrIncremental = ifelse(Cumulative, "Cumulative", "Incremental")
#   CumulativeOrIncremental = "Cumulative"
#   LossValue = paste0(CumulativeOrIncremental, PaidOrIncurred)
#   
#   df$LossValue = df[,LossValue]
#   
#   PlotTitle = paste(tri@TriangleName, tolower(CumulativeOrIncremental), tolower(PaidOrIncurred), "loss by accident year")
#   
#   plt = ggplot(df, aes(x = DevelopmentLag, y = LossValue, group = OriginPeriod, colour = OriginPeriod)) 
#   plt = plt + geom_line(show_guide=FALSE) + geom_point(show_guide=FALSE) + labs(title=PlotTitle)
#   
#   print(plt)
#   
#   return (plt)
# }
# 
# setMethod("plot", "Triangle"
#           , function(x, y, ...){
#             .plotTriangle(x)
#           })
# 
# 
# setValidity("Triangle"
#             , function(object){
#               # the dataframe must have the following named columns:
#               # OriginPeriodStart
#               # OriginPeriodEnd
#               # EvaluationDate
#               # DevelopmentInterval
#               # one of the following: Incremental or cumulative PaidLoss, ReportedLoss, CaseReserve, ReportedClaims, ClosedClaims
#               ValidPeriodTypes = c("accident", "policy", "report")
#               if (is.na(match(tolower(object@OriginPeriodType), ValidPeriodTypes))){
#                 return ("Invalid loss period type")
#               }else{
#                 return (TRUE)
#               }
#             })
# 
# is.Triangle = function(object)
# {
#   is(object, "Triangle")
# }
# 
# 
# setGeneric("setName<-", function(object, value) {
#   standardGeneric("setName<-")
# })
# 
# setReplaceMethod(
#   f = "setName"
#   , signature = "Triangle"
#   , definition = function(object, value){
#     object@TriangleName <- value
#     return(object)
#   })
# 
# # setGeneric("ProjectTriangle", function(x, ...){
# #   standardGeneric("ProjectTriangle")
# # })
# # 
# # setMethod("ProjectTriangle", "Triangle", function(x, FutureDate){
# #   df = LatestDiagonal(Triangle)
# #   return(df)
# # })
# # CreateProjection(dfTriangle, NumberOfPeriods, Response, IntervalWidth)
# # {
# #   
# #   return (dfProjection)
# # }
# 
# # setMethod(
# #   f = "Initialize"
# #   , signature = "Triangle"
# #   , definition = function(.Object, TriangleData
# #                           , TriangleName
# #                           , OriginPeriodType
# #                           , OriginPeriodInterval
# #                           , DevelopmentInterval){
# #     cat("Initialize triangle")
# #     .Object@TriangleData = TriangleData
# #     .Object@OriginPeriodType = OriginPeriodType
# #     .ObjectOriginPeriodInterval = OriginPeriodInterval
# #   }
# #   )