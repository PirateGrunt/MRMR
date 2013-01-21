
setClass("TriangleProjection", 
         representation(ProjectionData = "dataframe"
                        , ProjectionName = "character"
                        , BaseTriangle = "Triangle"))

setValidity("TriangleProjection"
            , function(object){
              if (is.Triangle(object@BaseTriangle)){
                return (TRUE)
              }else{
                return ("Triangle projection must be based on a valid triangle.")
              }
              }
            })



# summary will have ability to write TEX output.
# setMethod("summary", "TriangleProjection",
#           function(object, ...) {
#             print(paste("Loss period type = ", object@LossPeriodType))
#             print(paste("Loss period interval = ", object@LossPeriodInterval))
#           })
# 
# setMethod("plot", "TriangleProjection",
#           function(x, y, ...) {
#             
#           })