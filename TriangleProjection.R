setClass("TriangleProjection", 
         representation(data = "dataframe"
                        , name = "character"
                        , 
                        , LossPeriodInterval = "Interval"))

# summary will have ability to write TEX output.
setMethod("summary", "TriangleProjection",
          function(object, ...) {
            print(paste("Loss period type = ", object@LossPeriodType))
            print(paste("Loss period interval = ", object@LossPeriodInterval))
          })

setMethod("plot", "TriangleProjection",
          function(x, y, ...) {
            
          })