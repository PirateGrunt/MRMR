PivotTriangleByFormula = function(df, Formula, Measure, Function = sum){
  z = dcast(df, theFormula, fun=Function, value.var=Measure, fill=NA_real_)
}

#' @title PivotTriangle
#' 
#' @export
#' 
#' @description
#' This function will alter triangle data from the "long" to the "wide format. This may be done
#' along any temporal dimension with any measure. (Static measures will produce trivial results.)
#' 
#' @param Triangle A valid triangle object
#' @param Row What dimension should appear in the rows?
#' @param Column What dimension should appear in the columns?
#' @param Measure What measure will be summarized?
#' @param Function What function will be used to summarize?
PivotTriangle = function(Triangle, Row, Column, Measure, Function=sum){
  df = as.data.frame(Triangle)
  
  myFormula = paste0(Row, " ~ ", Column)
  myFormula = as.formula(myFormula)
  z = PivotTriangleByFormula(df, myFormula, Measure, Function)
  z
}

x = PivotTriangle(Friedland, "OriginPeriodStart", "DevInteger", "CumulativePaid")
y = PivotTriangle(Friedland, "OriginPeriodStart", "DevInteger", "IncrementalPaid")
y = PivotTriangle(Friedland, "OriginPeriodStart", "EvaluationDate", "IncrementalPaid")
y = PivotTriangle(Friedland, "OriginPeriodStart", "EvaluationDate", "CumulativePaid")

View(x)
View(y)

# library(xtable)
# library(reshape2)
# temp = tempfile(fileext = ".html")
# print.xtable(xtable(z), type="html", file=temp)
# rstudio::viewer(temp)
# unlink(temp)
