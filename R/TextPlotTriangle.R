plot.TriangleNumbers = function(aMatrix)
{
  numRows = nrow(aMatrix)
  numCols = ncol(aMatrix)
  x = rep(1:numCols, numRows)
  x = x[order(x)]
  y = rep(numRows:1, numCols)
  
  plot(1:(numCols+1), 1:(numRows+1), type="n", xaxt = "n", yaxt="n", bty="n", xlab="", ylab="")
  text (x+1, y, labels = aTriangle)  
  text (1:numCols+1, numRows + 1, labels=colnames(aMatrix))
  text (1, numRows:1, labels=row.names(aMatrix))
  
}

# aTriangle = matrix(c("1,000", "1,100", "1,200", "1,800"
#                      , "1,100", "1,350", "1,500", ""
#                      , "1,200", "1,400", "", ""
#                      , "1,400", "", "", ""), nrow = 4, ncol = 4, byrow=TRUE)
# 
# colnames(aTriangle) = c("6 Months", "12 Months", "18 Months", "24 Months")
# row.names(aTriangle) = c("2000 H1", "2000 H2", "2001 H1", "2001 H2")
# 
# plot.TriangleNumbers(aTriangle)