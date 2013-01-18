# Demo script
source("https://raw.github.com/PirateGrunt/MRMR/master/RegressionSupport.r")
source("https://raw.github.com/PirateGrunt/MRMR/master/NAIC.R")
source("https://raw.github.com/PirateGrunt/MRMR/master/ReservingVisualization.R")

df = GetNAICData("wkcomp_pos.csv")
bigCompany = as.character(df[which(df$CumulativePaid == max(df$CumulativePaid)),"GroupName"])

df.sub = subset(df, GroupName == bigCompany)

df.sub = subset(df.sub, DevelopmentYear <=1997)

library(ggplot2)
#ShowIncrementals (df.sub, bigCompany)
plt = ShowTriangle(df.sub, bigCompany)
plt = ShowTriangle(df.sub, bigCompany, Cumulative=FALSE)
#Note the apparent calendar year impact in 1996. This is invisible in the cumulative display.