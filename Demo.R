# Demo script
source("https://raw.github.com/PirateGrunt/MRMR/master/RegressionSupport.r")
source("https://raw.github.com/PirateGrunt/MRMR/master/NAIC.r")
source("https://raw.github.com/PirateGrunt/MRMR/master/ReservingVisualization.r")

df = GetNAICData("wkcomp_pos.csv")
bigCompany = as.character(df[which(df$CumulativePaid == max(df$CumulativePaid)),"GroupName"])

df.sub = subset(df, GroupName == bigCompany)

df.sub = subset(df.sub, DevelopmentYear <=1997)

ShowIncrementals (df.sub, bigCompany)
