# Demo script
source("https://raw.github.com/PirateGrunt/MRMR/master/RegressionSupport.r")
source("https://raw.github.com/PirateGrunt/MRMR/master/NAIC.R")
source("https://raw.github.com/PirateGrunt/MRMR/master/ReservingVisualization.R")
source("https://raw.github.com/PirateGrunt/MRMR/master/Triangle.R")

df = GetNAICData("wkcomp_pos.csv")
bigCompany = as.character(df[which(df$CumulativePaid == max(df$CumulativePaid)),"GroupName"])

df.BigCo = subset(df, GroupName == bigCompany)

df.UpperTriangle = subset(df.BigCo, DevelopmentYear <=1997)

tri = Triangle(TriangleData = df.UpperTriangle
               , TriangleName = bigCompany
               , LossPeriodType = "accident"
               , LossPeriodInterval = years(1)
               , DevelopmentInterval = years(1)
               , LossPeriodColumn = "LossPeriodStart"
               , DevelopmentColumn = "DevelopmentLag")

tri@TriangleName
tri

is(tri, "Triangle")
is.Triangle(tri)

plt = ShowTriangle(df.UpperTriangle, bigCompany)

plot(tri)
head(LatestDiagonal(tri))
length(LatestDiagonal(tri)[,1])

plt = ShowTriangle(df.UpperTriangle, bigCompany, Cumulative=FALSE)
#Note the apparent calendar year impact in 1996. This is invisible in the cumulative display.

# To calibrate a multiplicative chain ladder, we must omit the first development lag.
df.UpperTriangle = subset(df.UpperTriangle, DevelopmentLag >1)

fit = with( df.UpperTriangle, FitModel(IncrementalPaid, PriorCumulativePaid, DevelopmentLag, 1, 0))

devFactors = CreateDevelopmentArray(fit, 1, 10)

# To project a multiplicative chain ladder, we may only use the latest diagonal.
df.Latest = subset(df.BigCo, DevelopmentYear == 1997)

df.project = with(df.Latest, ProjectValues(CumulativePaid, DevelopmentLag, LossPeriod, devFactors, TRUE))