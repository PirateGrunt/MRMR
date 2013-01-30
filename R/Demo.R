# Demo script

#=============================
# Source the necessary code
source("https://raw.github.com/PirateGrunt/MRMR/master/RegressionSupport.r")
source("https://raw.github.com/PirateGrunt/MRMR/master/NAIC.R")
source("https://raw.github.com/PirateGrunt/MRMR/master/ReservingVisualization.R")
source("https://raw.github.com/PirateGrunt/MRMR/master/Triangle.R")
source("https://raw.github.com/PirateGrunt/MRMR/master/TriangleModel.R")
source("https://raw.github.com/PirateGrunt/MRMR/master/TriangleProjection.R")

#=============================
# Get some data from the big NAIC database 
# and get a triangle we can project
df = GetNAICData("wkcomp_pos.csv")
bigCompany = as.character(df[which(df$CumulativePaid == max(df$CumulativePaid)),"GroupName"])

df.BigCo = subset(df, GroupName == bigCompany)

df.UpperTriangle = subset(df.BigCo, DevelopmentYear <=1997)
df.LowerTriangle = subset(df.BigCo, DevelopmentYear > 1997)

#=============================
# Construct the triangle and display 
# some basic properties
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

plt = ShowTriangle(tri@TriangleData, bigCompany)

plot(tri)
head(LatestDiagonal(tri))
length(LatestDiagonal(tri)[,1])

plt = ShowTriangle(tri@TriangleData, bigCompany, Cumulative=FALSE)
#Note the apparent calendar year impact in 1996. This is invisible in the cumulative display.

setName(tri) = "AnotherName"
tri@TriangleName
setName(tri) = bigCompany
tri@TriangleName
tri@TriangleName = "Another name"

#===========================
# Now let's fit a of model

chainLadder = TriangleModel("CumulPaid"
                            , BaseTriangle = tri
                            , ResponseName = "CumulativePaid"
                            , PredictorName = "DirectEP"
                            , CategoryName = "DevelopmentLag"
                            , MinimumCategoryFrequency = 1
                            , delta = 0)
summary(chainLadder@LinearFit)



df.LowerTriangle$Cat = as.factor(df.LowerTriangle$DevelopmentLag)
df.LowerTriangle$prediction = predict.lm(fit, newdata = df.LowerTriangle)
df.LowerTriangle$Error = with(df.LowerTriangle, prediction - CumulativePaid)

# To calibrate a multiplicative chain ladder, we must omit the first development lag.
df.UpperTriangle = subset(df.UpperTriangle, DevelopmentLag >1)

# To project a multiplicative chain ladder, we may only use the latest diagonal.
df.Latest = subset(df.BigCo, DevelopmentYear == 1997)

df.project = with(df.Latest, ProjectValues(CumulativePaid, DevelopmentLag, LossPeriod, devFactors, TRUE))