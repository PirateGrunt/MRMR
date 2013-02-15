# Demo script

#=============================
# Source the necessary code
# source("./RegressionSupport.r")
# source("./NAIC.R")
# source("./ReservingVisualization.R")
# source("./Triangle.R")
# source("./TriangleModel.R")
# source("./TriangleProjection.R")

#=============================
# Get some data from the big NAIC database 
# and get a triangle we can project
data(NAIC)
# load(file="~/GitHub/MRMR/Data/NAIC.rda")
bigCompany = as.character(NAIC[which(NAIC$CumulativePaid == max(NAIC$CumulativePaid)),"GroupName"])

df.BigCo = subset(NAIC, GroupName == bigCompany)
df.UpperTriangle = subset(df.BigCo, DevelopmentYear <=1997)

MeasureName = colnames(df.UpperTriangle)[5:10]
MeasureMeta = data.frame(MeasureName = as.character(MeasureName), Cumulative = as.character(c(rep("Cumulative", 2), rep("Neither", 4))))
rm(MeasureName)

#=============================
# Construct the triangle and display 
# some basic properties
tri = Triangle(TriangleData = df.UpperTriangle
               , TriangleName = bigCompany
               , OriginPeriodColumn = "OriginPeriodStart"
               , OriginPeriodType = "accident"
               , OriginPeriodInterval = years(1)
               , DevelopmentColumn = "DevelopmentLag"
               , DevelopmentInterval = years(1)
               , MeasureMeta)

df = tri@TriangleData

names(tri@TriangleData)

unique(tri@TriangleData$DevelopmentMultiplier)

rm(bigCompany)
tri@TriangleName
tri
 
is(tri, "Triangle")
is.Triangle(tri)
 
plt = ShowTriangle(tri@TriangleData, tri@TriangleName)
 
plot(tri)

# Two methods to set the name of the Triangle object
setName(tri) = "AnotherName"
tri@TriangleName
setName(tri) = bigCompany
tri@TriangleName
tri@TriangleName = "Another name"
setName(tri) = bigCompany

#===========================
# Now let's fit a model

chainLadder = TriangleModel("CumulPaid"
                           , BaseTriangle = tri
                           , ResponseName = "IncrementalPaid"
                           , PredictorName = "PriorCumulativePaid"
                           , CategoryName = "DevelopmentMultiplier"
                           , MinimumCategoryFrequency = 1
                           , delta = 0)
summary(chainLadder@LinearFit)

Additive = TriangleModel("AdditivePaid"
                            , BaseTriangle = tri
                            , ResponseName = "IncrementalPaid"
                            , PredictorName = "NetEP"
                            , CategoryName = "DevelopmentMultiplier"
                            , MinimumCategoryFrequency = 1
                            , delta = 0)
summary(Additive@LinearFit)


#df.LowerTriangle = subset(df.BigCo, DevelopmentYear > 1997)

# df.LowerTriangle$Cat = as.factor(df.LowerTriangle$DevelopmentLag)
# df.LowerTriangle$prediction = predict.lm(fit, newdata = df.LowerTriangle)
# df.LowerTriangle$Error = with(df.LowerTriangle, prediction - CumulativePaid)
# 
# # To calibrate a multiplicative chain ladder, we must omit the first development lag.
# df.UpperTriangle = subset(df.UpperTriangle, DevelopmentLag >1)
# 
# # To project a multiplicative chain ladder, we may only use the latest diagonal.
# df.Latest = subset(df.BigCo, DevelopmentYear == 1997)
# 
# df.project = with(df.Latest, ProjectValues(CumulativePaid, DevelopmentLag, LossPeriod, devFactors, TRUE))