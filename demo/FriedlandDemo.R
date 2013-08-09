data(Friedland)

head(LatestDiagonal(Friedland))

plot(Friedland, Predictor = "DevInteger", Response = "CumulativePaid")
plot(Friedland, Predictor = "DevInteger", Response = "IncrementalPaid")
plot(Friedland, Predictor = "EvaluationDate", Response = "IncrementalPaid")

plot(Friedland, Predictor = "CumulativePaid", Response ="IncrementalPaid", Group = "DevInteger", Lines = FALSE)
plot(Friedland, Predictor = "CumulativePaid", Response ="IncrementalPaid", Group = "DevInteger", Lines = FALSE, FitLines = TRUE)
plot(Friedland, Predictor = "EP", Response ="IncrementalPaid", Lines = FALSE)
plot(Friedland, Predictor = "EP", Response ="IncrementalPaid", Group = "DevInteger", Lines = FALSE)
plot(Friedland, Predictor = "EP", Response ="IncrementalPaid", Group = "DevInteger", Lines = FALSE, FitLines = TRUE)

PaidAM1 = newTriangleModel(Triangle = Friedland, Response = "IncrementalPaid", Predictor = "EP", FitCategory = "DevInteger", Tail = 6)
summary(PaidAM1@Fit)
summary(PaidAM1)
PlotModelFactors(PaidAM1)

PaidAM2 = newTriangleModel(Triangle = Friedland, Response = "IncrementalPaid", Predictor = "EP", FitCategory = "DevInteger", Tail = 5)
PlotModelFactors(PaidAM2)
PlotModelGoF(PaidAM1)
PlotModelGoF(PaidAM2)

PaidCL1 = newTriangleModel(Triangle = Friedland, Response = "IncrementalPaid", Predictor = "PriorPaid", FitCategory = "DevInteger")
PlotModelFactors(PaidCL1)

PaidCL2 = newTriangleModel(Triangle = Friedland, Response = "IncrementalPaid", Predictor = "PriorPaid", FitCategory = "DevInteger", Tail = 5)
PlotModelFactors(PaidCL2)

PaidCL3 = newTriangleModel(Triangle = Friedland, Response = "IncrementalPaid", Predictor = "PriorPaid", FitCategory = "DevInteger", Tail = 4)
PlotModelFactors(PaidCL3)

PlotModelGoF(PaidCL1)
PlotModelGoF(PaidCL2)
PlotModelGoF(PaidCL3)

# ReportedAM = newTriangleModel(Friedland, Response = "IncrementalReported", Predictor = "CumulativeEP", Group = "DevInteger")
# ReportedCL = newTriangleModel(Friedland, Response = "IncrementalReported", Predictor = "PriorReported", Group = "DevInteger")

PaidAM_Projection = TriangleProjection(PaidAM1, ProjectToDev = FALSE, AsOfDate = mdy("12/31/2010"))
df = PaidAM_Projection@ProjectionData

PaidAM_Projection = TriangleProjection(PaidAM1, ProjectToDev = TRUE, MaxDev = 10)
df = PaidAM_Projection@ProjectionData

plot(PaidAM_Projection)

dfBase = Friedland@TriangleData