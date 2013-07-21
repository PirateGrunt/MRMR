data(Friedland)

plot(Friedland, Predictor = "DevInteger", Response = "CumulativePaid", Friedland)
plot(Predictor = "DevInteger", Response = "IncrementalPaid", Friedland)
plot(Predictor = "EvaluationDate", Response = "CumulativePaid", Friedland)

plot(Predictor = "EvaluationDate", Response = "IncrementalPaid", Friedland)

# plot("EvaluationDate", "IncrementalPaid", Friedland, "DevInteger")
# plot("OriginPeriodStart", "IncrementalPaid", Friedland, "DevInteger")
# 
# plot(Predictor = "CumulativePaid", Response ="IncrementalPaid", Friedland, GroupColumn = "DevInteger", Lines = FALSE)
# 
# plot(x = "CumulativePaid", y="IncrementalPaid", Friedland, "DevInteger", Lines = FALSE, FitLines = TRUE)
# plot(x = "CumulativeEP", y="IncrementalPaid", Friedland, "DevInteger", Lines = FALSE)
# plot(x = "CumulativeEP", y="IncrementalPaid", Friedland, "DevInteger", Lines = FALSE, FitLines = TRUE)
# 
# 
# plot(x = "CumulativePaid", y="IncrementalPaid", Friedland, "EvaluationDate", Lines = FALSE)
# plot(x = "CumulativePaid", y="IncrementalPaid", Friedland, "DevInteger", Lines = FALSE)
# plot(x = "CumulativePaid", y="IncrementalPaid", Friedland, "EvaluationDate", Lines = FALSE)
# plot(x = "CumulativePaid", y="IncrementalPaid", Friedland, "EvaluationDate", Lines = FALSE, FitLines = TRUE)
# plot(x = "CumulativeEP", y="IncrementalPaid", Friedland, "EvaluationDate", Lines = FALSE, FitLines = TRUE)
x

EPModel = newTriangleModel(Friedland, Response = "IncrementalPaid", Predictor = "CumulativeEP", Group = "DevInteger")
summary(EPModel)

plot(myModel)
summary(myModel@Fit)

mojo = myModel@Fit$coefficients
cazart = names(mojo)
plot(myModel)

myModel2 = newTriangleModel(Friedland, "IncrementalPaid", "PriorPaid", "DevInteger")
summary(myModel2@Fit)