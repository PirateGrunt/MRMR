data(NAIC)
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

par(mfrow=c(2,1))
# Incremental paid ~ Prior cumulative
dev.factors = tri@TriangleData$IncrementalPaid / tri@TriangleData$PriorCumulativePaid

factorLabels = tri@TriangleData$DevelopmentMultiplier[which(!is.na(dev.factors))]
evalLabels = tri@TriangleData$EvaluationDate[which(!is.na(dev.factors))]
evalLabels = year(evalLabels)
dev.factors = dev.factors[!is.na(dev.factors)]

plot(dev.factors, rep(1, length(dev.factors)), col = factorLabels, pch=19, main="Incremental paid ~ Prior cumulative", xlab="Dev factor", ylab="")
text(dev.factors, rep(1.1, length(dev.factors)), labels=factorLabels, col = factorLabels)

# Clustered
set.seed(6789)
mojo = kmeans(dev.factors, centers = length(unique(factorLabels)))

plot(dev.factors, rep(1, length(dev.factors)), col = mojo$cluster, pch=19, main="Incremental paid ~ Prior cumulative, clustered", xlab="Dev factor", ylab="")
text(dev.factors, rep(1.1, length(dev.factors)), labels=factorLabels, col=mojo$cluster)

# Now add calendar period
plot(dev.factors, rep(1, length(dev.factors)), col = factorLabels, pch=19, main="Incremental paid ~ Prior cumulative", xlab="Dev factor", ylab="")
text(dev.factors, rep(1.1, length(dev.factors)), labels=factorLabels, col = factorLabels)
text(dev.factors, rep(0.9, length(dev.factors)), labels=evalLabels, col = factorLabels)

plot(dev.factors, rep(1, length(dev.factors)), col = mojo$cluster, pch=19, main="Incremental paid ~ Prior cumulative, clustered", xlab="Dev factor", ylab="")
text(dev.factors, rep(1.1, length(dev.factors)), labels=factorLabels, col=mojo$cluster)
text(dev.factors, rep(0.9, length(dev.factors)), labels=evalLabels, col=mojo$cluster)

# Focus on dev lags > 2
par(mfrow=c(1,1))
dev.subset = dev.factors[which(factorLabels > 2)]
evalLabels = evalLabels[which(factorLabels > 2)]
clusterColor = mojo$cluster[which(factorLabels > 2)]
factorLabels = factorLabels[which(factorLabels > 2)]

plot(dev.subset, rep(1, length(dev.subset)), col = clusterColor, pch=19, main="Incremental paid ~ Prior cumulative, clustered", xlab="Dev factor", ylab="")
text(dev.subset, rep(1.1, length(dev.subset)), labels=factorLabels, col=clusterColor)
text(dev.subset, rep(0.9, length(dev.subset)), labels=evalLabels, col=clusterColor)

#==========================================================
dev.factors = tri@TriangleData$CumulativePaid / tri@TriangleData$NetEP

factorLabels = tri@TriangleData$DevelopmentMultiplier[which(!is.na(dev.factors))]
dev.factors = dev.factors[!is.na(dev.factors)]

length(unique(factorLabels))

mojo = kmeans(dev.factors, centers = length(unique(factorLabels)))

plot(dev.factors, rep(1,length(dev.factors)), col = mojo$cluster, pch=19, main="Additive", xlab="Dev factor", ylab="")
text(dev.factors, rep(1.1, length(dev.factors)), labels=factorLabels, col=mojo$cluster)
