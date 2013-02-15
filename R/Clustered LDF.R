
dev.factors = tri@TriangleData$IncrementalPaid / tri@TriangleData$PriorCumulativePaid

factorLabels = tri@TriangleData$DevelopmentMultiplier[which(!is.na(dev.factors))]
dev.factors = dev.factors[!is.na(dev.factors)]

mojo = kmeans(dev.factors, centers = length(unique(factorLabels)))

par(mfrow=c(2,1))

plot(dev.factors, rep(1, length(dev.factors)), col = mojo$cluster, pch=19, main="Multiplicative chain ladder", xlab="Dev factor")
text(dev.factors, rep(1.1, length(dev.factors)), labels=factorLabels, col=mojo$cluster)

#==========================================================
dev.factors = tri@TriangleData$IncrementalPaid / tri@TriangleData$NetEP

factorLabels = tri@TriangleData$DevelopmentMultiplier[which(!is.na(dev.factors))]
dev.factors = dev.factors[!is.na(dev.factors)]

length(unique(factorLabels))

mojo = kmeans(dev.factors, centers = length(unique(factorLabels)))

plot(dev.factors, rep(1,length(dev.factors)), col = mojo$cluster, pch=19, main="Additive", xlab="Dev factor")
text(dev.factors, rep(1.1, length(dev.factors)), labels=factorLabels, col=mojo$cluster)
