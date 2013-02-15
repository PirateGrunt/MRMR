# 
# FrequencyMean = 40
# FrequencyVariance = 60
# numTrials = 1000
# numClaims = rnorm(numTrials, mean = FrequencyMean, sd = sqrt(FrequencyVariance))
# numClaims = ifelse(numClaims<=0,1,numClaims)
# 
# #numClaims = rnbinom(numTrials, )
# 
# LossMonth = sapply(numClaims, runif, min=0, max=11)
# # cazart = do.call("rbind", mojo)
# # cazart2 = stack(mojo)
# ReportLag = sapply(numClaims, rexp, rate= 1/18)
# PaymentLag = sapply(numClaims, rexp, rate = 1/ 12)
# UntrendedClaim = sapply(numClaims, rlnorm, mean = 10400, sdlog = 34800)
# #ReserveError = sapply(numClaims)
# 
# PaymentMonth = LossMonth + ReportLag
# PaymentMonth = vector("list", 1000)
# for (iTrial in 1:numTrials)
# {
#   PaymentMonth[[iTrial]] = LossMonth[[iTrial]] + ReportLag[[iTrial]] + PaymentLag[[iTrial]]
# }
# 
# iTrial = 1:2
# PaymentMonth[[iTrial]] = LossMonth[[iTrial]] + ReportLag[[iTrial]] + PaymentLag[[iTrial]]
# 
# PaymentMonth[[iTrial]]
# 
# x = PaymentMonth[[1]]