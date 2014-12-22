library(MRMR)

URL.stem = "http://www.casact.org/research/reserve_data/"

URL = paste0(URL.stem, "ppauto_pos.csv")
dfPPA = read.csv(URL, stringsAsFactors = FALSE)

URL = paste0(URL.stem, "wkcomp_pos.csv")
dfWC = read.csv(URL, stringsAsFactors=FALSE)

NewColnames = c("GroupCode"
             , "Company"
             , "AccidentYear"
             , "DevelopmentYear"
             , "Lag"
             , "CumulativeIncurred"
             , "CumulativePaid"
             , "IBNR"
             , "DirectEP"
             , "CededEP"
             , "NetEP"
             , "Single"
             , "Reserve1997")

colnames(dfPPA) = NewColnames
colnames(dfWC) = NewColnames
rm(URL.stem, URL, NewColnames)

# Fetch the top 10 insurers
TopPPA = aggregate(dfPPA$NetEP, by=list(dfPPA$Company), sum)
TopPPA = TopPPA[order(TopPPA[,2], decreasing=TRUE), ]
TopPPA = TopPPA[1:20, 1]

TopWC = aggregate(dfWC$NetEP, by=list(dfWC$Company), sum)
TopWC = TopWC[order(TopWC[,2], decreasing=TRUE), ]
TopWC = TopWC[1:20, 1]

companies = intersect(TopWC, TopPPA)
companies = companies[companies != "State Farm Mut Grp"]

rm(TopPPA, TopWC)
dfPPA = subset(dfPPA, Company %in% companies)
dfPPA$Line = "Personal Auto"

dfWC = subset(dfWC, Company %in% companies)
dfWC$Line = "Workers Comp"

# Form the OriginPeriod
ay = unique(dfWC$AccidentYear)
op = OriginPeriod(StartDate = as.Date(paste0(min(ay),"-01-01"))
                  , NumPeriods = length(ay)
                  , Period=as.period(1, "year")
                  , Type="Accident Year"
                  , Moniker = paste("AY", ay))

rm(ay)

save(dfPPA, dfWC, file = "~/Documents/Projects/MRMR/data/NAIC.rda", compress = "xz")

# load("./data/NAIC.rda")
# 
# # Form a StaticMeasure
# smPPA = StaticMeasure(OriginPeriod = op
#                    , Level=list(Company=companies, Line="PPA")
#                    , Measure=c("DirectEP", "NetEP")
#                    , Data=dfSubPPA[dfSubPPA$Lag == 1, ])
# 
# smWC = StaticMeasure(OriginPeriod = op
#                       , Level=list(Company=companies, Line="WC")
#                       , Measure=c("DirectEP", "NetEP")
#                       , Data=dfSubWC[dfSubWC$Lag == 1, ])
# 
# smMulti = c(smPPA, smWC)
# save(smPPA, smWC, smMulti, file="./data/smMulti.rda")
# load("./data/smMulti.rda")
# 
# # Form a StochasticMeasure
# scmWC = StochasticMeasure(OriginPeriod = op
#                         , Level=list(Company=companies
#                                      , Line="WC")
#                         , Measure = c("CumulativeIncurred"
#                                       , "CumulativePaid")
#                         , DevPeriod = as.period(1, "year")
#                         , Lags=1:10
#                         , Data=dfSubWC
#                         , OriginPeriodSort = "AccidentYear"
#                         , EvaluationDates=seq.Date(
#                           as.Date("1988-12-31")
#                           , as.Date("2006-12-31"), by="1 year"))
# 
# scmPPA = StochasticMeasure(OriginPeriod = op
#                            , Level=list(Company=companies, Line="PPA")
#                            , Measure = c("CumulativeIncurred", "CumulativePaid")
#                            , DevPeriod = as.period(1, "year")
#                            , Data=dfSubPPA
#                            , Lags=1:10
#                            , OriginPeriodSort = "AccidentYear"
#                            , FirstEvaluationDate=as.Date("1988-12-31")
#                            , LastEvaluationDate=as.Date("2006-12-31"))
# 
# scmMulti = c(scmWC, scmPPA)
# scmMulti = rbind(scmWC, scmPPA)
# 
# save(scmWC, scmPPA, scmMulti, file="./data/scmMulti.rda")
# load("./data/scmMulti.rda")
# 
# # Form a triangle
# triWC = Triangle(smWC, scmWC, "Workers Comp Triangle")
# triPPA = Triangle(smPPA, scmPPA, "Personal Auto Triangle")
# triMulti = c(triWC, triPPA)
# triMulti = Triangle(smMulti, scmMulti, "Multi-line triangle")
# 
# save(triWC, triPPA, triMulti, file="./data/triMulti.rda")
# load("./data/triMulti.rda")
