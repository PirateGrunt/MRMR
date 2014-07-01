library(MRMR)

URL.stem = "http://www.casact.org/research/reserve_data/"

URL = paste0(URL.stem, "ppauto_pos.csv")
dfPPA = read.csv(URL, stringsAsFactors = FALSE)

URL = paste0(URL.stem, "wkcomp_pos.csv")
dfWC = read.csv(URL, stringsAsFactors=FALSE)

NewColnames = c("GroupCode"
             , "GroupName"
             , "AccidentYear"
             , "DevelopmentYear"
             , "DevelopmentLag"
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
TenPPA = aggregate(dfPPA$NetEP, by=list(dfPPA$GroupName), sum)
TenPPA = TenPPA[order(TenPPA[,2], decreasing=TRUE), ]
TenPPA = TenPPA[1:10, 1]

TenWC = aggregate(dfWC$NetEP, by=list(dfWC$GroupName), sum)
TenWC = TenWC[order(TenWC[,2], decreasing=TRUE), ]
TenWC = TenWC[1:10, 1]

companies = intersect(TenWC, TenPPA)

rm(TenPPA, TenWC)
dfSubPPA = subset(dfPPA, GroupName %in% companies)
dfSubPPA$Line = "Personal Auto"

dfSubWC = subset(dfWC, GroupName %in% companies)
dfSubWC$Line = "Workers Comp"

# Form the OriginPeriod
ay = unique(dfWC$AccidentYear)
op = OriginPeriod(StartDate = as.Date(paste0(min(ay),"-01-01"))
                  , NumPeriods = length(ay)
                  , Period=as.period(1, "year")
                  , Type="Accident Year"
                  , Moniker = paste("AY", ay))

rm(ay)
# Form a StaticMeasure
smPPA = StaticMeasure(OriginPeriod = op
                   , Level=list(GroupName=companies, Line="PPA")
                   , Measure=c("DirectEP", "NetEP")
                   , Data=dfSubPPA[dfSubPPA$DevelopmentLag == 1, ])

smWC = StaticMeasure(OriginPeriod = op
                      , Level=list(GroupName=companies, Line="WC")
                      , Measure=c("DirectEP", "NetEP")
                      , Data=dfSubWC[dfSubWC$DevelopmentLag == 1, ])

smMulti = c(smPPA, smWC)

# Form a StochasticMeasure
scmWC = StochasticMeasure(OriginPeriod = op
                        , Level=list(GroupName=companies)
                        , Measure = c("CumulativeIncurred", "CumulativePaid")
                        , DevPeriod = as.period(1, "year")
                        , DevIntervals = 1:10
                        , Cumulative=TRUE
                        , Data=dfSubWC
                        , OriginPeriodSort = "AccidentYear"
                        , DevIntervalSort = "DevelopmentLag")

scmPPA = StochasticMeasure(OriginPeriod = op
                          , Level=list(GroupName=companies)
                          , Measure = c("CumulativeIncurred", "CumulativePaid")
                          , DevPeriod = as.period(1, "year")
                          , DevIntervals = 1:10
                          , Cumulative=TRUE
                          , Data=dfSubPPA
                          , OriginPeriodSort = "AccidentYear"
                          , DevIntervalSort = "DevelopmentLag")

scmMulti = c(scmWC, scmPPA)

# Form a triangle
triWC = Triangle(smWC, scmWC, "Workers Comp Triangle")

triMulti = Triangle(smMulti, scmMulti, "Multiline NAIC data")
