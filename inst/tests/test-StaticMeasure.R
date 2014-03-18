startDates = seq(as.Date("2001/01/01"), as.Date("2010/12/31"), by="1 year")
moniker = paste0("AY ", as.character(year(startDates)))
type = "Accident"

op = OriginPeriod(startDates, as.period(1, "years"), Moniker=moniker, Type=type)

EarnedPremium = seq(from=10000, to=19000, by=1000)
IncurredLoss = EarnedPremium * 0.75

x = new("StaticMeasure", OriginPeriod = op, Measure = data.frame(EarnedPremium, IncurredLoss), Dimension=data.frame(Line = "GL", ClassGroup = "PremOps", State = "CA"))

EarnedPremium.TX = EarnedPremium * .56
IncurredLoss.TX = IncurredLoss * .85

df = data.frame(EarnedPremium, IncurredLoss)
df = rbind(df, data.frame(EarnedPremium = EarnedPremium.TX, IncurredLoss = IncurredLoss.TX))

x = new("StaticMeasure", OriginPeriod = op
        , Measure = df
        , Dimension=data.frame(Line = rep("GL", 2), ClassGroup = rep("PremOps", 2), State = c("CA", "TX")))

df = as.data.frame(x)
