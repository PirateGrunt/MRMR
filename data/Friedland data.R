AccidentYear = c(2002, 2003, 2004, 2005, 2006, 2007, 2008	
                 , 2002, 2003, 2004, 2005, 2006, 2007	
                 , 2002, 2003, 2004, 2005, 2006	
                 , 2002, 2003, 2004, 2005	
                 , 2002, 2003, 2004	
                 , 2002, 2003	
                 , 2002)
Month = c(12, 12, 12, 12, 12, 12, 12	
          , 24, 24, 24, 24, 24, 24	
          , 36, 36, 36, 36, 36	
          , 48, 48, 48, 48	
          , 60, 60, 60	
          , 72, 72	
          , 84)
Reported = c(12811, 9651, 16995, 28674, 27066, 19477, 18632	
             , 20370, 16995, 40180, 47432, 46783, 31732
             , 26656, 30354, 58866, 70340, 48804	
             , 37667, 40594, 71707, 70655	
             , 44414, 44231, 70288	
             , 48701, 44373	
             , 48169)
Paid = c(2318, 1743, 2221, 3043, 3531, 3529, 3409	
         , 7932, 6240, 9898, 12219, 11778, 11865	
         , 13822, 12683, 25950, 27073, 22819	
         , 22095, 22892, 43439, 40026	
         , 31945, 34505, 52811	
         , 40629, 39320	
         , 44437)
EP = c(61183, 69175, 99322, 138151, 107578, 62438, 47797	
       , 61183, 69175, 99322, 138151, 107578, 62438	
       , 61183, 69175, 99322, 138151, 107578	
       , 61183, 69175, 99322, 138151	
       , 61183, 69175, 99322	
       , 61183, 69175	
       , 61183)

df = data.frame(AccidentYear = AccidentYear
                , Month = Month
                , Reported = Reported
                , Paid = Paid
                , EP = EP)

Friedland = newTriangle(OriginPeriods = AccidentYear
                         , TriangleData = df
                         , DevelopmentLags = Month
                         , Cumulative = TRUE
                         , StochasticMeasures = c("Reported", "Paid")
                         , StaticMeasures = c("EP"))

View(Friedland@TriangleData)
rm(AccidentYear, Month, Reported, Paid, EP, df)

save(Friedland, file = "~/Github/MRMR/Data/Friedland.rda")