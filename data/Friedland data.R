AccidentYear = c(2002, 2002, 2002, 2002, 2002, 2002, 2002	
               , 2003, 2003, 2003, 2003, 2003, 2003
               , 2004, 2004, 2004, 2004, 2004	
               , 2005, 2005, 2005, 2005	
               , 2006, 2006, 2006	
               , 2007, 2007	
               , 2008)

Month = c(12, 24, 36, 48, 60, 72, 84	
        , 12, 24, 36, 48, 60, 72	
        , 12, 24, 36, 48, 60	
        , 12, 24, 36, 48	
        , 12, 24, 36	
        , 12, 24	
        , 12)

Reported = c(12811, 20370, 26656, 37667, 44414, 48701, 48169
           ,  9651, 16995, 30354, 40594, 44231, 44373
           , 16995,	40180, 58866, 71707, 70288	
           , 28674,	47432, 70340, 70655		
           , 27066,	46783, 48804		
           , 19477,	31732				
           , 18632)

Paid = c(2318,  7932, 13822, 22095, 31945, 40629, 44437
       , 1743,  6240, 12683, 22892, 34505, 39320	
       , 2221,  9898, 25950, 43439, 52811	
       , 3043, 12219, 27073, 40026	
       , 3531, 11778, 22819
       , 3529, 11865	
       , 3409)

EP = c( 61183,  61183,  61183,  61183, 61183, 61183, 61183
     ,  69175,  69175,  69175,  69175, 69175, 69175	
     ,  99322,  99322,  99322,  99322, 99322	
     , 138151, 138151, 138151, 138151	
     , 107578, 107578, 107578	
     ,  62438,  62438	
     ,  47797)

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