supportLibraries = c("RColorBrewer"
                     , "plyr"
                     , "lubridate"
                     , "ggplot2"
                     , "reshape2")

dummy = lapply(supportLibraries, library, character.only = TRUE)

# Source from a local directory
myDirectory = "~/Github/MRMR/R/"
sourceFiles = c("TriangleOriginPeriod.R"
                , "TriangleDevelopmentLag.R"
                , "TriangleEvaluationDate.R"
                , "TriangleOriginPeriod.R"
                , "TriangleAdjustMeasures.R"
                , "Triangle.R"
                , "TriangleMeta.R")

dummy = lapply(paste0(myDirectory, sourceFiles), source)

dataDirectory = "~/Github/MRMR/Data/"

load(paste0(dataDirectory, "Multiline.rda"))

rm(supportLibraries, dummy, sourceFiles, myDirectory)

measureCols = c("CumulativeIncurred", "CumulativePaid", "NetEP", "DirectEP", "CededEP", "IBNR")
MultilineTriangle = newTriangle(TriangleData = Multiline
                         , OriginPeriods = OriginPeriodStart
                         , OriginLength = years(1)
                         , StartDay = 1
                         , StartMonth = 1
                         , DevelopmentLags = DevelopmentLag
                         , DevelopmentPeriod = years(1)
                         , Groups = c("Line", "GroupName")
                         , Measures = measureCols
                         , Cumulative = TRUE)

load(paste0(dataDirectory, "NAIC.rda"))
measureCols = c("CumulativeIncurred", "CumulativePaid", "NetEP", "DirectEP", "CededEP", "IBNR")
NAIC = NAIC[1:100,]
AutoTriangle = newTriangle(TriangleData = NAIC
                         , OriginPeriods = OriginPeriodStart
                         , OriginLength = years(1)
                         , StartDay = 1
                         , StartMonth = 1
                         , DevelopmentLags = DevelopmentLag
                         , DevelopmentPeriod = years(1)
                         , Groups = "GroupName"
                         , Measures = measureCols
                         , Cumulative = TRUE)