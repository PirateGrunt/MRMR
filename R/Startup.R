supportLibraries = c("RColorBrewer"
                     , "plyr"
                     , "lubridate"
                     , "ggplot2"
                     , "reshape2")

#install.packages(supportLibraries)

invisible(lapply(supportLibraries, library, character.only = TRUE))

sourceFiles = c("TriangleOriginPeriod.R"
                , "TriangleDevelopmentLag.R"
                , "TriangleEvaluationDate.R"
                , "TriangleOriginPeriod.R"
                , "TriangleAdjustMeasures.R"
                , "Triangle.R"
                , "TriangleMeta.R")

sourceDirectory = "https://raw.github.com/PirateGrunt/MRMR/master/R/"

invisible(lapply(paste0(sourceDirectory, sourceFiles), source))

dataDirectory = "https://raw.github.com/PirateGrunt/MRMR/master/Data/"
dataFiles = c("NAIC.rda", "Multiline.rda", "Friedland.rda")
invisible(lapply(paste0(dataDirectory, dataFiles), load))


rm(supportLibraries, sourceFiles, sourceDirectory, dataFiles, dataDirectory)