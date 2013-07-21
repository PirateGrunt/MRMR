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
                , "TriangleMeta.R"
                , "PlotTriangle.R")

sourceDirectory = "https://raw.github.com/PirateGrunt/MRMR/master/R/"

invisible(lapply(paste0(sourceDirectory, sourceFiles), source))

library(RCurl)
dataURL = "https://github.com/PirateGrunt/MRMR/blob/master/data/"
dataFiles = c("NAIC.rda", "Multiline.rda", "Friedland.rda")
dataURLStem = "?raw=true"

LoadDataFromGitHub = function(url)
{
  sit = getURLContent(url, binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
  con = gzcon(rawConnection(sit, 'rb'))
  eval(load(con), envir=globalenv())
  1
#  close(con)
}

# LoadDataFromGitHub(mojo[3])
# 
# lapply(paste0(dataURL, dataFiles, dataURLStem), LoadDataFromGitHub)
# 
mojo = paste0(dataURL, dataFiles, dataURLStem)

url = mojo[1]
sit = getURLContent(url, binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
con = gzcon(rawConnection(sit, 'rb'))
load(con)

url = mojo[2]
sit = getURLContent(url, binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
con = gzcon(rawConnection(sit, 'rb'))
load(con)

url = mojo[3]
sit = getURLContent(url, binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
con = gzcon(rawConnection(sit, 'rb'))
load(con)

close(con)

rm(mojo, con, url, sit)

rm(supportLibraries, sourceFiles, sourceDirectory, dataFiles, dataURL, dataURLStem, LoadDataFromGitHub)