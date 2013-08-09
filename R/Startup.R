local = TRUE
localRoot = "C:/Users/bfannin/Documents/GitHub/MRMR/"
installPackages = FALSE

sourceDirectory = ifelse(local, paste0(localRoot, "R/"), "https://raw.github.com/PirateGrunt/MRMR/master/R/")

supportLibraries = c("RColorBrewer"
                     , "plyr"
                     , "lubridate"
                     , "ggplot2"
                     , "reshape2")

if (installPackages) invisible(lapply(supportaLibraries, install.packages))

invisible(lapply(supportLibraries, library, character.only = TRUE))

sourceFiles = c("TriangleOriginPeriod.R"
                , "TriangleDevelopmentLag.R"
                , "TriangleEvaluationDate.R"
                , "TriangleOriginPeriod.R"
                , "TriangleAdjustMeasures.R"
                , "Triangle.R"
                , "TriangleMeta.R"
                , "PlotTriangle.R"
                , "TriangleModel.R"
                , "PlotTriangleModel.R"
                , "PlotModelFactors.R"
                , "PlotModelGoF.R"
                , "LatestDiagonal.R"
                , "ProjectToDev.R"
                , "ProjectToDate.R"
                , "ProjectToDev.R"
                , "ProjectValues.R"
                , "TriangleProjection.R")

invisible(lapply(paste0(sourceDirectory, sourceFiles), source))

dataFiles = c("NAIC.rda", "Multiline.rda", "Friedland.rda")
if (local) {
  invisible(lapply(paste0(localRoot, "/Data/",dataFiles), load))
} else {
  print("remote github install not supported")
}

rm(supportLibraries, sourceFiles, sourceDirectory, dataFiles, local, localRoot, installPackages)

# library(RCurl)
# dataURL = "https://github.com/PirateGrunt/MRMR/blob/master/data/"
# 
# dataURLStem = "?raw=true"
# 
# LoadDataFromGitHub = function(url)
# {
#   sit = getURLContent(url, binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
#   con = gzcon(rawConnection(sit, 'rb'))
#   eval(load(con), envir=globalenv())
#   1
# #  close(con)
# }
# 
# # LoadDataFromGitHub(mojo[3])
# # 
# # lapply(paste0(dataURL, dataFiles, dataURLStem), LoadDataFromGitHub)
# # 
# mojo = paste0(dataURL, dataFiles, dataURLStem)
# 
# url = mojo[1]
# sit = getURLContent(url, binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
# con = gzcon(rawConnection(sit, 'rb'))
# load(con)
# 
# url = mojo[2]
# sit = getURLContent(url, binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
# con = gzcon(rawConnection(sit, 'rb'))
# load(con)
# 
# url = mojo[3]
# sit = getURLContent(url, binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
# con = gzcon(rawConnection(sit, 'rb'))
# load(con)
# 
# close(con)
# 
# rm(mojo, con, url, sit, dataURL, dataURLStem, LoadDataFromGitHub)