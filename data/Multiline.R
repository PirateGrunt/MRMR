# This script will fetch several sets of data from the CAS website and bind them together
# using multiple group IDs

# Source from a local directory
myDirectory = "~/Github/MRMR/R/"
sourceFiles = c("NAIC.R")

dummy = lapply(paste0(myDirectory, sourceFiles), source)
rm(myDirectory, sourceFiles, dummy)

dfAuto = GetNAICData(dataSetName = "comAuto_pos.csv")
dfWC =  GetNAICData(dataSetName = "wkcomp_pos.csv")
dfGL =  GetNAICData(dataSetName = "othliab_pos.csv")
dfProd =  GetNAICData(dataSetName = "prodliab_pos.csv")

groups = intersect(unique(dfAuto$GroupName)
                   , unique(dfWC$GroupName)
                   , unique(dfGL$GroupName)
                   , unique(dfProd$GroupName))

groups = intersect(unique(dfAuto$GroupName), unique(dfWC$GroupName))
groups = intersect(groups, unique(dfGL$GroupName))
groups = intersect(groups, unique(dfProd$GroupName))

dfAuto = subset(dfAuto, GroupName %in% groups)
dfWC = subset(dfWC, GroupName %in% groups)
dfGL = subset(dfGL, GroupName %in% groups)
dfProd = subset(dfProd, GroupName %in% groups)

dfAuto$Line = "Auto"
dfWC$Line = "WC"
dfGL$Line = "GL"
dfProd$Line = "Products"

Multiline = rbind(dfAuto, dfWC, dfGL, dfProd)

rm(dfAuto, dfWC, dfGL, dfProd, groups)

save(Multiline, file = "~/Github/MRMR/Data/Multiline.rda")