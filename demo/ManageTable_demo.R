library(IATTCassessment)

Path <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/R-1/"
Dynamic_Path <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/Impact Plot/R-1/noF/"

makeManagTable(Path, FFleets = 1:23, dynamicS0 = TRUE, Dynamic_Path = Dynamic_Path)
