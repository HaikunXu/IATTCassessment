library(IATTCassessment)
library(r4ss)

Save_Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/SAC10/Plot Code/"

Path <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/stock assessment/BET Assessment/SS_work/BET_SAC9/Final Models/BET_base/"
myreplist = SS_output(dir=Path,ncols=400,covar=F)

faa(Last_Year = 2017, Save_Dir = Save_Dir, myreplist = myreplist, Species = "BET")
