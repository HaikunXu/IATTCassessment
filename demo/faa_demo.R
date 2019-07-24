library(IATTCassessment)
# library(r4ss)

Save_Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/SAC10/Plot Code/"

Path <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/External Review/Recruit/Base - SAC9/"
# myreplist = SS_output(dir=Path,ncols=400,covar=F)

faa(Last_Year = 2017, Save_Dir = Save_Dir, Dir=Path, Species = "BET")
