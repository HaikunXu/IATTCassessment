library(IATTCassessment)

Path <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/External Review/Recruit/Base - SAC9/"
replist = r4ss::SS_output(dir=Path,ncols=400,covar=F)

makeManagTable(replist, Path)